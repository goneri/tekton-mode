;;; tekton.el --- Helpers for Tekton/Konflux container image references -*- lexical-binding: t; -*-

;; Copyright (C) 2026 goneri

;; Author: goneri
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, tekton, konflux, containers
;; URL: https://github.com/goneri/tekton-emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities for container image references of the form
;; `name:tag@sha256:...' as found in Tekton/Konflux pipeline
;; definitions:
;;
;; - `tekton-image-info': display an overlay above the reference
;;   with the image's age and whether it is the latest for its tag.
;; - `tekton-refresh-digest': replace the pinned digest with the
;;   registry's current one.
;; - `tekton-scan-buffer': display overlays for every reference in
;;   the buffer.
;; - `tekton-image-info-mode': enable automatic behavior: overlay
;;   when the cursor rests on a reference, full scan of
;;   Konflux/Tekton YAML files on visit, and a 10-minute refresh.
;; - `tekton-image-info-status': debug helper.
;;
;; Inspections run asynchronously through `skopeo'.  For Tekton
;; bundles that carry no creation date, the build time is recovered
;; from the image's `.att' attestation.  Background chatter and
;; errors go to the `*tekton log*' buffer.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(require 'cl-lib)
(require 'json)

(defun tekton--image-reference-at-point ()
  "Return (NAME PINNED BEG END) for an image reference on the current line.

The reference under point is preferred; otherwise the first
reference found on the line is used.  This means the cursor can sit
anywhere on a `value:' line and still trigger."
  (let ((bol (line-beginning-position))
        (eol (line-end-position))
        (pt (point))
        found containing)
    (save-excursion
      (goto-char bol)
      (while (and (not containing)
                  (re-search-forward "\\([^ 	()\"'=]+\\)@\\(sha256:[0-9a-f]\\{64\\}\\)"
                                     eol t))
        (let ((b (match-beginning 0)) (e (match-end 0)))
          (unless found (setq found (list b e)))
          (when (and (<= b pt) (<= pt e))
            (setq containing (list b e))))))
    (pcase-let ((`(,b ,e) (or containing found)))
      (unless b
        (user-error "No image reference found on this line"))
      (let ((ref (buffer-substring-no-properties b e)))
        (if (string-match "^\\(.*\\)@\\(sha256:[0-9a-f]\\{64\\}\\)$" ref)
            (list (match-string 1 ref) (match-string 2 ref) b e)
          (user-error "Reference %S has no @sha256 digest part" ref))))))

(defun tekton--overlay-message (text &optional pos green)
  "Display TEXT as a persistent overlay before POS's line.
The overlay is anchored at the beginning of the line containing POS
(default: point), displayed above the line and indented to match
the line's own indentation.  GREEN non-nil uses a green face
instead of orange.  It stays until the buffer is killed or the
reference is inspected again.  Returns the overlay."
  (let* ((bol (save-excursion
                (goto-char (or pos (point)))
                (line-beginning-position)))
         (indent (save-excursion
                   (goto-char bol)
                   (current-indentation)))
         (ov (make-overlay bol bol)))
    ;; Replace any previous info overlay on this line.
    (dolist (old (overlays-in bol bol))
      (when (overlay-get old 'tekton-info)
        (delete-overlay old)))
    (overlay-put ov 'tekton-info t)
    (overlay-put ov 'before-string
                 (propertize (concat (make-string indent ?\s) text "\n")
                             'face (if green
                                      '(:foreground "green" :weight bold)
                                    '(:foreground "orange" :weight bold))))
    ov))

(defun tekton--log (format-string &rest args)
  "Append a message to the `*tekton log*' buffer.
Background chatter and errors are logged here rather than shown in
the mode-line / echo area."
  (with-current-buffer (get-buffer-create "*tekton log*")
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ")
              (apply #'format format-string args) "\n"))))

(defun tekton--start-process (program args callback)
  "Run PROGRAM with ARGS asynchronously, never blocking Emacs.
CALLBACK is called with (EXIT-CODE STDOUT STDERR) when the process
finishes."
  (let ((acc "")
        (err-buf (generate-new-buffer " *tekton-stderr*")))
    (let ((proc (make-process :name "tekton" :buffer nil
                              :command (cons program args)
                              :connection-type 'pipe :noquery t
                              :stderr err-buf)))
      (set-process-filter proc (lambda (_s out) (setq acc (concat acc out))))
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (memq (process-status p) '(exit signal))
           (let ((code (process-exit-status p))
                 (err (with-current-buffer err-buf (buffer-string))))
             (kill-buffer err-buf)
             (funcall callback code acc (string-trim err)))))))))

(defun tekton--attestation-time-in-dir (dir)
  "Return the newest finishedOn timestamp among attestations in DIR.
Konflux-built Tekton bundles have no Created field, but their
`sha256-<digest>.att' attestation tag embeds the pipeline's finish
timestamps.  Returns a time value or nil."
  (let (newest)
    (dolist (blob (ignore-errors
                    (directory-files-recursively dir "^[0-9a-f]\\{64\\}$")))
      (let ((ts (tekton--attestation--blob-time blob)))
        (when (and ts (or (null newest) (time-less-p newest ts)))
          (setq newest ts))))
    newest))

(defun tekton--attestation--blob-time (blob)
  "Return the newest finishedOn time found in DSSE attestation BLOB, or nil."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents blob)
      (let* ((envelope (json-read-from-string (buffer-string)))
             (payload (cdr (assoc 'payload envelope)))
             (type (cdr (assoc 'payloadType envelope))))
        (when (and (stringp payload) (string-match-p "in-toto" (or type "")))
          (let* ((statement (json-read-from-string (base64-decode-string payload)))
                 (tasks (cdr (assoc 'tasks
                                    (cdr (assoc 'buildConfig
                                                (cdr (assoc 'predicate statement)))))))
                 newest)
            (dolist (task (append tasks nil))
              (let ((f (cdr (assoc 'finishedOn task))))
                (when (and (stringp f)
                           (or (null newest) (time-less-p newest (date-to-time f))))
                  (setq newest (date-to-time f)))))
            newest))))))

(defun tekton--attestation-time-async (name digest callback)
  "Asynchronously fetch the attestation time for DIGEST in repo NAME.
Calls CALLBACK with a time value or nil.  Non-blocking."
  (let* ((repo (replace-regexp-in-string ":[^:/@]+$" "" name))
         (ref (format "%s:sha256-%s.att" repo (string-remove-prefix "sha256:" digest)))
         (dir (make-temp-file "tekton-att-" t))
         (buf (current-buffer)))
    (tekton--start-process
     "skopeo" (list "copy" "--quiet" (concat "docker://" ref) (concat "oci:" dir))
     (lambda (code _stdout _stderr)
       (let ((time (and (zerop code) (buffer-live-p buf)
                        (with-current-buffer buf
                          (tekton--attestation-time-in-dir dir)))))
         (ignore-errors (delete-directory dir :recursive))
         (funcall callback time))))))

(defvar tekton--overlays nil
  "Alist of (REF . MARKER) for info overlays displayed in this buffer.
MARKER points at the beginning of the line the overlay is anchored to.
Buffer-local.")
(make-variable-buffer-local 'tekton--overlays)

(defvar tekton--pending nil
  "Alist of (REF . MARKER) currently being inspected in this buffer.
Buffer-local.")
(make-variable-buffer-local 'tekton--pending)

(defun tekton--pending-remove (ref)
  "Remove REF from the pending list of the current buffer."
  (setq tekton--pending
        (cl-remove ref tekton--pending :key #'car :test #'equal)))

(defun tekton--displayed-or-pending-p (ref)
  "Return non-nil if REF is being inspected or already displayed here."
  (or (assoc ref tekton--overlays)
      (assoc ref tekton--pending)))

(defun tekton--inspect (name pinned bol)
  "Asynchronously fetch info for image NAME/PINNED.
When done, displays an overlay above BOL's line (as a marker, so it
follows edits) and records it for the periodic refresh.  Never
blocks Emacs; failures are reported in the minibuffer."
  (let* ((ref (concat name "@" pinned))
         (marker (copy-marker bol))
         (buf (current-buffer)))
    (cl-pushnew (cons ref marker) tekton--pending :key #'car :test #'equal)
    (tekton--log "Inspecting %s..." name)
    (tekton--start-process
     "skopeo" (list "inspect" "--format" "{{.Created}} {{.Digest}}"
                    (concat "docker://" name))
     (lambda (code stdout stderr)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (tekton--inspect-finish name pinned marker code stdout stderr)))))))

(defun tekton--inspect-finish (name pinned marker code stdout stderr)
  "Handle the skopeo inspect result for NAME/PINNED."
  (let* ((ref (concat name "@" pinned))
         (created (and (zerop code)
                       (string-match
                        "^\\([0-9-]+ [0-9:.]+\\([0-9]*\\) ?\\+?[0-9]* ?[A-Z]*\\) " stdout)
                       (match-string 1 stdout)))
         (digest (and (string-match "sha256:[0-9a-f]\\{64\\}" stdout)
                      (match-string 0 stdout))))
    (if (not (and created digest))
        (progn
          (tekton--pending-remove ref)
          (tekton--log "tekton: inspection of %s failed: %s"
                   name (if (and (not (string-empty-p (or stdout "")))
                                 (not (string-empty-p stderr)))
                            (concat (string-trim stdout) "; " stderr)
                          (or (and (not (string-empty-p (or stdout "")))
                                   (string-trim stdout))
                              stderr "skopeo error"))))
      (let ((created-time (date-to-time created)))
        (if (time-less-p created-time (encode-time 0 0 0 1 1 2000))
            ;; Bogus epoch-zero date: Tekton bundle, fetch the
            ;; attestation asynchronously.
            (tekton--attestation-time-async
             name digest
             (lambda (att-time)
               (when (buffer-live-p (marker-buffer marker))
                 (with-current-buffer (marker-buffer marker)
                   (tekton--display ref pinned marker
                                       (or att-time created-time) digest)))))
          (tekton--display ref pinned marker created-time digest))))))

(defun tekton--display (ref pinned marker created-time digest)
  "Display the info overlay for REF and record it."
  (let* ((name (substring ref 0 (- (length ref) (length pinned) 1)))
         (bol (marker-position marker))
         (bogus-p (time-less-p created-time (encode-time 0 0 0 1 1 2000)))
         (latest-p (string-equal-ignore-case digest pinned))
         (ov (and bol
                  (tekton--overlay-message
                   (format "Image is %s | %s"
                           (if bogus-p
                               "of UNKNOWN age (no creation date)"
                             (concat (tekton--human-age created-time) " old"))
                           (if latest-p
                               "This is the LATEST image for this tag"
                             (format "OUTDATED: registry has %s" digest)))
                   bol latest-p))))
    (tekton--pending-remove ref)
    (when ov
      (setq tekton--overlays
            (cons (cons ref marker)
                  (cl-remove ref tekton--overlays
                             :key #'car :test #'equal))))
    ov))

(defun tekton-image-info ()
  "Show the age of the image at point and whether it is the latest.

Displays an overlay above the current line with the image's age and
whether the registry's current digest for that tag matches the
pinned digest.  The fetch is asynchronous.  If the image has no
creation date (Tekton bundles), falls back to the pipeline
timestamp from its attestation."
  (interactive)
  (pcase-let ((`(,name ,pinned ,beg _end) (tekton--image-reference-at-point)))
    (tekton--inspect name pinned beg)))

(defun tekton--launch (ref marker)
  "Launch an async inspection of REF anchored at MARKER, unless
already displayed or in progress."
  (unless (tekton--displayed-or-pending-p ref)
    (when (string-match "^\\(.*\\)@\\(sha256:[0-9a-f]\\{64\\}\\)$" ref)
      (tekton--inspect (match-string 1 ref) (match-string 2 ref)
                          (marker-position marker)))))

(defun tekton-scan-buffer (&optional buffer)
  "Display info overlays for every image reference in BUFFER.

Inspections run asynchronously, one skopeo process each; Emacs
never blocks.  References already displayed or in progress are
skipped."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[^ \t()\"'=]+@sha256:[0-9a-f]\\{64\\}" nil t)
        (let ((ref (match-string-no-properties 0)))
          (tekton--launch ref (copy-marker (line-beginning-position))))))))

(defun tekton--refresh-all ()
  "Re-inspect every displayed overlay in every buffer."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((entries tekton--overlays))
          (setq tekton--overlays nil)
          (dolist (entry entries)
            (tekton--launch (car entry) (cdr entry))))))))

(defun tekton--maybe-scan-file ()
  "Scan the visited file if it is a Konflux/Tekton YAML pipeline."
  (when (and tekton-image-info-mode
             (buffer-file-name)
             (string-match-p "\\.ya?ml\\'" (buffer-file-name))
             (string-match-p "konflux\\|tekton" (buffer-file-name)))
    (tekton-scan-buffer)))


(defvar tekton--last-ref nil
  "Last image reference for which info was displayed automatically.")

(defvar tekton--idle-timer nil
  "Idle timer used to debounce automatic image info display.")

(defun tekton--idle-check ()
  "Show image info if point is idle on a line with an image reference."
  (unless (minibufferp)
    (let ((ref (save-excursion
                 (goto-char (line-beginning-position))
                 (when (re-search-forward "[^ \t()\"'=]+@sha256:[0-9a-f]\\{64\\}"
                                          (line-end-position) t)
                   (match-string-no-properties 0)))))
      (when (and ref (not (equal ref tekton--last-ref)))
        (condition-case _err
            (progn
              (tekton-image-info)
              ;; Only remember the ref on success, so a transient
              ;; failure is retried next time.
              (setq tekton--last-ref ref))
          (error
           (tekton--log "tekton: auto info failed: %s"
                    (error-message-string _err))))))))

(defun tekton-image-info-status ()
  "Report why automatic image info may not be triggering."
  (interactive)
  (let ((on-p (and (bound-and-true-p tekton-image-info-mode) t))
       (line-ref (save-excursion
                    (goto-char (line-beginning-position))
                    (when (re-search-forward "[^ \t()\"'=]+@sha256:[0-9a-f]\\{64\\}"
                                              (line-end-position) t)
                      (match-string-no-properties 0)))))
    (message
     "mode: %s | timer: %s | ref on line: %s | last ref: %s"
     (if on-p "ON" "OFF")
     (if tekton--idle-timer "running" "NONE")
     (or (and line-ref (concat (substring line-ref 0 40) "...")) "none")
     (or (and tekton--last-ref
              (concat (substring tekton--last-ref 0 40) "..."))
         "none"))))

(defvar tekton--refresh-timer nil
  "Periodic timer that refreshes all displayed overlays.")

(define-minor-mode tekton-image-info-mode
  "Automatically show image info when point rests on an image reference.

Displays the age and latest-status of a container image reference
of the form name@sha256:... after a short idle delay. The info is
only fetched once per reference until you move to a different one.

Additionally, Konflux/Tekton YAML files are fully scanned on visit
(all references get an overlay), and every displayed overlay is
re-inspected every 10 minutes."
  :global t
  :group 'tools
  (when tekton--idle-timer
    (cancel-timer tekton--idle-timer)
    (setq tekton--idle-timer nil))
  (when tekton--refresh-timer
    (cancel-timer tekton--refresh-timer)
    (setq tekton--refresh-timer nil))
  (if tekton-image-info-mode
      (progn
        (setq tekton--idle-timer
              (run-with-idle-timer 1.0 t #'tekton--idle-check))
        (setq tekton--refresh-timer
              (run-at-time 600 600 #'tekton--refresh-all))
        (add-hook 'find-file-hook #'tekton--maybe-scan-file))
    (remove-hook 'find-file-hook #'tekton--maybe-scan-file)))

(defun tekton--human-age (time)
  "Return a human-readable age string for TIME (a time value)."
  (let* ((secs (float-time (time-subtract nil time)))
         (days (/ secs 86400)))
    (cond
     ((>= days 365) (format "%.1f years" (/ days 365.0)))
     ((>= days 30) (format "%.1f months" (/ days 30.0)))
     ((>= days 1) (format "%.1f days" days))
     ((>= secs 3600) (format "%.1f hours" (/ secs 3600.0)))
     (t (format "%.0f minutes" (/ secs 60.0))))))

(defun tekton-refresh-digest ()
  "Refresh the sha256 digest of the container image reference at point.

The image reference must look like `quay.io/foo/bar:tag@sha256:...'
(possibly preceded by a `VARIABLE=' prefix).  Uses `skopeo inspect'
to fetch the current digest from the registry, replaces the old
one, and refreshes the info overlay.  Asynchronous."
  (interactive)
  (pcase-let ((`(,name ,_pinned ,beg ,end)
               (tekton--image-reference-at-point)))
    (message "Fetching digest for %s..." name)
    (let ((buf (current-buffer))
          (beg-m (copy-marker beg))
          (end-m (copy-marker end)))
      (tekton--start-process
       "skopeo" (list "inspect" "--format" "{{.Digest}}"
                      (concat "docker://" name))
       (lambda (code stdout _stderr)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let ((digest (string-trim stdout)))
               (if (not (and (zerop code)
                             (string-match-p "^sha256:[0-9a-f]\\{64\\}$" digest)))
                   (message "tekton: refresh of %s failed: %s"
                            name (or stdout "skopeo error"))
                 (save-excursion
                   (goto-char beg-m)
                   (when (re-search-forward "@sha256:[0-9a-f]\\{64\\}" end-m t)
                     (replace-match (concat "@" digest))
                     (message "Updated digest of %s to %s" name digest)
                     ;; Drop the stale overlay entry for the old ref and
                     ;; re-inspect to refresh the info overlay.
                     (setq tekton--overlays
                           (cl-remove-if
                            (lambda (e)
                              (and (marker-position (cdr e))
                                   (= (marker-position (cdr e))
                                      (marker-position beg-m))))
                            tekton--overlays))
                     (tekton--inspect name digest
                                         (marker-position beg-m)))))))))))))


(provide 'tekton)
;;; tekton.el ends here
