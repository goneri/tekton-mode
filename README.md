# tekton-emacs

Emacs helpers for Tekton/Konflux container image references
(`name:tag@sha256:...`).

## Features

- `M-x tekton-image-info` — display an overlay above the image
  reference at point with its age and whether it is the latest
  image for its tag (green when up to date, orange when outdated).
- `M-x tekton-refresh-digest` — replace the pinned `@sha256:`
  digest with the registry's current one.
- `M-x tekton-scan-buffer` — display overlays for every image
  reference in the buffer.
- `M-x tekton-image-info-mode` — enable the automatic behavior:
  - overlay when the cursor rests on a reference line
  - full scan of `.yaml`/`.yml` files whose path contains
    `konflux` or `tekton`, on visit
  - refresh of all displayed overlays every 10 minutes
- `M-x tekton-image-info-status` — debug helper.

All `skopeo` calls are asynchronous; Emacs never blocks. Tekton
bundles carry no creation date, so the build time is recovered from
the image's `.att` attestation. Background chatter and errors go to
the `*tekton log*` buffer.

## Requirements

- Emacs 27.1+
- [skopeo](https://github.com/containers/skopeo) in `$PATH`

## Installation

```elisp
(add-to-list 'load-path "~/git_repos/tekton-emacs")
(require 'tekton)
(tekton-image-info-mode 1)
```
