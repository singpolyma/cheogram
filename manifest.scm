(define cheogram (load "./guix.scm"))

(concatenate-manifests
  (list
    (specifications->manifest
      '("hlint"))
    (package->development-manifest cheogram)))
