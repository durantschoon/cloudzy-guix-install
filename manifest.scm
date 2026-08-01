;;; Development toolchain for this repository.
;;;
;;; Provides the Go toolchain without installing it into a user profile or the
;;; host distribution:
;;;
;;;   guix shell -m manifest.scm -- ./run-tests.sh
;;;   guix shell -m manifest.scm -- lib/validate-before-deploy.sh --verbose
;;;
;;; The `-m manifest.scm` is required and cannot be shortened to a bare
;;; `guix shell --`.  Automatic loading of manifest.scm is disabled whenever a
;;; command is given: guix/scripts/shell.scm defines
;;;
;;;   (define interactive? (not (assoc-ref opts 'exec)))
;;;
;;; and skips auto-detection unless interactive?.  Omitting -m therefore
;;; yields an EMPTY environment with only a mild warning, which looks like the
;;; toolchain is missing rather than like a usage error.  Auto-loading also
;;; requires the directory to be listed in
;;; ~/.config/guix/shell-authorized-directories.
;;;
;;; go.mod declares `go 1.22`, which is a floor rather than a pin, so the
;;; current Guix `go` (1.26) satisfies it.  Pin with "go@1.22" here if a
;;; future toolchain ever breaks the build.
;;;
;;; gcc-toolchain is included because Go enables cgo by default; without a C
;;; compiler on PATH, packages that use cgo fail to build with an error that
;;; does not obviously point at a missing gcc.

(specifications->manifest
 '("go"
   "gcc-toolchain"))
