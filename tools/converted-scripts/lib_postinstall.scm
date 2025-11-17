#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; lib/postinstall.scm - Shared functions for post-installation customization scripts
;;; These functions run AFTER the system boots (not on the ISO)

(use-modules (ice-9 format)
             (srfi srfi-1))

;;; Configuration defaults

(define default-guix-url "https://git.savannah.gnu.org/git/guix.git")
(define default-nonguix-url "https://gitlab.com/nonguix/nonguix.git")

;;; Channel generation functions

;;; Generate channels.scm with configured mirrors
;;; Returns a string containing the channels configuration
;;; 
;;; Parameters:
;;;   guix-url: URL for the Guix channel (optional)
;;;   nonguix-url: URL for the Nonguix channel (optional)
;;;
;;; Usage: (generate-channels-scm) => string
;;;        (generate-channels-scm #:guix-url "https://mirror.example.com/guix.git")
(define* (generate-channels-scm #:key 
                                (guix-url (or (getenv "GUIX_GIT_URL") 
                                             default-guix-url))
                                (nonguix-url (or (getenv "NONGUIX_GIT_URL") 
                                                default-nonguix-url)))
  "Generate a Guix channels configuration with the specified mirror URLs.
Reads GUIX_GIT_URL and NONGUIX_GIT_URL from environment if not explicitly provided."
  (format #f ";; Guix channels configuration
;; Generated with regional mirror optimization

(list (channel
        (name 'guix)
        (url \"~a\")
        (branch \"master\"))
      (channel
        (name 'nonguix)
        (url \"~a\")
        (branch \"master\")))
"
          guix-url
          nonguix-url))

;;; Main entry point

;;; When run as a script, provide usage information
(define (main args)
  (cond
   ;; If called with --help or -h, show usage
   ((or (member "--help" args)
        (member "-h" args))
    (display "Usage: guile postinstall.scm [OPTION]
Post-installation utility functions for Guix system customization.

Options:
  --help, -h              Show this help message
  --generate-channels     Generate channels.scm content to stdout

Environment Variables:
  GUIX_GIT_URL           Override default Guix channel URL
  NONGUIX_GIT_URL        Override default Nonguix channel URL

Examples:
  # Generate channels.scm
  guile postinstall.scm --generate-channels > ~/.config/guix/channels.scm

  # Use with custom mirror
  GUIX_GIT_URL=https://mirror.example.com/guix.git \\
    guile postinstall.scm --generate-channels

  # Use as library in other scripts
  (load \"postinstall.scm\")
  (display (generate-channels-scm))
"))
   
   ;; If called with --generate-channels, output channels.scm
   ((member "--generate-channels" args)
    (display (generate-channels-scm)))
   
   ;; Otherwise, show brief usage
   (else
    (display "Guix post-installation utility library.
Run with --help for usage information.
This script is typically loaded as a library by other scripts.
"))))

;;; Execute main if run as script (not loaded as library)
(when (batch-mode?)
  (main (command-line)))
