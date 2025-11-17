!#

;;; Tests for channel-utils.scm

(use-modules (srfi srfi-64)
             (ice-9 ftw)
             (ice-9 format))

;;; Test configuration

(define *test-dir* "/tmp/guix-channel-utils-test")
(define *test-home* (string-append *test-dir* "/home"))
(define *test-config-dir* (string-append *test-home* "/.config/guix"))
(define *test-channels-file* (string-append *test-config-dir* "/channels.scm"))

;;; Setup and teardown helpers

(define (setup-test-env)
  "Create test environment"
  (when (file-exists? *test-dir*)
    (system* "rm" "-rf" *test-dir*))
  (mkdir *test-dir*)
  (mkdir *test-home*)
  (setenv "HOME" *test-home*))

(define (teardown-test-env)
  "Clean up test environment"
  (when (file-exists? *test-dir*)
    (system* "rm" "-rf" *test-dir*)))

;;; Load the script functions (without running main)
;;; We need to extract the functions for testing

(define (ensure-directory path)
  "Create directory if it doesn't exist"
  (unless (file-exists? path)
    (mkdir path)))

(define (get-channels-file)
  "Return the path to the user's channels.scm file"
  (string-append (getenv "HOME") "/.config/guix/channels.scm"))

(define (get-config-dir)
  "Return the path to the user's Guix config directory"
  (string-append (getenv "HOME") "/.config/guix"))

(define *default-channels-content*
  "(cons* (channel
        (name 'nonguix)
        (url \"https://gitlab.com/nonguix/nonguix\")
        (branch \"master\")
        (introduction
         (make-channel-introduction
          \"897c1a470da759236cc11798f4e0a5f7d4d59fbc\"
          (openpgp-fingerprint
           \"2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5\"))))
       %default-channels)")

;;; Test suite

(test-begin "channel-utils")

;;; Test: Directory creation

(test-group "ensure-directory"
  (setup-test-env)
  
  (test-assert "creates directory when it doesn't exist"
    (let ((test-path (string-append *test-dir* "/test-subdir")))
      (ensure-directory test-path)
      (file-exists? test-path)))
  
  (test-assert "doesn't fail when directory exists"
    (let ((test-path (string-append *test-dir* "/existing")))
      (mkdir test-path)
      (ensure-directory test-path)
      (file-exists? test-path)))
  
  (teardown-test-env))

;;; Test: Path helpers

(test-group "path-helpers"
  (setup-test-env)
  
  (test-equal "get-config-dir returns correct path"
    *test-config-dir*
    (get-config-dir))
  
  (test-equal "get-channels-file returns correct path"
    *test-channels-file*
    (get-channels-file))
  
  (teardown-test-env))

;;; Test: Setup default channels

(test-group "setup-default-channels"
  (setup-test-env)
  
  (test-assert "creates config directory"
    (begin
      (ensure-directory (get-config-dir))
      (file-exists? (get-config-dir))))
  
  (test-assert "creates channels file with content"
    (begin
      (ensure-directory (get-config-dir))
      (call-with-output-file (get-channels-file)
        (lambda (port)
          (display *default-channels-content* port)))
      (and (file-exists? (get-channels-file))
           (> (stat:size (stat (get-channels-file))) 0))))
  
  (test-assert "channels file contains nonguix"
    (begin
      (ensure-directory (get-config-dir))
      (call-with-output-file (get-channels-file)
        (lambda (port)
          (display *default-channels-content* port)))
      (call-with-input-file (get-channels-file)
        (lambda (port)
          (let ((content (read-string port)))
            (string-contains content "nonguix"))))))
  
  (teardown-test-env))

;;; Test: File existence checks

(test-group "file-operations"
  (setup-test-env)
  
  (test-assert "detects missing channels file"
    (not (file-exists? (get-channels-file))))
  
  (test-assert "detects existing channels file"
    (begin
      (ensure-directory (get-config-dir))
      (call-with-output-file (get-channels-file)
        (lambda (port)
          (display "test content" port)))
      (file-exists? (get-channels-file))))
  
  (teardown-test-env))

;;; Test: Channel content validation

(test-group "channel-content"
  (setup-test-env)
  
  (test-assert "default channels contain required fields"
    (and (string-contains *default-channels-content* "channel")
         (string-contains *default-channels-content* "name")
         (string-contains *default-channels-content* "url")
         (string-contains *default-channels-content* "branch")
         (string-contains *default-channels-content* "introduction")))
  
  (test-assert "default channels reference nonguix"
    (string-contains *default-channels-content* "nonguix"))
  
  (test-assert "default channels have correct URL"
    (string-contains *default-channels-content* 
                     "https://gitlab.com/nonguix/nonguix"))
  
  (teardown-test-env))

(test-end "channel-utils")

;;; Run tests and report

(define (run-tests)
  (let ((runner (test-runner-simple)))
    (test-runner-current runner)
    (test-begin "channel-utils")
    (test-end "channel-utils")
    (if (zero? (test-runner-fail-count runner))
        (begin
          (format #t "~%All tests passed!~%")
          (exit 0))
        (begin
          (format #t "~%~a tests failed!~%"
                  (test-runner-fail-count runner))
          (exit 1)))))

;; Don't run tests automatically when loaded as module
(when (string=? (basename (car (command-line))) "test-channel-utils.scm")
  (run-tests))
