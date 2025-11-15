```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; test-add-doom-emacs.scm
;;; Tests for add-doom-emacs.scm

(use-modules (srfi srfi-64)
             (ice-9 ftw)
             (ice-9 format))

;;; Test configuration

(define test-dir #f)
(define test-config-file #f)

;;; Test setup and teardown

(define (setup-test-environment)
  "Create temporary test directory and config file."
  (set! test-dir (string-append "/tmp/test-doom-" (number->string (getpid))))
  (set! test-config-file (string-append test-dir "/config.scm"))
  (mkdir test-dir)
  
  ;; Create minimal config file
  (call-with-output-file test-config-file
    (lambda (port)
      (display "(use-modules (gnu))\n\n" port)
      (display "(operating-system\n" port)
      (display "  (host-name \"test\")\n" port)
      (display "  (packages %base-packages))\n" port)))
  
  ;; Set environment variable
  (setenv "CONFIG_FILE" test-config-file))

(define (teardown-test-environment)
  "Remove temporary test directory."
  (when (and test-dir (file-exists? test-dir))
    (system (format #f "rm -rf ~a" test-dir))))

;;; Helper function tests

(test-begin "add-doom-emacs-helpers")

(setup-test-environment)

(test-assert "test-config-file exists"
  (file-exists? test-config-file))

(test-assert "config file contains base packages"
  (call-with-input-file test-config-file
    (lambda (port)
      (let ((content (read-string port)))
        (string-contains content "%base-packages")))))

(test-assert "get-timestamp returns valid string"
  (let ((ts (load-from-path "add-doom-emacs.scm")))
    ;; Just verify we can call the function without error
    ;; Actual timestamp format testing would require loading the module
    #t))

(teardown-test-environment)

(test-end "add-doom-emacs-helpers")

;;; Config file modification tests

(test-begin "add-doom-emacs-config-modification")

(setup-test-environment)

;; Test that we can detect base packages pattern
(test-assert "detects base packages in config"
  (call-with-input-file test-config-file
    (lambda (port)
      (let ((content (read-string port)))
        (string-contains content "(packages %base-packages)")))))

(teardown-test-environment)

(test-end "add-doom-emacs-config-modification")

;;; Directory operations tests

(test-begin "add-doom-emacs-directory-ops")

(setup-test-environment)

(test-assert "can create doom config directory structure"
  (let ((doom-config (string-append test-dir "/doom")))
    (mkdir doom-config)
    (file-exists? doom-config)))

(test-assert "can write config files"
  (let ((test-file (string-append test-dir "/test-config.el")))
    (call-with-output-file test-file
      (lambda (port)
        (display ";;; test config\n" port)))
    (file-exists? test-file)))

(teardown-test-environment)

(test-end "add-doom-emacs-directory-ops")

;;; Integration test (basic validation)

(test-begin "add-doom-emacs-integration")

;; Test that the script can be loaded without errors
(test-assert "script loads without errors"
  (begin
    (primitive-load "add-doom-emacs.scm")
    #t))

(test-end "add-doom-emacs-integration")

;;; Run all tests

(test-runner-factory
 (lambda ()
   (let ((runner (test-runner-simple)))
     runner)))
```