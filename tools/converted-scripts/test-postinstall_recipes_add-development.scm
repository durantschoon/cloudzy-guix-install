!#

;;; Test suite for add-development.scm

(use-modules (srfi srfi-64)  ; Testing framework
             (ice-9 ftw)
             (ice-9 match)
             (ice-9 format))

;;; Load the script being tested
;;; Note: We need to load functions without executing main
(primitive-load "add-development.scm")

;;; Test configuration

(define test-dir "/tmp/add-development-test")
(define test-config (string-append test-dir "/config.scm"))

;;; Helper functions for tests

(define (setup-test-env)
  "Create temporary test directory."
  (when (file-exists? test-dir)
    (system* "rm" "-rf" test-dir))
  (mkdir test-dir))

(define (cleanup-test-env)
  "Remove temporary test directory."
  (when (file-exists? test-dir)
    (system* "rm" "-rf" test-dir)))

(define (write-test-config content)
  "Write a test configuration file."
  (call-with-output-file test-config
    (lambda (port)
      (display content port))))

(define (read-test-config)
  "Read the test configuration file."
  (call-with-input-file test-config read-string))

;;; Minimal config template for testing

(define minimal-config
  "(use-modules (gnu))

(operating-system
  (host-name \"test\")
  (timezone \"UTC\")
  (locale \"en_US.utf8\")
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '(\"/dev/sda\"))))
  (file-systems %base-file-systems)
  (packages %base-packages)
  (services %base-services))
")

;;; Config with existing packages

(define existing-packages-config
  "(use-modules (gnu)
              (gnu packages base))

(operating-system
  (host-name \"test\")
  (timezone \"UTC\")
  (locale \"en_US.utf8\")
  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '(\"/dev/sda\"))))
  (file-systems %base-file-systems)
  (packages
   (append
    (list
     (specification->package \"bash\")
     (specification->package \"coreutils\"))
    %base-packages))
  (services %base-services))
")

;;; Test suite

(test-begin "add-development-tests")

;;; Test helper functions

(test-group "read-all-exprs"
  (setup-test-env)
  (write-test-config "(define x 1)\n(define y 2)\n(define z 3)")
  (let ((exprs (read-all-exprs test-config)))
    (test-equal "reads all expressions" 3 (length exprs))
    (test-equal "first expr" '(define x 1) (first exprs))
    (test-equal "last expr" '(define z 3) (last exprs)))
  (cleanup-test-env))

(test-group "has-minimal-packages?"
  (setup-test-env)
  (write-test-config minimal-config)
  (let ((exprs (read-all-exprs test-config)))
    (test-assert "detects minimal packages" 
                 (has-minimal-packages? exprs)))
  
  (write-test-config existing-packages-config)
  (let ((exprs (read-all-exprs test-config)))
    (test-assert "detects non-minimal packages" 
                 (not (has-minimal-packages? exprs))))
  (cleanup-test-env))

(test-group "package-already-present?"
  (setup-test-env)
  (write-test-config existing-packages-config)
  (let ((exprs (read-all-exprs test-config)))
    (test-assert "finds existing package" 
                 (package-already-present? exprs "bash"))
    (test-assert "doesn't find missing package" 
                 (not (package-already-present? exprs "git"))))
  (cleanup-test-env))

(test-group "create-package-list"
  (let ((result (create-package-list '("git" "vim"))))
    (test-assert "creates list form" (eq? (car result) 'list))
    (test-equal "has correct number of packages" 3 (length result))
    (test-equal "first package" 
                '(specification->package "git") 
                (second result))))

(test-group "add-packages-to-minimal"
  (setup-test-env)
  (write-test-config minimal-config)
  (let* ((exprs (read-all-exprs test-config))
         (new-exprs (add-packages-to-minimal exprs '("git" "vim"))))
    (test-assert "transforms config" (list? new-exprs))
    (test-assert "no longer minimal" 
                 (not (has-minimal-packages? new-exprs)))
    ;; Verify packages were added
    (test-assert "contains git" 
                 (package-already-present? new-exprs "git")))
  (cleanup-test-env))

(test-group "add-packages-to-existing"
  (setup-test-env)
  (write-test-config existing-packages-config)
  (let* ((exprs (read-all-exprs test-config))
         (new-exprs (add-packages-to-existing exprs '("git" "vim" "bash"))))
    (test-assert "adds new packages" 
                 (package-already-present? new-exprs "git"))
    (test-assert "preserves existing" 
                 (package-already-present? new-exprs "bash")))
  (cleanup-test-env))

;;; Integration tests

(test-group "add-development integration"
  (setup-test-env)
  
  ;; Test with minimal config
  (write-test-config minimal-config)
  (setenv "CONFIG_FILE" test-config)
  (add-development)
  (let ((exprs (read-all-exprs test-config)))
    (test-assert "adds git to minimal config" 
                 (package-already-present? exprs "git"))
    (test-assert "adds vim to minimal config" 
                 (package-already-present? exprs "vim")))
  
  ;; Test with existing packages
  (write-test-config existing-packages-config)
  (add-development)
  (let ((exprs (read-all-exprs test-config)))
    (test-assert "adds git to existing config" 
                 (package-already-present? exprs "git"))
    (test-assert "preserves bash" 
                 (package-already-present? exprs "bash")))
  
  (cleanup-test-env))

(test-end "add-development-tests")

;;; Run tests and exit with appropriate status
(exit (if (zero? (test-runner-fail-count (test-runner-current)))
          0
          1))
