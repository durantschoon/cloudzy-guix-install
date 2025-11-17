!#

;;; test-lib.scm - Tests for postinstall/lib.scm
;;; Tests the shared functions for post-installation customization

(use-modules (srfi srfi-64)   ; Testing framework
             (ice-9 ftw)
             (ice-9 format))

;;; Load the module being tested
;;; Note: Adjust path as needed based on where test is run from
(load "lib.scm")

;;; Test suite setup
(test-begin "postinstall-lib")

;;; ============================================================================
;;; Output functions tests
;;; ============================================================================

(test-group "output-functions"
  (test-assert "msg displays without error"
    (with-output-to-string
      (lambda () (msg "test message")))
    #t)
  
  (test-assert "warn displays without error"
    (with-output-to-string
      (lambda () (warn "test warning")))
    #t)
  
  (test-assert "err displays without error"
    (with-output-to-string
      (lambda () (err "test error")))
    #t)
  
  (test-assert "info displays without error"
    (with-output-to-string
      (lambda () (info "test info")))
    #t)
  
  (test-assert "success displays without error"
    (with-output-to-string
      (lambda () (success "test success")))
    #t))

;;; ============================================================================
;;; File operations tests
;;; ============================================================================

(test-group "backup-config"
  (test-assert "backup-config creates backup directory"
    (let* ((test-dir (tmpnam))
           (config-file (tmpnam))
           (backup-dir (string-append test-dir "/backups")))
      ;; Create test config file
      (call-with-output-file config-file
        (lambda (port)
          (display "(operating-system)" port)))
      
      ;; Create test directory
      (mkdir test-dir)
      
      ;; Test backup
      (let ((result (backup-config config-file backup-dir)))
        ;; Cleanup
        (when (file-exists? config-file)
          (delete-file config-file))
        (when (file-exists? backup-dir)
          (system* "rm" "-rf" backup-dir))
        (when (file-exists? test-dir)
          (rmdir test-dir))
        
        result))))

;;; ============================================================================
;;; Helper path resolution tests
;;; ============================================================================

(test-group "find-guile-helper"
  (test-assert "find-guile-helper returns #f for non-existent path"
    (not (find-guile-helper "/nonexistent/path")))
  
  (test-assert "find-guile-helper returns path when file exists"
    (let* ((test-dir (tmpnam))
           (lib-dir (string-append test-dir "/lib"))
           (helper-file (string-append lib-dir "/guile-config-helper.scm")))
      ;; Create test structure
      (mkdir test-dir)
      (mkdir lib-dir)
      (call-with-output-file helper-file
        (lambda (port)
          (display ";; test helper\n" port)))
      
      (let ((result (string? (find-guile-helper test-dir))))
        ;; Cleanup
        (delete-file helper-file)
        (rmdir lib-dir)
        (rmdir test-dir)
        result))))

;;; ============================================================================
;;; Safe edit config tests
;;; ============================================================================

(test-group "safe-edit-config"
  (test-assert "safe-edit-config modifies file correctly"
    (let ((test-file (tmpnam)))
      ;; Create test file
      (call-with-output-file test-file
        (lambda (port)
          (display "old-text\n" port)))
      
      ;; Edit file
      (safe-edit-config test-file "s/old-text/new-text/")
      
      ;; Verify change
      (let* ((content (call-with-input-file test-file read-string))
             (result (string-contains content "new-text")))
        ;; Cleanup
        (when (file-exists? test-file)
          (delete-file test-file))
        result))))

;;; ============================================================================
;;; Integration tests
;;; ============================================================================

(test-group "integration"
  (test-assert "clear-screen executes without error"
    (begin
      (clear-screen)
      #t))
  
  (test-assert "add-packages checks for existing configuration"
    (let ((test-file (tmpnam))
          (backup-dir (tmpnam)))
      ;; Create test config with existing packages
      (call-with-output-file test-file
        (lambda (port)
          (display "(operating-system\n" port)
          (display "  (packages (append (list (specification->package \"vim\"))\n" port)
          (display "                    %base-packages)))\n" port)))
      
      ;; Create backup directory
      (mkdir backup-dir)
      
      ;; Test add-packages (should detect existing)
      (let ((result (add-packages test-file backup-dir)))
        ;; Cleanup
        (when (file-exists? test-file)
          (delete-file test-file))
        (when (file-exists? backup-dir)
          (system* "rm" "-rf" backup-dir))
        result))))

;;; Test suite teardown
(test-end "postinstall-lib")

;;; Exit with test result status
(exit (= (test-runner-fail-count (test-runner-current)) 0))
