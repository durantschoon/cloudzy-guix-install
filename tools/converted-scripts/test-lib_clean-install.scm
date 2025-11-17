!#

;;; Test file for clean-install.scm
;;; Tests file removal logic and helper functions

(use-modules (srfi srfi-64)  ; Testing framework
             (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1))

;;; Load the script being tested
;;; We'll load it in a way that doesn't execute main
(primitive-load "clean-install.scm")

;;; Test Setup and Teardown

(define test-dir "/tmp/guile-clean-install-test")

(define (setup-test-environment)
  "Create test directory and sample files."
  (when (file-exists? test-dir)
    (system* "rm" "-rf" test-dir))
  (mkdir test-dir)
  
  ;; Create some test files
  (call-with-output-file (string-append test-dir "/test-file-1.txt")
    (lambda (port) (display "test content 1" port)))
  (call-with-output-file (string-append test-dir "/test-file-2.txt")
    (lambda (port) (display "test content 2" port)))
  (call-with-output-file (string-append test-dir "/test-file-3.txt")
    (lambda (port) (display "test content 3" port))))

(define (teardown-test-environment)
  "Remove test directory."
  (when (file-exists? test-dir)
    (system* "rm" "-rf" test-dir)))

;;; Helper Function Tests

(test-begin "clean-install-tests")

(test-group "file-exists-and-removable?"
  (setup-test-environment)
  
  (test-assert "returns #t for existing regular file"
    (file-exists-and-removable? (string-append test-dir "/test-file-1.txt")))
  
  (test-assert "returns #f for non-existent file"
    (not (file-exists-and-removable? (string-append test-dir "/nonexistent.txt"))))
  
  (test-assert "returns #f for directory"
    (not (file-exists-and-removable? test-dir)))
  
  (teardown-test-environment))

(test-group "remove-if-exists"
  (setup-test-environment)
  
  (test-equal "returns 'removed for existing file"
    'removed
    (remove-if-exists (string-append test-dir "/test-file-1.txt")))
  
  (test-assert "file is actually removed"
    (not (file-exists? (string-append test-dir "/test-file-1.txt"))))
  
  (test-equal "returns 'not-found for non-existent file"
    'not-found
    (remove-if-exists (string-append test-dir "/nonexistent.txt")))
  
  (teardown-test-environment))

(test-group "group-files-by-category"
  (let* ((files '("/mnt/etc/config.scm"
                  "/tmp/channels.scm"
                  "/root/bootstrap.sh"
                  "/root/run-remote-steps"))
         (descriptions '(("/mnt/etc/config.scm" . "Configuration Files")
                        ("/tmp/channels.scm" . "Configuration Files")
                        ("/root/bootstrap.sh" . "Bootstrap Scripts")
                        ("/root/run-remote-steps" . "Binaries")))
         (grouped (group-files-by-category files descriptions)))
    
    (test-assert "returns list of category pairs"
      (every pair? grouped))
    
    (test-assert "groups files correctly"
      (let ((config-files (assoc "Configuration Files" grouped)))
        (and config-files
             (= 2 (length (cdr config-files)))
             (member "/mnt/etc/config.scm" (cdr config-files))
             (member "/tmp/channels.scm" (cdr config-files)))))
    
    (test-assert "maintains file order within categories"
      (let ((config-files (assoc "Configuration Files" grouped)))
        (and config-files
             (equal? (cdr config-files)
                     '("/mnt/etc/config.scm" "/tmp/channels.scm")))))))

(test-group "cleanup-files integration"
  (setup-test-environment)
  
  (let* ((test-files (list (string-append test-dir "/test-file-1.txt")
                           (string-append test-dir "/test-file-2.txt")
                           (string-append test-dir "/nonexistent.txt")))
         (test-descriptions (list (cons (string-append test-dir "/test-file-1.txt")
                                       "Test Category")
                                 (cons (string-append test-dir "/test-file-2.txt")
                                       "Test Category")
                                 (cons (string-append test-dir "/nonexistent.txt")
                                       "Test Category"))))
    
    ;; Capture output to avoid cluttering test results
    (with-output-to-file "/dev/null"
      (lambda ()
        (let-values (((removed not-found errors)
                      (cleanup-files test-files test-descriptions)))
          
          (test-equal "removed list has 2 files"
            2
            (length removed))
          
          (test-equal "not-found list has 1 file"
            1
            (length not-found))
          
          (test-equal "errors list is empty"
            0
            (length errors))
          
          (test-assert "removed files don't exist anymore"
            (not (or (file-exists? (string-append test-dir "/test-file-1.txt"))
                     (file-exists? (string-append test-dir "/test-file-2.txt")))))))))
  
  (teardown-test-environment))

(test-group "display functions"
  ;; These tests just verify the functions can be called without error
  ;; Real output testing would require capturing stdout
  
  (test-assert "display-header runs without error"
    (with-output-to-file "/dev/null"
      (lambda () (display-header) #t)))
  
  (test-assert "display-category-header runs without error"
    (with-output-to-file "/dev/null"
      (lambda () (display-category-header "Test Category") #t)))
  
  (test-assert "display-file-result runs without error"
    (with-output-to-file "/dev/null"
      (lambda ()
        (display-file-result "/test/file.txt" 'removed)
        (display-file-result "/test/file.txt" 'not-found)
        (display-file-result "/test/file.txt" 'error)
        #t)))
  
  (test-assert "display-summary runs without error"
    (with-output-to-file "/dev/null"
      (lambda ()
        (display-summary '("/file1" "/file2") '("/file3") '())
        #t)))
  
  (test-assert "display-next-steps runs without error"
    (with-output-to-file "/dev/null"
      (lambda () (display-next-steps) #t))))

(test-end "clean-install-tests")

;;; Print test summary
(format #t "~%Test run complete.~%")
