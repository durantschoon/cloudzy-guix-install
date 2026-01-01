#!/usr/bin/env guile
!#

;;; Test suite for serve-logs.scm
;;; Usage: guile tools/test-serve-logs.scm

(use-modules (srfi srfi-64)
             (ice-9 ftw)
             (ice-9 format))

;; Define testing mode to prevent main from running
(define %testing-mode #t)

;; Load the script under test
(load (string-append (dirname (current-filename)) "/serve-logs.scm"))

;; Initialize runner explicitly
(define runner (test-runner-simple))
(test-runner-current runner)

;;; Test Configuration
(test-begin "serve-logs")

;;; Helper Functions Tests

(test-group "copy-file-safe"
  (let ((src-file "/tmp/test-serve-logs-src.txt")
        (dest-file "/tmp/test-serve-logs-dest.txt"))
    
    ;; Setup
    (call-with-output-file src-file
      (lambda (port) (display "test content" port)))
    (when (file-exists? dest-file) (delete-file dest-file))
    
    ;; Test: Copy existing file
    (copy-file-safe src-file dest-file)
    (test-assert "copies existing file"
      (file-exists? dest-file))
      
    ;; Test: Missing file (should not error)
    (test-assert "ignores missing file"
      (begin
        (copy-file-safe "/nonexistent/file.txt" "/tmp/should-not-exist.txt")
        #t))
        
    ;; Cleanup
    (when (file-exists? src-file) (delete-file src-file))
    (when (file-exists? dest-file) (delete-file dest-file))))

;;; End of tests
(test-end "serve-logs")

;; Print summary
(format #t "~%Test Summary:~%")
(format #t "  Tests passed: ~a~%" (test-runner-pass-count runner))
(format #t "  Tests failed: ~a~%" (test-runner-fail-count runner))

;; Exit with error code if any tests failed
(exit (zero? (test-runner-fail-count runner)))
