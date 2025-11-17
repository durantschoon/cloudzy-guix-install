!#

;;; Tests for add-spacemacs.scm

(use-modules (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1))

;;; Test Framework

(define test-count 0)
(define test-pass 0)
(define test-fail 0)

(define (test-assert name condition)
  "Simple assertion test."
  (set! test-count (+ test-count 1))
  (if condition
      (begin
        (set! test-pass (+ test-pass 1))
        (format #t "✓ ~a~%" name))
      (begin
        (set! test-fail (+ test-fail 1))
        (format #t "✗ ~a~%" name))))

(define (test-summary)
  "Print test summary."
  (newline)
  (format #t "Tests run: ~a~%" test-count)
  (format #t "Passed: ~a~%" test-pass)
  (format #t "Failed: ~a~%" test-fail)
  (if (zero? test-fail)
      (begin
        (format #t "~%All tests passed!~%")
        (exit 0))
      (begin
        (format #t "~%Some tests failed!~%")
        (exit 1))))

;;; Test Setup

(define test-dir (string-append (getcwd) "/test-spacemacs-tmp"))
(define test-config (string-append test-dir "/config.scm"))

(define (setup-test-env!)
  "Create test environment."
  (when (access? test-dir F_OK)
    (system* "rm" "-rf" test-dir))
  (mkdir test-dir)
  ;; Create a minimal test config file
  (call-with-output-file test-config
    (lambda (port)
      (display "(use-modules (gnu))\n" port)
      (display "(operating-system\n" port)
      (display "  (host-name \"test\")\n" port)
      (display "  (packages %base-packages))\n" port))))

(define (cleanup-test-env!)
  "Clean up test environment."
  (when (access? test-dir F_OK)
    (system* "rm" "-rf" test-dir)))

;;; Load the script

(load "add-spacemacs.scm")

;;; Tests

(define (run-tests)
  "Run all tests."
  (display "Running add-spacemacs.scm tests...\n\n")
  
  ;; Test 1: Helper functions exist
  (test-assert "info procedure exists"
               (procedure? info))
  
  (test-assert "warn procedure exists"
               (procedure? warn))
  
  (test-assert "success procedure exists"
               (procedure? success))
  
  ;; Test 2: File operations
  (setup-test-env!)
  
  (test-assert "file-contains? detects existing pattern"
               (file-contains? test-config "use-modules"))
  
  (test-assert "file-contains? returns #f for missing pattern"
               (not (file-contains? test-config "nonexistent-pattern")))
  
  ;; Test 3: Directory operations
  (test-assert "directory-exists? detects existing directory"
               (directory-exists? test-dir))
  
  (test-assert "directory-exists? returns #f for missing directory"
               (not (directory-exists? (string-append test-dir "/nonexistent"))))
  
  ;; Test 4: Main procedure exists
  (test-assert "add-spacemacs procedure exists"
               (procedure? add-spacemacs))
  
  ;; Test 5: Config file path
  (test-assert "config-file is set"
               (string? config-file))
  
  ;; Cleanup
  (cleanup-test-env!)
  
  ;; Print summary
  (test-summary))

;;; Run tests
(run-tests)
