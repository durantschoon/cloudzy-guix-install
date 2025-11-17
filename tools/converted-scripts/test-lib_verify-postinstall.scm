!#

;;; test-verify-postinstall.scm
;;; Tests for verify-postinstall.scm

(use-modules (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1))

;; Load the script being tested
(load "lib/verify-postinstall.scm")

;;; Test configuration

(define test-manifest
  "# Test Manifest
## Guile Library Scripts
abc123def456  lib/guile-config-helper.scm

## Post-Install Customization Scripts
111222333444  framework/postinstall/customize
555666777888  cloudzy/postinstall/customize

## Critical Shell Scripts
999000111222  lib/postinstall.sh
")

;;; Test helpers

(define (create-test-file path content)
  "Create a test file with given content."
  (call-with-output-file path
    (lambda (port)
      (display content port))))

(define (create-test-file-with-hash path hash-prefix)
  "Create a test file with content that produces a specific hash prefix.
   (Note: This is simplified - real tests would need actual hash matching)"
  (call-with-output-file path
    (lambda (port)
      (display (string-append "test content for " hash-prefix) port))))

(define test-dir #f)

(define (setup-test-env)
  "Create temporary test directory structure."
  (set! test-dir (mkdtemp "/tmp/verify-postinstall-test-XXXXXX"))
  (mkdir (string-append test-dir "/lib"))
  (mkdir (string-append test-dir "/framework"))
  (mkdir (string-append test-dir "/framework/postinstall"))
  (mkdir (string-append test-dir "/cloudzy"))
  (mkdir (string-append test-dir "/cloudzy/postinstall")))

(define (teardown-test-env)
  "Remove temporary test directory."
  (when test-dir
    (system* "rm" "-rf" test-dir)
    (set! test-dir #f)))

(define (with-test-env thunk)
  "Run thunk with test environment setup/teardown."
  (dynamic-wind
    setup-test-env
    thunk
    teardown-test-env))

;;; Test cases

(define (test-extract-expected-hash)
  "Test extracting hash from manifest."
  (format #t "Testing extract-expected-hash...~%")
  
  (let ((hash (extract-expected-hash test-manifest
                                    "lib/guile-config-helper.scm"
                                    "Guile Library Scripts")))
    (if (string=? hash "abc123def456")
        (format #t "  ✓ Extracted correct hash~%")
        (begin
          (format #t "  ✗ Failed to extract hash: ~a~%" hash)
          (exit 1))))
  
  (let ((hash (extract-expected-hash test-manifest
                                    "nonexistent.scm"
                                    "Guile Library Scripts")))
    (if (not hash)
        (format #t "  ✓ Returns #f for nonexistent file~%")
        (begin
          (format #t "  ✗ Should return #f for nonexistent: ~a~%" hash)
          (exit 1)))))

(define (test-calculate-file-hash)
  "Test file hash calculation."
  (format #t "Testing calculate-file-hash...~%")
  
  (with-test-env
   (lambda ()
     (let ((test-file (string-append test-dir "/test.txt")))
       (create-test-file test-file "test content\n")
       
       (let ((hash (calculate-file-hash test-file)))
         (if (and hash (string? hash) (= (string-length hash) 64))
             (format #t "  ✓ Calculated valid SHA256 hash~%")
             (begin
               (format #t "  ✗ Invalid hash: ~a~%" hash)
               (exit 1))))
       
       (let ((hash (calculate-file-hash "/nonexistent/file.txt")))
         (if (not hash)
             (format #t "  ✓ Returns #f for nonexistent file~%")
             (begin
               (format #t "  ✗ Should return #f for nonexistent~%")
               (exit 1))))))))

(define (test-verify-file-not-found)
  "Test verify-file with nonexistent file."
  (format #t "Testing verify-file (not found)...~%")
  
  (with-test-env
   (lambda ()
     (let ((result (verify-file "/nonexistent/file.txt"
                               "Test Section"
                               test-manifest)))
       (if (eq? result 'not-found)
           (format #t "  ✓ Returns 'not-found for missing file~%")
           (begin
             (format #t "  ✗ Expected 'not-found, got: ~a~%" result)
             (exit 1)))))))

(define (test-verify-file-not-in-manifest)
  "Test verify-file with file not in manifest."
  (format #t "Testing verify-file (not in manifest)...~%")
  
  (with-test-env
   (lambda ()
     (let ((test-file (string-append test-dir "/unknown.txt")))
       (create-test-file test-file "content\n")
       
       (let ((result (verify-file test-file
                                 "Test Section"
                                 test-manifest)))
         (if (eq? result 'not-in-manifest)
             (format #t "  ✓ Returns 'not-in-manifest for unlisted file~%")
             (begin
               (format #t "  ✗ Expected 'not-in-manifest, got: ~a~%" result)
               (exit 1))))))))

;;; Test runner

(define (run-tests)
  "Run all tests."
  (format #t "~%=== Running verify-postinstall.scm tests ===~%~%")
  
  (test-extract-expected-hash)
  (newline)
  
  (test-calculate-file-hash)
  (newline)
  
  (test-verify-file-not-found)
  (newline)
  
  (test-verify-file-not-in-manifest)
  (newline)
  
  (format #t "=== All tests passed ===~%~%"))

;; Run tests if executed as script
(when (string-suffix? "test-verify-postinstall.scm" (car (command-line)))
  (run-tests))
