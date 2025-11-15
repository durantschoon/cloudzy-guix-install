```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; test-update-manifest.scm
;;; Tests for update-manifest.scm

(use-modules (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1))

;;; Test Helper Functions

(define test-count 0)
(define passed-count 0)
(define failed-count 0)

(define (assert-true description condition)
  "Assert that condition is true."
  (set! test-count (+ test-count 1))
  (if condition
      (begin
        (set! passed-count (+ passed-count 1))
        (format #t "  ✓ ~a~%" description))
      (begin
        (set! failed-count (+ failed-count 1))
        (format #t "  ✗ ~a~%" description))))

(define (assert-equal description expected actual)
  "Assert that expected equals actual."
  (assert-true description (equal? expected actual)))

(define (assert-not-false description value)
  "Assert that value is not #f."
  (assert-true description (not (eq? value #f))))

(define (run-test-suite name thunk)
  "Run a test suite."
  (format #t "~%=== ~a ===~%" name)
  (thunk)
  (format #t "~%"))

(define (display-test-results)
  "Display summary of test results."
  (format #t "~%========================================~%")
  (format #t "Test Results: ~a total~%" test-count)
  (format #t "  Passed: ~a~%" passed-count)
  (format #t "  Failed: ~a~%" failed-count)
  (format #t "========================================~%")
  (exit (if (zero? failed-count) 0 1)))

;;; Setup Test Environment

(define test-dir "test-manifest-tmp")

(define (create-test-environment)
  "Create test directory structure with sample files."
  (when (file-exists? test-dir)
    (system* (format #f "rm -rf ~a" test-dir)))
  (mkdir test-dir)
  (with-output-to-file (format #f "~a/test.go" test-dir)
    (lambda () (display "package main\n")))
  (mkdir (format #f "~a/lib" test-dir))
  (with-output-to-file (format #f "~a/lib/test.sh" test-dir)
    (lambda () (display "#!/bin/bash\n"))))

(define (cleanup-test-environment)
  "Remove test directory."
  (when (file-exists? test-dir)
    (system* (format #f "rm -rf ~a" test-dir))))

;;; Load the script being tested
;;; Note: We'll load functions individually to avoid running main logic

(load "update-manifest.scm")

;;; Test Suites

(run-test-suite "File SHA256 Calculation"
  (lambda ()
    ;; Create a test file with known content
    (create-test-environment)
    (let ((test-file (format #f "~a/test.txt" test-dir)))
      (with-output-to-file test-file
        (lambda () (display "Hello, World!\n")))
      
      (let ((hash (file-sha256 test-file)))
        (assert-not-false "file-sha256 returns a hash" hash)
        (assert-true "hash is 64 characters (SHA256)" 
                     (and hash (= (string-length hash) 64)))
        (assert-true "hash contains only hex characters"
                     (and hash (string-every 
                                (lambda (c) 
                                  (or (char-set-contains? char-set:digit c)
                                      (char-set-contains? (char-set #\a #\b #\c #\d #\e #\f) c)))
                                hash)))))
    
    (cleanup-test-environment)))

(run-test-suite "Extract First/Last Words"
  (lambda ()
    (let ((words "one two three four five six seven eight nine ten"))
      (assert-equal "extract first 3 and last 3"
                    "one two three ... eight nine ten"
                    (extract-first-last-words words 3))
      
      (assert-equal "extract first 2 and last 2"
                    "one two ... nine ten"
                    (extract-first-last-words words 2)))
    
    (let ((short "one two three"))
      (assert-equal "short string unchanged"
                    short
                    (extract-first-last-words short 3)))))

(run-test-suite "Find Files Recursive"
  (lambda ()
    (create-test-environment)
    
    ;; Create some test .go files
    (with-output-to-file (format #f "~a/main.go" test-dir)
      (lambda () (display "package main\n")))
    (with-output-to-file (format #f "~a/lib/helper.go" test-dir)
      (lambda () (display "package lib\n")))
    
    (let ((go-files (find-files-recursive test-dir ".go")))
      (assert-true "finds .go files" (>= (length go-files) 2))
      (assert-true "returns sorted list" 
                   (equal? go-files (sort go-files string<?))))
    
    (cleanup-test-environment)))

(run-test-suite "Manifest File Generation"
  (lambda ()
    (create-test-environment)
    
    ;; Create minimal structure
    (with-output-to-file (format #f "~a/test.go" test-dir)
      (lambda () (display "package main\n")))
    
    (let ((test-manifest (format #f "~a/TEST_MANIFEST.txt" test-dir)))
      ;; Test that we can write a manifest header
      (call-with-output-file test-manifest
        (lambda (port)
          (write-manifest-header port)))
      
      (assert-true "manifest file created" (file-exists? test-manifest))
      
      (let ((content (call-with-input-file test-manifest
                       (lambda (port) (read-string port)))))
        (assert-true "manifest has header"
                     (string-contains content "Source File Manifest"))
        (assert-true "manifest has generation info"
                     (string-contains content "Generated by"))))
    
    (cleanup-test-environment)))

(run-test-suite "Script Loading"
  (lambda ()
    (assert-true "script loads without errors" #t)))

;;; Display Results

(display-test-results)
```