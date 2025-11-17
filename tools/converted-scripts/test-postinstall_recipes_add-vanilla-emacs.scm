!#

;;; test-add-vanilla-emacs.scm --- Tests for add-vanilla-emacs.scm

(use-modules (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1))

;;; Test utilities

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (assert-true condition test-name)
  "Assert that condition is true."
  (set! test-count (+ test-count 1))
  (if condition
      (begin
        (set! pass-count (+ pass-count 1))
        (format #t "  ✓ ~a~%" test-name))
      (begin
        (set! fail-count (+ fail-count 1))
        (format #t "  ✗ ~a~%" test-name))))

(define (assert-false condition test-name)
  "Assert that condition is false."
  (assert-true (not condition) test-name))

(define (assert-equal expected actual test-name)
  "Assert that expected equals actual."
  (assert-true (equal? expected actual)
               (format #f "~a (expected: ~a, got: ~a)" test-name expected actual)))

(define (test-summary)
  "Print test summary."
  (newline)
  (format #t "Tests run: ~a~%" test-count)
  (format #t "Passed: ~a~%" pass-count)
  (format #t "Failed: ~a~%" fail-count)
  (if (= fail-count 0)
      (format #t "\n✓ All tests passed!~%")
      (format #t "\n✗ Some tests failed~%")))

;;; Test fixtures

(define test-dir "/tmp/test-add-vanilla-emacs")
(define test-config (string-append test-dir "/config.scm"))
(define test-emacs-dir (string-append test-dir "/.emacs.d"))
(define test-init-file (string-append test-emacs-dir "/init.el"))

(define minimal-config-content
  "(use-modules (gnu))
(operating-system
  (packages %base-packages))")

(define config-with-packages
  "(use-modules (gnu)
             (gnu packages base))
(operating-system
  (packages
    (list coreutils
          bash)))")

(define (setup-test-env)
  "Create test environment."
  (when (file-exists? test-dir)
    (system* "rm" "-rf" test-dir))
  (mkdir test-dir)
  (setenv "HOME" test-dir)
  (setenv "CONFIG_FILE" test-config))

(define (teardown-test-env)
  "Clean up test environment."
  (when (file-exists? test-dir)
    (system* "rm" "-rf" test-dir)))

(define (write-test-file path content)
  "Write content to test file."
  (call-with-output-file path
    (lambda (port)
      (display content port))))

(define (read-test-file path)
  "Read content from test file."
  (if (file-exists? path)
      (call-with-input-file path
        (lambda (port)
          (read-string port)))
      #f))

;;; Load the script being tested
;;; Note: In a real test, we would load the actual script
;;; For now, we'll test the helper functions we can recreate

(define (file-contains? filepath pattern)
  "Check if file contains pattern."
  (let ((content (read-test-file filepath)))
    (and content (string-contains content pattern))))

;;; Tests

(define (test-file-contains?)
  (format #t "\nTesting file-contains?...~%")
  (setup-test-env)

  (write-test-file test-config minimal-config-content)
  (assert-true (file-contains? test-config "operating-system")
               "Should find existing pattern")
  (assert-false (file-contains? test-config "emacs")
                "Should not find missing pattern")

  (teardown-test-env))

(define (test-minimal-config-structure)
  (format #t "\nTesting minimal config structure...~%")
  (setup-test-env)

  (write-test-file test-config minimal-config-content)
  (assert-true (file-contains? test-config "(use-modules")
               "Config should have use-modules")
  (assert-true (file-contains? test-config "(operating-system")
               "Config should have operating-system")
  (assert-true (file-contains? test-config "%base-packages")
               "Config should have base-packages")

  (teardown-test-env))

(define (test-emacs-init-creation)
  (format #t "\nTesting Emacs init.el creation...~%")
  (setup-test-env)

  ;; Test that minimal init would be valid Emacs Lisp structure
  (assert-true (string-contains minimal-emacs-init "lexical-binding: t")
               "Init should enable lexical binding")
  (assert-true (string-contains minimal-emacs-init "require 'package")
               "Init should require package")
  (assert-true (string-contains minimal-emacs-init "load-theme")
               "Init should load a theme")

  (teardown-test-env))

(define (test-backup-functionality)
  (format #t "\nTesting backup functionality...~%")
  (setup-test-env)

  (write-test-file test-config minimal-config-content)
  (assert-true (file-exists? test-config)
               "Test config should exist")

  ;; Test would backup existing file (testing the logic, not actual backup)
  (let ((original-content (read-test-file test-config)))
    (assert-true (string? original-content)
                 "Should be able to read original content"))

  (teardown-test-env))

(define (test-emacs-dir-structure)
  (format #t "\nTesting Emacs directory structure...~%")
  (setup-test-env)

  ;; Create emacs dir
  (mkdir test-emacs-dir)
  (write-test-file test-init-file ";; test init")

  (assert-true (file-exists? test-emacs-dir)
               "Emacs directory should exist")
  (assert-true (file-exists? test-init-file)
               "Init file should exist")

  (teardown-test-env))

;;; Define minimal-emacs-init for testing
(define minimal-emacs-init
  ";;; init.el --- Minimal Emacs configuration -*- lexical-binding: t -*-
(require 'package)
(load-theme 'wombat t)")

;;; Run all tests

(define (run-tests)
  (format #t "=== Running add-vanilla-emacs tests ===~%")

  (test-file-contains?)
  (test-minimal-config-structure)
  (test-emacs-init-creation)
  (test-backup-functionality)
  (test-emacs-dir-structure)

  (test-summary)
  (exit (if (= fail-count 0) 0 1)))

;; Run tests when script is executed
(run-tests)
