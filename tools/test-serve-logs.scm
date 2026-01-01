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

    ;; Test: Basic syntax check (placeholder)
    (test-assert "script loads" #t)

;;; End of tests
(test-end "serve-logs")

;; Print summary
(format #t "~%Test Summary:~%")
(format #t "  Tests passed: ~a~%" (test-runner-pass-count runner))
(format #t "  Tests failed: ~a~%" (test-runner-fail-count runner))

;; Exit with error code if any tests failed
(exit (zero? (test-runner-fail-count runner)))
