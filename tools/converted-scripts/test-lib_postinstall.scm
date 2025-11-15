```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; test-postinstall.scm - Tests for postinstall.scm
;;; Tests channel generation and configuration handling

(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-64))  ; Testing framework

;;; Load the script being tested
(load "postinstall.scm")

;;; Test suite setup

(test-begin "postinstall")

;;; Test: generate-channels-scm with defaults

(test-group "generate-channels-scm-defaults"
  (let ((output (generate-channels-scm)))
    (test-assert "output is a string"
      (string? output))
    
    (test-assert "contains guix channel"
      (string-contains output "(name 'guix)"))
    
    (test-assert "contains nonguix channel"
      (string-contains output "(name 'nonguix)"))
    
    (test-assert "contains default guix URL"
      (string-contains output "https://git.savannah.gnu.org/git/guix.git"))
    
    (test-assert "contains default nonguix URL"
      (string-contains output "https://gitlab.com/nonguix/nonguix.git"))
    
    (test-assert "contains master branch"
      (string-contains output "(branch \"master\")"))
    
    (test-assert "is valid S-expression"
      (with-input-from-string output
        (lambda ()
          (let ((expr (read)))
            (and (pair? expr)
                 (eq? (car expr) 'list))))))))

;;; Test: generate-channels-scm with custom URLs

(test-group "generate-channels-scm-custom"
  (let ((custom-guix "https://custom-mirror.example.com/guix.git")
        (custom-nonguix "https://custom-mirror.example.com/nonguix.git"))
    
    (let ((output (generate-channels-scm 
                   #:guix-url custom-guix
                   #:nonguix-url custom-nonguix)))
      
      (test-assert "contains custom guix URL"
        (string-contains output custom-guix))
      
      (test-assert "contains custom nonguix URL"
        (string-contains output custom-nonguix))
      
      (test-assert "does not contain default guix URL"
        (not (string-contains output "https://git.savannah.gnu.org/git/guix.git")))
      
      (test-assert "is valid S-expression"
        (with-input-from-string output
          (lambda ()
            (let ((expr (read)))
              (and (pair? expr)
                   (eq? (car expr) 'list)))))))))

;;; Test: generate-channels-scm with environment variables

(test-group "generate-channels-scm-env-vars"
  ;; Save original environment
  (let ((orig-guix-url (getenv "GUIX_GIT_URL"))
        (orig-nonguix-url (getenv "NONGUIX_GIT_URL")))
    
    ;; Set test environment variables
    (setenv "GUIX_GIT_URL" "https://env-mirror.example.com/guix.git")
    (setenv "NONGUIX_GIT_URL" "https://env-mirror.example.com/nonguix.git")
    
    (let ((output (generate-channels-scm)))
      
      (test-assert "uses GUIX_GIT_URL from environment"
        (string-contains output "https://env-mirror.example.com/guix.git"))
      
      (test-assert "uses NONGUIX_GIT_URL from environment"
        (string-contains output "https://env-mirror.example.com/nonguix.git")))
    
    ;; Restore original environment
    (if orig-guix-url
        (setenv "GUIX_GIT_URL" orig-guix-url)
        (unsetenv "GUIX_GIT_URL"))
    (if orig-nonguix-url
        (setenv "NONGUIX_GIT_URL" orig-nonguix-url)
        (unsetenv "NONGUIX_GIT_URL"))))

;;; Test: output format validation

(test-group "output-format"
  (let ((output (generate-channels-scm)))
    
    (test-assert "starts with comment"
      (string-prefix? ";;" output))
    
    (test-assert "contains channel configuration header"
      (string-contains output "Guix channels configuration"))
    
    (test-assert "contains mirror optimization comment"
      (string-contains output "regional mirror optimization"))
    
    (test-assert "proper S-expression structure"
      (with-input-from-string output
        (lambda ()
          (let ((expr (read)))
            (and (pair? expr)
                 (eq? (car expr) 'list)
                 (= (length (cdr expr)) 2)  ; Two channels
                 (every (lambda (ch)
                          (and (pair? ch)
                               (eq? (car ch) 'channel)))
                        (cdr expr)))))))))

;;; Test: URL validation (basic sanity checks)

(test-group "url-validation"
  (let ((output (generate-channels-scm 
                 #:guix-url "file:///local/guix.git"
                 #:nonguix-url "ssh://git@example.com/nonguix.git")))
    
    (test-assert "accepts file:// URLs"
      (string-contains output "file:///local/guix.git"))
    
    (test-assert "accepts ssh:// URLs"
      (string-contains output "ssh://git@example.com/nonguix.git"))))

;;; Test suite teardown

(test-end "postinstall")

;;; Display test results summary
(define (display-test-summary)
  (let ((results (test-runner-get (test-runner-current))))
    (format #t "~%Test Summary:~%")
    (format #t "  Passed: ~a~%" (test-runner-pass-count results))
    (format #t "  Failed: ~a~%" (test-runner-fail-count results))
    (format #t "  Skipped: ~a~%" (test-runner-skip-count results))
    (when (> (test-runner-fail-count results) 0)
      (exit 1))))

(display-test-summary)
```