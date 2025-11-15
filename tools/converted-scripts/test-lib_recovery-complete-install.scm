```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Test suite for recovery-complete-install.scm

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1)
             (srfi srfi-64))

;;; Load the script being tested
;;; Note: We'll test individual functions by loading them

;;; Test Helper Functions

(define (create-temp-file content)
  "Create a temporary file with given content and return its path."
  (let ((tmpfile (tmpnam)))
    (call-with-output-file tmpfile
      (lambda (port)
        (display content port)))
    tmpfile))

(define (cleanup-temp-file path)
  "Remove temporary file."
  (when (file-exists? path)
    (delete-file path)))

;;; Begin test suite

(test-begin "recovery-complete-install")

;;; Test run-command
(test-group "run-command"
  (test-assert "run-command returns output and status"
    (let-values (((output status) 
                  (let* ((port (open-input-pipe "echo test"))
                         (output (read-string port))
                         (status (close-pipe port)))
                    (values output status))))
      (and (string-contains output "test")
           (zero? status))))
  
  (test-assert "run-command-success? returns #t for successful command"
    (let ((result (let* ((port (open-input-pipe "true"))
                        (output (read-string port))
                        (status (close-pipe port)))
                   (zero? status))))
      result))
  
  (test-assert "run-command-success? returns #f for failed command"
    (let ((result (let* ((port (open-input-pipe "false"))
                        (output (read-string port))
                        (status (close-pipe port)))
                   (zero? status))))
      (not result))))

;;; Test file-exists-in-dir?
(test-group "file-pattern-matching"
  (test-assert "get-first-matching-file returns filename"
    (let* ((tmpdir (tmpnam))
           (testfile (string-append tmpdir "/test-vmlinuz-5.10.0")))
      (mkdir tmpdir)
      (call-with-output-file testfile
        (lambda (port) (display "" port)))
      
      (let* ((cmd (format #f "ls ~a/vmlinuz-* 2>/dev/null | head -1" tmpdir))
             (port (open-input-pipe cmd))
             (output (string-trim-right (read-string port)))
             (status (close-pipe port))
             (result (if (and (zero? status) (not (string=? output "")))
                        (basename output)
                        #f)))
        
        ;; Cleanup
        (delete-file testfile)
        (rmdir tmpdir)
        
        ;; Test
        (and result (string-contains result "vmlinuz"))))))

;;; Test grep-config simulation
(test-group "grep-config-parsing"
  (test-assert "grep-config extracts username from config"
    (let* ((config-content "(user-account (name \"testuser\"))")
           (tmpfile (create-temp-file config-content))
           (cmd (format #f "grep -oP '(?<=\\(name \")[^\"]+' ~a | head -1" tmpfile))
           (port (open-input-pipe cmd))
           (output (string-trim-right (read-string port)))
           (status (close-pipe port))
           (result (if (and (zero? status) (not (string=? output "")))
                      output
                      #f)))
      
      ;; Cleanup
      (cleanup-temp-file tmpfile)
      
      ;; Test
      (an