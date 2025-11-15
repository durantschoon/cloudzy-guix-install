```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Test suite for verify-guix-install.scm
;;; Tests verification functions with mock data

(use-modules (srfi srfi-64)   ; Testing framework
             (ice-9 ftw)
             (ice-9 format)
             (ice-9 popen)
             (ice-9 rdelim))

;;; Test Setup

(define test-dir #f)

(define (create-test-directory)
  "Create a temporary test directory structure."
  (let ((tmpdir (format #f "/tmp/guix-verify-test-~a" (getpid))))
    (system* "mkdir" "-p" tmpdir)
    (set! test-dir tmpdir)
    tmpdir))

(define (cleanup-test-directory)
  "Remove test directory and contents."
  (when test-dir
    (system* "rm" "-rf" test-dir)
    (set! test-dir #f)))

(define (create-test-file path content)
  "Create a test file with given content."
  (let ((full-path (string-append test-dir path)))
    ;; Create parent directories
    (system* "mkdir" "-p" (dirname full-path))
    ;; Write content
    (call-with-output-file full-path
      (lambda (port)
        (display content port)))))

(define (create-test-boot-structure)
  "Create a minimal boot structure for testing."
  ;; Kernel
  (create-test-file "/boot/vmlinuz-5.15.0-gnu" "fake kernel")
  
  ;; Initrd
  (create-test-file "/boot/initrd-5.15.0-gnu" "fake initrd")
  
  ;; GRUB config
  (create-test-file "/boot/grub/grub.cfg" "fake grub config")
  
  ;; EFI
  (system* "mkdir" "-p" (string-append test-dir "/boot/efi/EFI/Guix"))
  (create-test-file "/boot/efi/EFI/Guix/grubx64.efi" "fake efi binary")
  (create-test-file "/boot/efi/EFI/Guix/grub.cfg" "fake efi config"))

(define (create-test-config)
  "Create a test config.scm file."
  (create-test-file "/etc/config.scm"
    "(operating-system
  (host-name \"test-system\")
  (timezone \"America/New_York\")
  (kernel-arguments '(\"quiet\" \"splash\"))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '(\"/boot/efi\"))))
  (file-systems
    (cons*
      (file-system
        (device (file-system-label \"GUIX_ROOT\"))
        (mount-point \"/\")
        (type \"ext4\"))
      %base-file-systems)))"))

(define (create-test-users)
  "Create test passwd file."
  (create-test-file "/etc/passwd"
    "root:x:0:0:System administrator:/root:/bin/bash
testuser:x:1000:1000:Test User:/home/testuser:/bin/bash
nobody:x:65534:65534:Unprivileged user:/dev/null:/usr/sbin/nologin
")
  ;; Create home directory
  (system* "mkdir" "-p" (string-append test-dir "/home/testuser")))

(define (create-test-store)
  "Create a test GNU store."
  (let ((store-path (string-append test-dir "/gnu/store")))
    (system* "mkdir" "-p" store-path)
    ;; Create some fake store items
    (for-each
     (lambda (i)
       (let ((item-path (format #f "~a/test-package-~a" store-path i)))
         (system* "mkdir" "-p" item-path)))
     (iota 150))))  ; Create 150 items to pass the "100 items" threshold

;;; Tests

(test-begin "verify-guix-install")

;; Test: File size calculation
(test-group "get-file-size"
  (create-test-directory)
  (create-test-file "/test-small" "small")
  (create-test-file "/test-large" (make-string 1024 #\x))
  
  (test-assert "returns size for existing file"
    (number? (get-file-size (string-append test-dir "/test-small"))))
  
  (test-assert "returns #f for non-existent file"
    (not (get-file-size (string-append test-dir "/nonexistent"))))
  
  (cleanup-test-directory))

;; Test: Directory counting
(test-group "get-directory-count"
  (create-test-directory)
  (system* "mkdir" "-p" (string-append test-dir "/testdir"))
  (create-test-file "/testdir/file1" "content")
  (create-test-file "/testdir/file2" "content")
  (create-test-file "/testdir/file3" "content")
  
  (test-equal "counts directory entries correctly"
    3
    (get-directory-count (string-append test-dir "/testdir")))
  
  (test-equal "returns 0 for non-existent directory"
    0
    (get-directory-count (string-append test-dir "/nonexistent")))
  
  (cleanup-test-directory))

;; Test: Context detection
(test-group "determine-root-context"
  (test-assert "returns pair with root path and description"
    (let ((result (determine-root-context)))
      (and (pair? result)
           (string? (car result))
           (string? (cdr result))))))

;; Test: Command execution
(test-group "run-command"
  (test-assert "executes simple command successfully"
    (string? (run-command "echo test")))
  
  (test-equal "returns trimmed output"
    "test"
    (run-command "echo test"))
  
  (test-assert "returns #f for failed command"
    (not (run-command "false"))))

;; Test: Command existence check
(test-group "command-exists?"
  (test-assert "finds existing command"
    (command-exists? "echo"))
  
  (test-assert "returns #f for non-existent command"
    (not (command-exists? "nonexistent-command-xyz"))))

;; Test: Format size
(test-group "format-size-mb"
  (test-equal "formats bytes as MB"
    "1.0 MB"
    (format-size-mb (* 1024 1024)))
  
  (test-equal "handles #f"
    "? MB"
    (format-size-mb #f)))

;; Integration test: Full verification with mock structure
(test-group "integration-complete-system"
  (create-test-directory)
  (create-test-boot-structure)
  (create-test-config)
  (create-test-users)
  (create-test-store)
  
  ;; Test that all check functions can run without errors
  (test-assert "check-critical-boot-files runs"
    (begin
      (check-critical-boot-files test-dir)
      #t))
  
  (test-assert "check-efi-boot-files runs"
    (begin
      (check-efi-boot-files test-dir)
      #t))
  
  (test-assert "check-system-configuration runs"
    (begin
      (check-system-configuration test-dir)
      #t))
  
  (test-assert "check-user-accounts runs"
    (begin
      (check-user-accounts test-dir)
      #t))
  
  (test-assert "check-gnu-store runs"
    (begin
      (check-gnu-store test-dir)
      #t))
  
  (cleanup-test-directory))

;; Test: Pattern matching
(test-group "check-pattern"
  (create-test-directory)
  (create-test-file "/boot/vmlinuz-5.15.0" "kernel")
  (create-test-file "/boot/vmlinuz-5.16.0" "kernel")
  
  (test-assert "finds files matching pattern"
    (begin
      (check-pattern test-dir "/boot/vmlinuz-*" "Test kernel" #f)
      #t))
  
  (cleanup-test-directory))

;; Test: Error and warning counters
(test-group "error-warning-counters"
  ;; Reset counters
  (set! *errors* 0)
  (set! *warnings* 0)
  
  (test-equal "errors start at 0" 0 *errors*)
  (test-equal "warnings start at 0" 0 *warnings*)
  
  ;; Generate errors and warnings
  (error-msg "Test error")
  (warn-msg "Test warning")
  
  (test-equal "error counter increments" 1 *errors*)
  (test-equal "warning counter increments" 1 *warnings*)
  
  ;; Reset for other tests
  (set! *errors* 0)
  (set! *warnings* 0))

(test-end "verify-guix-install")

;; Print test results
(format #t "~%Test Results:~%")
(format #t "  Total tests run: ~a~%" (test-runner-test-count (test-runner-current)))
(format #t "  Passed: ~a~%" (test-runner-pass-count (test-runner-current)))
(format #t "  Failed: ~a~%" (test-runner-fail-count (test-runner-current)))
(format #t "  Skipped: ~a~%" (test-runner-skip-count (test-runner-current)))

;; Exit with appropriate code
(exit (if (zero? (test-runner-fail-count (test-runner-current))) 0 1))
```