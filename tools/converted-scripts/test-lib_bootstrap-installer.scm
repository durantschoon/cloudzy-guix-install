```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Test suite for bootstrap-installer.scm

(use-modules (srfi srfi-64)
             (ice-9 ftw)
             (ice-9 format))

;;; Test Configuration

(test-begin "bootstrap-installer")

;;; Helper Functions Tests

(test-group "parse-arguments"
  (load "../bootstrap-installer.scm")
  
  (test-equal "parse empty args"
    '((channel-repo . "")
      (channel-branch . "main")
      (channel-path . "")
      (platform . ""))
    (parse-arguments '()))
  
  (test-equal "parse channel-repo"
    '((channel-repo . "test/repo")
      (channel-branch . "main")
      (channel-path . "")
      (platform . ""))
    (parse-arguments '("--channel-repo" "test/repo")))
  
  (test-equal "parse channel-branch"
    '((channel-repo . "")
      (channel-branch . "develop")
      (channel-path . "")
      (platform . ""))
    (parse-arguments '("--channel-branch" "develop")))
  
  (test-equal "parse platform"
    '((channel-repo . "")
      (channel-branch . "main")
      (channel-path . "")
      (platform . "framework-dual"))
    (parse-arguments '("framework-dual")))
  
  (test-equal "parse mixed arguments"
    '((channel-repo . "my/repo")
      (channel-branch . "testing")
      (channel-path . "/path/to/channel")
      (platform . "laptop"))
    (parse-arguments '("--channel-repo" "my/repo" 
                       "--channel-branch" "testing"
                       "--channel-path" "/path/to/channel"
                       "laptop"))))

(test-group "file-helpers"
  (let ((test-file "/tmp/test-bootstrap-file.txt"))
    
    ;; Create test file
    (call-with-output-file test-file
      (lambda (port) (display "test content" port)))
    
    (test-assert "file-exists-and-readable? returns true for existing file"
      (file-exists-and-readable? test-file))
    
    (test-assert "file-exists-and-readable? returns false for non-existing file"
      (not (file-exists-and-readable? "/nonexistent/file")))
    
    ;; Clean up
    (when (file-exists? test-file)
      (delete-file test-file))))

(test-group "hash-calculation"
  (let ((test-file "/tmp/test-hash-file.txt"))
    
    ;; Create test file with known content
    (call-with-output-file test-file
      (lambda (port) (display "hello world\n" port)))
    
    (test-assert "calculate-sha256 returns non-empty string"
      (let ((hash (calculate-sha256 test-file)))
        (and (string? hash)
             (> (string-length hash) 0))))
    
    ;; Known hash for "hello world\n"
    (test-equal "calculate-sha256 returns correct hash"
      "a948904f2f0f479b8f8197694b30184b0d2ed1c1cd2a1ec0fb85d299a192a447"
      (calculate-sha256 test-file))
    
    ;; Clean up
    (when (file-exists? test-file)
      (delete-file test-file))))

(test-group "command-execution"
  (test-assert "run-command returns output and status"
    (let ((result (run-command "echo test")))
      (and (pair? result)
           (string? (car result))
           (number? (cdr result)))))
  
  (test-assert "run-command captures output"
    (let ((result (run-command "echo 'hello'")))
      (string-contains (car result) "hello")))
  
  (test-assert "run-command/check succeeds for valid command"
    (string? (run-command/check "echo test"))))

(test-group "manifest-verification"
  (let ((test-manifest "/tmp/test-manifest.txt")
        (test-file-1 "/tmp/test-file-1.txt")
        (test-file-2 "/tmp/test-file-2.txt"))
    
    ;; Create test files
    (call-with-output-file test-file-1
      (lambda (port) (display "content1" port)))
    (call-with-output-file test-file-2
      (lambda (port) (display "content2" port)))
    
    ;; Calculate their hashes
    (let ((hash1 (calculate-sha256 test-file-1))
          (hash2 (calculate-sha256 test-file-2)))
      
      ;; Create manifest
      (call-with-output-file test-manifest
        (lambda (port)
          (format port "# Test manifest~%")
          (format port "~a  ~a~%" hash1 test-file-1)
          (format port "~a  ~a~%" hash2 test-file-2)))
      
      ;; Test verification (should pass without error)
      (test-assert "verify-manifest-file succeeds with correct hashes"
        (begin
          (verify-manifest-file test-manifest)
          #t)))
    
    ;; Clean up
    (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
              (list test-manifest test-file-1 test-file-2))))

;;; Integration Tests

(test-group "go-cache-setup"
  (test-assert "setup-go-cache creates directories"
    (begin
      (setup-go-cache)
      (and (file-exists? "/tmp/go-cache")
           (file-exists? "/tmp/go-tmp"))))
  
  (test-equal "setup-go-cache sets environment variables"
    "/tmp/go-cache"
    (begin
      (setup-go-cache)
      (getenv "GOCACHE")))
  
  (test-equal "setup-go-cache sets GOTMPDIR"
    "/tmp/go-tmp"
    (begin
      (setup-go-cache)
      (getenv "GOTMPDIR"))))

;;; End of tests

(test-end "bootstrap-installer")

;; Print summary
(format #t "~%Test Summary:~%")
(format #t "  Tests run: ~a~%" (test-runner-test-count (test-runner-current)))
(format #t "  Tests passed: ~a~%" (test-runner-pass-count (test-runner-current)))
(format #t "  Tests failed: ~a~%" (test-runner-fail-count (test-runner-current)))

;; Exit with error code if any tests failed
(exit (zero? (test-runner-fail-count (test-runner-current))))
```