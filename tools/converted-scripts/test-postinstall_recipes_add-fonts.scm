```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Tests for add-fonts.scm

(use-modules (srfi srfi-64)   ; Testing framework
             (ice-9 textual-ports)
             (ice-9 format))

;;; Load the script being tested
(load "add-fonts.scm")

;;; Test Setup

(define test-dir #f)
(define test-config #f)

(define (setup-test-env)
  "Create temporary directory and test config file"
  (set! test-dir (tmpnam))
  (system (format #f "mkdir -p ~a" test-dir))
  (set! test-config (string-append test-dir "/config.scm"))
  (setenv "CONFIG_FILE" test-config))

(define (teardown-test-env)
  "Clean up temporary test files"
  (when (and test-dir (file-exists? test-dir))
    (system (format #f "rm -rf ~a" test-dir))))

(define (write-test-config content)
  "Write test configuration content"
  (call-with-output-file test-config
    (lambda (port)
      (put-string port content))))

(define (read-test-config)
  "Read test configuration content"
  (call-with-input-file test-config get-string-all))

;;; Test Suite

(test-begin "add-fonts")

;;; Test: Helper Functions

(test-group "helper-functions"
  
  (test-assert "file-contains? with existing pattern"
    (setup-test-env)
    (write-test-config "(packages %base-packages)")
    (let ((result (file-contains? test-config "%base-packages")))
      (teardown-test-env)
      result))
  
  (test-assert "file-contains? with missing pattern"
    (setup-test-env)
    (write-test-config "(packages %base-packages)")
    (let ((result (not (file-contains? test-config "font-fira-code"))))
      (teardown-test-env)
      result))
  
  (test-equal "read-file returns content"
    "(packages %base-packages)"
    (begin
      (setup-test-env)
      (write-test-config "(packages %base-packages)")
      (let ((result (read-file test-config)))
        (teardown-test-env)
        result))))

;;; Test: Package List Building

(test-group "package-list-building"
  
  (test-assert "build-package-list-string creates valid list"
    (let ((result (build-package-list-string '("font-fira-code" "font-dejavu"))))
      (and (string-contains result "font-fira-code")
           (string-contains result "font-dejavu")
           (string-contains result "specification->package")))))

;;; Test: Minimal Config Conversion

(test-group "minimal-config"
  
  (test-assert "add-fonts-to-minimal-config converts %base-packages"
    (let* ((content "(packages %base-packages)")
           (result (add-fonts-to-minimal-config content)))
      (and (string-contains result "(packages")
           (string-contains result "append")
           (string-contains result "font-fira-code")
           (string-contains result "font-jetbrains-mono")
           (not (string-contains result "(packages %base-packages)")))))
  
  (test-assert "converted config has all fonts"
    (let* ((content "(packages %base-packages)")
           (result (add-fonts-to-minimal-config content)))
      (every (lambda (font)
               (string-contains result font))
             font-packages))))

;;; Test: Existing Config Updates

(test-group "existing-config"
  
  (test-assert "add-font-to-existing-config adds new font"
    (let* ((content "(specification->package \"some-package\")")
           (result (add-font-to-existing-config content "font-fira-code")))
      (and (string-contains result "font-fira-code")
           (string-contains result "some-package"))))
  
  (test-assert "add-font-to-existing-config skips existing font"
    (let* ((content "(specification->package \"font-fira-code\")")
           (result (add-font-to-existing-config content "font-fira-code"))
           ;; Count occurrences - should only be 1
           (count (let loop ((str result) (count 0))
                    (let ((pos (string-contains str "font-fira-code")))
                      (if pos
                          (loop (substring str (+ pos 14)) (+ count 1))
                          count)))))
      (= count 1)))
  
  (test-assert "add-fonts-to-existing-config adds all fonts"
    (let* ((content "(specification->package \"some-package\")")
           (result (add-fonts-to-existing-config content)))
      (every (lambda (font)
               (string-contains result font))
             font-packages))))

;;; Test: Full Integration

(test-group "integration"
  
  (test-assert "add-fonts updates minimal config file"
    (setup-test-env)
    (write-test-config "(packages %base-packages)")
    (add-fonts)
    (let* ((result (read-test-config))
           (success (every (lambda (font)
                            (string-contains result font))
                          font-packages)))
      (teardown-test-env)
      success))
  
  (test-assert "add-fonts updates existing config file"
    (setup-test-env)
    (write-test-config "(packages (list (specification->package \"some-package\")))")
    (add-fonts)
    (let* ((result (read-test-config))
           (success (and (string-contains result "some-package")
                        (every (lambda (font)
                                 (string-contains result font))
                               font-packages))))
      (teardown-test-env)
      success))
  
  (test-assert "add-fonts is idempotent"
    (setup-test-env)
    (write-test-config "(packages %base-packages)")
    (add-fonts)
    (let ((first-result (read-test-config)))
      (add-fonts)
      (let* ((second-result (read-test-config))
             ;; Count should be same - fonts not duplicated
             (count-font (lambda (content)
                          (let loop ((str content) (count 0))
                            (let ((pos (string-contains str "font-fira-code")))
                              (if pos
                                  (loop (substring str (+ pos 14)) (+ count 1))
                                  count))))))
        (let ((success (= (count-font first-result)
                         (count-font second-result))))
          (teardown-test-env)
          success)))))

(test-end "add-fonts")

;;; Run tests when executed directly
(when (batch-mode?)
  (let ((runner (test-runner-simple)))
    (test-runner-current runner)
    (exit (= (test-runner-fail-count runner) 0))))
```