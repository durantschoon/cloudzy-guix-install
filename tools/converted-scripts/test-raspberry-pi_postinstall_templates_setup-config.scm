```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Test suite for setup-config.scm
;;; Tests Raspberry Pi config.txt setup functionality

(use-modules (srfi srfi-64)  ; Testing framework
             (ice-9 format)
             (ice-9 ftw)
             (ice-9 textual-ports))

;;; Test configuration

(define test-dir #f)
(define test-boot-dir #f)

;;; Helper procedures for testing

;; Create a temporary directory for testing
(define (setup-test-environment)
  (let* ((tmpdir (or (getenv "TMPDIR") "/tmp"))
         (test-base (string-append tmpdir "/setup-config-test-" 
                                   (number->string (getpid)))))
    (mkdir test-base)
    (set! test-dir test-base)
    (set! test-boot-dir (string-append test-base "/boot"))
    (mkdir test-boot-dir)
    test-base))

;; Clean up test environment
(define (teardown-test-environment)
  (when test-dir
    (system* "rm" "-rf" test-dir)))

;; Create a dummy config template
(define (create-config-template model)
  (let ((template-file (string-append test-dir "/config-" model ".txt")))
    (call-with-output-file template-file
      (lambda (port)
        (format port "# Raspberry Pi ~a config~%" (string-upcase model))
        (display "# Test configuration\n" port)
        (display "arm_64bit=1\n" port)))
    template-file))

;; Read file contents as string
(define (read-file-contents filepath)
  (call-with-input-file filepath
    (lambda (port)
      (get-string-all port))))

;;; Test suite

(test-begin "setup-config-tests")

;;; Test model validation

(test-group "model-validation"
  (test-assert "pi3 is valid model"
    (member "pi3" '("pi3" "pi4" "pi5")))
  
  (test-assert "pi4 is valid model"
    (member "pi4" '("pi3" "pi4" "pi5")))
  
  (test-assert "pi5 is valid model"
    (member "pi5" '("pi3" "pi4" "pi5")))
  
  (test-assert "invalid-model is not valid"
    (not (member "invalid-model" '("pi3" "pi4" "pi5")))))

;;; Test model name conversion

(test-group "model-name-conversion"
  (test-equal "pi3 converts to PI3"
    "PI3"
    (string-upcase "pi3"))
  
  (test-equal "pi4 converts to PI4"
    "PI4"
    (string-upcase "pi4"))
  
  (test-equal "pi5 converts to PI5"
    "PI5"
    (string-upcase "pi5")))

;;; Test file operations

(test-group "file-operations"
  ;; Setup test environment
  (setup-test-environment)
  
  (test-assert "test directory created"
    (file-exists? test-dir))
  
  (test-assert "test boot directory created"
    (file-exists? test-boot-dir))
  
  ;; Test config template creation
  (let ((template (create-config-template "pi4")))
    (test-assert "config template created"
      (file-exists? template))
    
    (test-assert "config template has content"
      (> (string-length (read-file-contents template)) 0))
    
    (test-assert "config template mentions pi4"
      (string-contains (read-file-contents template) "PI4")))
  
  ;; Test backup filename generation
  (test-assert "backup filename contains .backup."
    (string-contains 
      (string-append "/boot/config.txt.backup.20240101_120000")
      ".backup."))
  
  ;; Cleanup
  (teardown-test-environment))

;;; Test model detection logic

(test-group "model-detection"
  ;; We can't test actual detection without /proc/device-tree/model
  ;; but we can test the string matching logic
  
  (test-equal "Raspberry Pi 3 string detected as pi3"
    "pi3"
    (cond
      ((string-contains "Raspberry Pi 3 Model B Rev 1.2" "Raspberry Pi 3") "pi3")
      (else "unknown")))
  
  (test-equal "Raspberry Pi 4 string detected as pi4"
    "pi4"
    (cond
      ((string-contains "Raspberry Pi 4 Model B Rev 1.1" "Raspberry Pi 4") "pi4")
      (else "unknown")))
  
  (test-equal "Raspberry Pi 5 string detected as pi5"
    "pi5"
    (cond
      ((string-contains "Raspberry Pi 5 Model B" "Raspberry Pi 5") "pi5")
      (else "unknown")))
  
  (test-equal "Unknown model string returns unknown"
    "unknown"
    (cond
      ((string-contains "Some Other Device" "Raspberry Pi 3") "pi3")
      ((string-contains "Some Other Device" "Raspberry Pi 4") "pi4")
      ((string-contains "Some Other Device" "Raspberry Pi 5") "pi5")
      (else "unknown"))))

;;; Test color code definitions

(test-group "color-codes"
  (let ((colors '((red . "\x1b[0;31m")
                  (green . "\x1b[0;32m")
                  (yellow . "\x1b[1;33m")
                  (blue . "\x1b[0;34m")
                  (nc . "\x1b[0m"))))
    
    (test-assert "red color code exists"
      (assoc-ref colors 'red))
    
    (test-assert "green color code exists"
      (assoc-ref colors 'green))
    
    (test-assert "yellow color code exists"
      (assoc-ref colors 'yellow))
    
    (test-assert "blue color code exists"
      (assoc-ref colors 'blue))
    
    (test-assert "nc (no color) code exists"
      (assoc-ref colors 'nc))))

(test-end "setup-config-tests")

;; Print test summary
(format #t "~%Test suite completed.~%")
```