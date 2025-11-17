#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Setup config.txt for Raspberry Pi model
;;; Usage: ./setup-config.scm [pi3|pi4|pi5]

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1)
             (srfi srfi-19))  ; For date/time formatting

;;; Configuration

;; ANSI color codes for output
(define *colors*
  '((red . "\x1b[0;31m")
    (green . "\x1b[0;32m")
    (yellow . "\x1b[1;33m")
    (blue . "\x1b[0;34m")
    (nc . "\x1b[0m")))

(define (get-color name)
  (assoc-ref *colors* name))

;;; Helper functions

;; Print colored output
(define (print-status msg)
  (format #t "~a[INFO]~a ~a~%" 
          (get-color 'blue) 
          (get-color 'nc) 
          msg))

(define (print-success msg)
  (format #t "~a[SUCCESS]~a ~a~%" 
          (get-color 'green) 
          (get-color 'nc) 
          msg))

(define (print-warning msg)
  (format #t "~a[WARNING]~a ~a~%" 
          (get-color 'yellow) 
          (get-color 'nc) 
          msg))

(define (print-error msg)
  (format #t "~a[ERROR]~a ~a~%" 
          (get-color 'red) 
          (get-color 'nc) 
          msg))

;; Get the directory containing this script
(define (get-script-dir)
  (let ((script-path (car (command-line))))
    (dirname script-path)))

;; Show usage information
(define (show-usage program-name)
  (format #t "Usage: ~a [pi3|pi4|pi5]~%" program-name)
  (display "\n")
  (display "This script copies the appropriate config.txt template for your Raspberry Pi model.\n")
  (display "\n")
  (display "Arguments:\n")
  (display "  pi3    - Raspberry Pi 3 (BCM2837, 1GB RAM)\n")
  (display "  pi4    - Raspberry Pi 4 (BCM2711, 2GB/4GB/8GB RAM)\n")
  (display "  pi5    - Raspberry Pi 5 (BCM2712, 4GB/8GB RAM)\n")
  (display "\n")
  (display "Examples:\n")
  (format #t "  ~a pi4    # Setup for Raspberry Pi 4~%" program-name)
  (format #t "  ~a pi5    # Setup for Raspberry Pi 5~%" program-name))

;; Convert model name to uppercase for display
(define (model->uppercase model)
  (string-upcase model))

;;; Pi model detection

;; Detect Raspberry Pi model from device tree
(define (detect-pi-model)
  (let ((model-file "/proc/device-tree/model"))
    (if (file-exists? model-file)
        (let ((model-str (call-with-input-file model-file read-string)))
          (cond
            ((string-contains model-str "Raspberry Pi 3") "pi3")
            ((string-contains model-str "Raspberry Pi 4") "pi4")
            ((string-contains model-str "Raspberry Pi 5") "pi5")
            (else "unknown")))
        "unknown")))

;;; Config file operations

;; Generate backup filename with timestamp
(define (make-backup-filename target-file)
  (let* ((current-time (current-time))
         (time-str (strftime "%Y%m%d_%H%M%S" (localtime (time-second current-time)))))
    (string-append target-file ".backup." time-str)))

;; Copy config file for specified model
(define (copy-config model)
  (let* ((script-dir (get-script-dir))
         (source-file (string-append script-dir "/config-" model ".txt"))
         (target-file "/boot/config.txt"))
    
    ;; Validate source file exists
    (unless (file-exists? source-file)
      (print-error (format #f "Config template not found: ~a" source-file))
      (exit 1))
    
    ;; Validate boot directory exists
    (unless (file-exists? "/boot")
      (print-error "Boot directory not found. Are you running this on a Raspberry Pi?")
      (exit 1))
    
    ;; Backup existing config.txt if it exists
    (when (file-exists? target-file)
      (let ((backup-file (make-backup-filename target-file)))
        (print-status (format #f "Backing up existing config.txt to ~a" backup-file))
        (copy-file target-file backup-file)))
    
    ;; Copy new config
    (print-status (format #f "Copying config for Raspberry Pi ~a..." 
                          (model->uppercase model)))
    (copy-file source-file target-file)
    
    (print-success (format #f "Config.txt updated for Raspberry Pi ~a" 
                           (model->uppercase model)))
    (print-status "You may need to reboot for changes to take effect")))

;;; Argument validation

;; Check if model is valid
(define (valid-model? model)
  (member model '("pi3" "pi4" "pi5")))

;; Parse and validate command line arguments
(define (parse-args args)
  (let ((argc (length args)))
    (cond
      ;; One argument provided
      ((= argc 1)
       (let ((arg (car args)))
         (cond
           ((valid-model? arg) arg)
           ((member arg '("-h" "--help" "help"))
            (show-usage (car (command-line)))
            (exit 0))
           (else
            (print-error (format #f "Invalid model: ~a" arg))
            (show-usage (car (command-line)))
            (exit 1)))))
      
      ;; No arguments - try auto-detect
      ((= argc 0)
       (print-status "No model specified, attempting to auto-detect...")
       (let ((detected-model (detect-pi-model)))
         (if (string=? detected-model "unknown")
             (begin
               (print-warning "Could not auto-detect Raspberry Pi model")
               (display "\n")
               (show-usage (car (command-line)))
               (exit 1))
             (begin
               (print-status (format #f "Detected Raspberry Pi model: ~a" 
                                     (model->uppercase detected-model)))
               detected-model))))
      
      ;; Too many arguments
      (else
       (print-error "Too many arguments")
       (show-usage (car (command-line)))
       (exit 1)))))

;;; Main logic

;; Main entry point
(define (main args)
  ;; Remove program name from args
  (let ((cli-args (cdr args)))
    (let ((model (parse-args cli-args)))
      (copy-config model))))

;; Run main function
(main (command-line))
