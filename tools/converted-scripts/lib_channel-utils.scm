# Converted Guile Script

```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Channel management utilities for Guix installations

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26))

;;; Configuration

(define *default-channels-content*
  "(cons* (channel
        (name 'nonguix)
        (url \"https://gitlab.com/nonguix/nonguix\")
        (branch \"master\")
        (introduction
         (make-channel-introduction
          \"897c1a470da759236cc11798f4e0a5f7d4d59fbc\"
          (openpgp-fingerprint
           \"2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5\"))))
       %default-channels)")

(define (get-channels-file)
  "Return the path to the user's channels.scm file"
  (string-append (getenv "HOME") "/.config/guix/channels.scm"))

(define (get-config-dir)
  "Return the path to the user's Guix config directory"
  (string-append (getenv "HOME") "/.config/guix"))

;;; Color output helpers

(define (color-string code text)
  "Wrap text in ANSI color codes"
  (string-append code text "\033[0m"))

(define (info . args)
  "Print info message in blue"
  (display (color-string "\033[0;34m" "[INFO] "))
  (for-each (lambda (arg) (display arg)) args)
  (newline))

(define (success . args)
  "Print success message in green"
  (display (color-string "\033[0;32m" "[OK] "))
  (for-each (lambda (arg) (display arg)) args)
  (newline))

(define (warn . args)
  "Print warning message in yellow"
  (display (color-string "\033[1;33m" "[WARN] "))
  (for-each (lambda (arg) (display arg)) args)
  (newline))

(define (error-msg . args)
  "Print error message in red"
  (display (color-string "\033[0;31m" "[ERROR] "))
  (for-each (lambda (arg) (display arg)) args)
  (newline))

;;; File and directory helpers

(define (ensure-directory path)
  "Create directory if it doesn't exist"
  (unless (file-exists? path)
    (mkdir path)))

(define (run-command cmd)
  "Execute shell command and return (output . status)"
  (let* ((port (open-input-pipe cmd))
         (output (read-string port))
         (status (close-pipe port)))
    (cons output status)))

(define (run-command-success? cmd)
  "Execute shell command and return #t if successful"
  (zero? (cdr (run-command cmd))))

;;; Show current channel status

(define (show-channel-status)
  "Display current channel configuration status"
  (newline)
  (display "=== Channel Status ===\n")
  
  (let ((channels-file (get-channels-file)))
    (if (file-exists? channels-file)
        (begin
          (format #t "Channels file: ~a\n" channels-file)
          (newline)
          (display "Current channel configuration:\n")
          (call-with-input-file channels-file
            (lambda (port)
              (display (read-string port))))
          (newline))
        (begin
          (warn "No channels.scm found - using default channels only")
          (format #t "Location: ~a\n" channels-file)))))

;;; Setup default channels

(define (setup-default-channels)
  "Configure default channels (nonguix + official)"
  (newline)
  (display "=== Setting up Default Channels ===\n")
  
  (let ((config-dir (get-config-dir)))
    (ensure-directory config-dir)
    
    (call-with-output-file (get-channels-file)
      (lambda (port)
        (display *default-channels-content* port)))
    
    (success "Default channels configured (nonguix + official)")
    (info "Channels file: " (get-channels-file))))

;;; Download channels from repository

(define (download-user-channels repo-url . rest)
  "Download channels.scm from a Git repository
   Arguments: repo-url [branch] [path]"
  (let ((branch (if (null? rest) "main" (car rest)))
        (path (if (or (null? rest) (null? (cdr rest)))
                  "channels/"
                  (cadr rest))))
    
    (newline)
    (display "=== Downloading Channels from Repository ===\n")
    (format #t "Repository: ~a\n" repo-url)
    (format #t "Branch: ~a\n" branch)
    (format #t "Path: ~a\n" path)
    (newline)
    
    (let ((temp-dir (string-append "/tmp/guix-channels-" (number->string (getpid)))))
      ;; Create temp directory
      (ensure-directory temp-dir)
      
      ;; Clone repository
      (info "Cloning channel repository...")
      (let ((clone-cmd (format #f "git clone --branch ~a --depth 1 ~a ~a 2>&1"
                               branch repo-url temp-dir)))
        (unless (run-command-success? clone-cmd)
          (error-msg "Failed to clone repository: " repo-url)
          (system* "rm" "-rf" temp-dir)
          (exit 1)))
      
      ;; Find channels.scm file
      (let* ((channels-path (string-append temp-dir "/" path "channels.scm"))
             (alt-paths (list (string-append temp-dir "/channels.scm")
                              (string-append temp-dir "/config/channels.scm")
                              (string-append temp-dir "/.config/guix/channels.scm")))
             (found-path (if (file-exists? channels-path)
                             channels-path
                             (find file-exists? alt-paths))))
        
        (if found-path
            (begin
              ;; Copy to user's config directory
              (let ((config-dir (get-config-dir)))
                (ensure-directory config-dir)
                
                (if (run-command-success?
                     (format #f "cp ~a ~a" found-path (get-channels-file)))
                    (begin
                      (success "Channels configured from: " repo-url)
                      (info "Channels file: " (get-channels-file))
                      
                      ;; Show the configuration
                      (newline)
                      (display "Channel configuration:\n")
                      (call-with-input-file (get-channels-file)
                        (lambda (port)
                          (display (read-string port)))))
                    (begin
                      (error-msg "Failed to copy channels.scm")
                      (system* "rm" "-rf" temp-dir)
                      (exit 1)))))
            (begin
              (error-msg "channels.scm not found in repository")
              (display "Tried locations:\n")
              (format #t "  - ~a\n" channels-path)
              (for-each (lambda (p) (format #t "  - ~a\n" p)) alt-paths)
              (system* "rm" "-rf" temp-dir)
              (exit 1))))
      
      ;; Clean up temp directory
      (system* "rm" "-rf" temp-dir))))

;;; Validate channel configuration

(define (validate-channels)
  "Validate the current channel configuration"
  (newline)
  (display "=== Validating Channel Configuration ===\n")
  
  (let ((channels-file (get-channels-file)))
    (if (not (file-exists? channels-file))
        (begin
          (warn "No channels.scm found - will use default channels")
          #t)
        (begin
          (info "Checking channels file: " channels-file)
          (newline)
          (display "Channel configuration:\n")
          (call-with-input-file channels-file
            (lambda (port)
              (display (read-string port))))
          (newline)
          
          ;; Try to validate with guix describe
          (info "Validating channel configuration...")
          (if (run-command-success? "guix describe --format=channels >/dev/null 2>&1")
              (begin
                (success "Channel configuration is valid")
                #t)
              (begin
                (warn "Channel validation failed - this is expected on ISO")
                (info "Channels will be validated during system init")
                #t))))))

;;; Interactive channel configuration

(define (interactive-setup)
  "Interactive channel configuration wizard"
  (newline)
  (display "=== Interactive Channel Configuration ===\n")
  (newline)
  (display "1) Use default channels (nonguix + official)\n")
  (display "2) Download channels from your Git repository\n")
  (display "3) Skip channel configuration\n")
  (newline)
  
  (display "Choose option [1-3]: ")
  (flush-output-port (current-output-port))
  (let ((choice (read-line)))
    (match choice
      ("1"
       (setup-default-channels))
      ("2"
       (display "Enter your channel repository URL: ")
       (flush-output-port (current-output-port))
       (let ((repo-url (read-line)))
         (display "Enter branch/tag [main]: ")
         (flush-output-port (current-output-port))
         (let ((branch (read-line)))
           (display "Enter path within repo [channels/]: ")
           (flush-output-port (current-output-port))
           (let ((path (read-line)))
             (download-user-channels
              repo-url
              (if (or (string=? branch "") (eof-object? branch)) "main" branch)
              (if (or (string=? path "") (eof-object? path)) "channels/" path))))))
      ("3"
       (info "Skipping channel configuration"))
      (_
       (error-msg "Invalid choice")
       (exit 1)))))

;;; Help message

(define (show-help)
  "Display help message"
  (display "Guix Channel Management Utilities\n")
  (newline)
  (display "Usage: channel-utils.scm <command> [args...]\n")
  (newline)
  (display "Commands:\n")
  (display "  status          Show current channel status\n")
  (display "  setup-default   Set up default channels (nonguix + official)\n")
  (display "  download <url> [branch] [path]  Download channels from Git repository\n")
  (display "  validate        Validate current channel configuration\n")
  (display "  interactive     Interactive channel setup\n")
  (display "  help            Show this help message\n")
  (newline)
  (display "Examples:\n")
  (display "  channel-utils.scm status\n")
  (display "  channel-utils.scm setup-default\n")
  (display "  channel-utils.scm download https://github.com/user/channels main config/\n")
  (display "  channel-utils.scm interactive\n"))

;;; Main entry point

(define (main args)
  "Main entry point for channel utilities"
  (if (< (length args) 2)
      (show-help)
      (let ((command (cadr args)))
        (match command
          ("status"
           (show-channel-status))
          ("setup-default"
           (setup-default-channels))
          ("download"
           (if (< (length args) 3)
               (begin
                 (error-msg "Usage: channel-utils.scm download <repo-url> [branch] [path]")
                 (exit 1))
               (apply download-user-channels (cddr args))))
          ("validate"
           (validate-channels))
          ("interactive"
           (interactive-setup))
          ("help"
           (show-help))
          (_
           (show-help))))))

;; Run main function
(main (command-line))
```