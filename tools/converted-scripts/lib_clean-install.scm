# Converted Guile Script

```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Clean Install Script
;;; Removes all installer artifacts to prepare for a fresh installation
;;; Run this from the Guix ISO before re-running the installer

(use-modules (ice-9 format)
             (ice-9 rdelim)
             (srfi srfi-1))

;;; Configuration

(define files-to-remove
  '("/mnt/etc/config.scm"
    "/tmp/channels.scm"
    "/root/bootstrap-installer.sh"
    "/root/bootstrap.sh"
    "/root/run-remote-steps"
    "/root/guix-init-time-machine.sh"
    "/root/recovery-complete-install.sh"
    "/tmp/guix-install.log"))

(define file-descriptions
  '(("/mnt/etc/config.scm" . "Configuration Files")
    ("/tmp/channels.scm" . "Configuration Files")
    ("/root/bootstrap-installer.sh" . "Bootstrap Scripts")
    ("/root/bootstrap.sh" . "Bootstrap Scripts")
    ("/root/run-remote-steps" . "Compiled Binaries and Helper Scripts")
    ("/root/guix-init-time-machine.sh" . "Compiled Binaries and Helper Scripts")
    ("/root/recovery-complete-install.sh" . "Compiled Binaries and Helper Scripts")
    ("/tmp/guix-install.log" . "Installation Logs")))

;;; Helper Functions

(define (file-exists-and-removable? filepath)
  "Check if file exists and is a regular file."
  (and (file-exists? filepath)
       (eq? 'regular (stat:type (stat filepath)))))

(define (remove-if-exists filepath)
  "Remove file if it exists. Returns 'removed, 'not-found, or 'error."
  (catch #t
    (lambda ()
      (if (file-exists-and-removable? filepath)
          (begin
            (delete-file filepath)
            'removed)
          'not-found))
    (lambda (key . args)
      'error)))

(define (read-confirmation)
  "Read user confirmation from stdin. Returns #t for yes, #f for no."
  (let ((line (read-line (current-input-port))))
    (if (eof-object? line)
        #f
        (let ((response (string-downcase (string-trim-both line))))
          (or (string=? response "y")
              (string=? response "yes"))))))

(define (group-files-by-category files descriptions)
  "Group files by their category based on descriptions."
  (let ((categories '()))
    (for-each
     (lambda (file)
       (let* ((desc-pair (assoc file descriptions))
              (category (if desc-pair (cdr desc-pair) "Other")))
         (let ((existing (assoc category categories)))
           (if existing
               (set-cdr! existing (cons file (cdr existing)))
               (set! categories (cons (cons category (list file)) categories))))))
     files)
    ;; Reverse file lists to maintain original order
    (map (lambda (cat-pair)
           (cons (car cat-pair) (reverse (cdr cat-pair))))
         (reverse categories))))

;;; Display Functions

(define (display-header)
  "Display the script header and warnings."
  (display "=== Guix Installer Cleanup ===\n")
  (newline)
  (display "This script will remove all installer artifacts to prepare for a clean reinstall.\n")
  (newline)
  (display "Files that will be removed:\n")
  (display "  - /mnt/etc/config.scm (generated configuration)\n")
  (display "  - /tmp/channels.scm (nonguix channel configuration)\n")
  (display "  - /root/bootstrap-installer.sh (downloaded bootstrap script)\n")
  (display "  - /root/bootstrap.sh (alternative name, if present)\n")
  (display "  - /root/run-remote-steps (compiled installer binary)\n")
  (display "  - /root/guix-init-time-machine.sh (helper script)\n")
  (display "  - /root/recovery-complete-install.sh (recovery script)\n")
  (display "  - /tmp/guix-install.log (installation log)\n")
  (newline)
  (display "WARNING: This will NOT remove:\n")
  (display "  - Partitions or filesystem data\n")
  (display "  - /mnt/gnu/store/ (already downloaded packages)\n")
  (display "  - /mnt/boot/ files (if any exist)\n")
  (newline))

(define (display-category-header category)
  "Display a category header."
  (format #t "=== ~a ===~%" category))

(define (display-file-result filepath status)
  "Display the result of attempting to remove a file."
  (case status
    ((removed) (format #t "[REMOVED] ~a~%" filepath))
    ((not-found) (format #t "[NOT FOUND] ~a~%" filepath))
    ((error) (format #t "[ERROR] Could not remove ~a~%" filepath))))

(define (display-summary removed not-found errors)
  "Display cleanup summary."
  (newline)
  (display "=== Cleanup Summary ===\n")
  (format #t "Removed: ~a files~%" (length removed))
  (format #t "Not found: ~a files~%" (length not-found))
  (when (> (length errors) 0)
    (format #t "Errors: ~a files~%" (length errors)))
  (newline)
  
  (when (> (length removed) 0)
    (display "Files removed:\n")
    (for-each
     (lambda (file)
       (format #t "  - ~a~%" file))
     removed)
    (newline)))

(define (display-next-steps)
  "Display next steps after cleanup."
  (display "[OK] Cleanup complete!\n")
  (newline)
  (display "Next steps:\n")
  (display "  1. Download the latest bootstrap installer:\n")
  (display "     curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/lib/bootstrap-installer.sh -o bootstrap-installer.sh\n")
  (newline)
  (display "  2. Run the installer with your platform:\n")
  (display "     bash bootstrap-installer.sh framework-dual\n")
  (newline)
  (display "  3. The installer will download fresh source code and compile a new binary\n")
  (newline))

;;; Main Logic

(define (cleanup-files files descriptions)
  "Remove all installer files and return results grouped by status."
  (let ((removed '())
        (not-found '())
        (errors '()))
    
    ;; Group files by category for organized display
    (let ((grouped (group-files-by-category files descriptions)))
      (for-each
       (lambda (category-pair)
         (let ((category (car category-pair))
               (category-files (cdr category-pair)))
           (display-category-header category)
           (for-each
            (lambda (filepath)
              (let ((status (remove-if-exists filepath)))
                (display-file-result filepath status)
                (case status
                  ((removed) (set! removed (cons filepath removed)))
                  ((not-found) (set! not-found (cons filepath not-found)))
                  ((error) (set! errors (cons filepath errors))))))
            category-files)
           (newline)))
       grouped))
    
    (values (reverse removed) (reverse not-found) (reverse errors))))

(define (main args)
  "Main entry point for the cleanup script."
  ;; Display header and get confirmation
  (display-header)
  (display "Continue with cleanup? [y/N] ")
  (force-output)
  
  (if (not (read-confirmation))
      (begin
        (newline)
        (display "Cleanup cancelled.\n")
        (exit 0))
      (begin
        (newline)
        (display "Starting cleanup...\n")
        (newline)
        
        ;; Perform cleanup
        (let-values (((removed not-found errors)
                      (cleanup-files files-to-remove file-descriptions)))
          
          ;; Display summary
          (display-summary removed not-found errors)
          
          ;; Display next steps
          (display-next-steps)
          
          ;; Exit with appropriate status
          (exit (if (null? errors) 0 1))))))

;;; Script Entry Point

(when (batch-mode?)
  (main (command-line)))
```