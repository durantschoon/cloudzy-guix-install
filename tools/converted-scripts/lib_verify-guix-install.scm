#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Guix Installation Verification Script
;;; Can be run from Guix ISO (checks /mnt) or from installed system (checks /)
;;;
;;; Usage:
;;;   From ISO:       ./verify-guix-install.scm
;;;   From installed: sudo /usr/local/bin/verify-guix-install.scm
;;;   From installed: sudo verify-guix-install  (if in PATH)

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 ftw)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-26))

;;; Configuration

(define *errors* 0)
(define *warnings* 0)

;; ANSI color codes
(define color-red "\x1b[0;31m")
(define color-green "\x1b[0;32m")
(define color-yellow "\x1b[1;33m")
(define color-blue "\x1b[0;34m")
(define color-reset "\x1b[0m")

;;; Helper Functions - Context Detection

(define (mountpoint-q? path)
  "Check if PATH is a mountpoint."
  (let* ((cmd (format #f "mountpoint -q ~s" path))
         (status (system cmd)))
    (zero? status)))

(define (determine-root-context)
  "Determine if we're checking /mnt (from ISO) or / (from installed system).
Returns (root-path . context-description) pair."
  (if (and (mountpoint-q? "/mnt")
           (file-exists? "/mnt/boot"))
      (cons "/mnt" "Guix ISO (checking /mnt)")
      (cons "" "Installed System (checking /)")))

;;; Helper Functions - Output and Counters

(define (error-msg msg . args)
  "Print error message and increment error counter."
  (format #t "~a[ERROR]~a ~?~%" color-red color-reset msg args)
  (set! *errors* (+ *errors* 1)))

(define (warn-msg msg . args)
  "Print warning message and increment warning counter."
  (format #t "~a[WARN]~a ~?~%" color-yellow color-reset msg args)
  (set! *warnings* (+ *warnings* 1)))

(define (ok-msg msg . args)
  "Print success message."
  (format #t "~a[OK]~a ~?~%" color-green color-reset msg args))

(define (info-msg msg . args)
  "Print informational message."
  (format #t "~a[INFO]~a ~?~%" color-blue color-reset msg args))

;;; Helper Functions - File System Operations

(define (get-file-size filepath)
  "Get file size in bytes. Returns #f if file doesn't exist."
  (catch #t
    (lambda ()
      (let ((info (stat filepath)))
        (stat:size info)))
    (lambda (key . args) #f)))

(define (format-size-mb size)
  "Format size in bytes as megabytes with 1 decimal place."
  (if size
      (format #f "~,1f MB" (/ size 1024.0 1024.0))
      "? MB"))

(define (get-directory-count dirpath)
  "Count number of items in directory. Returns 0 if directory doesn't exist."
  (catch #t
    (lambda ()
      (let ((entries (scandir dirpath)))
        (if entries
            ;; Subtract 2 for . and ..
            (max 0 (- (length entries) 2))
            0)))
    (lambda (key . args) 0)))

;;; Helper Functions - Command Execution

(define (run-command cmd)
  "Execute shell command and return output as string. Returns #f on error."
  (catch #t
    (lambda ()
      (let* ((port (open-input-pipe cmd))
             (output (read-string port))
             (status (close-pipe port)))
        (if (zero? status)
            (string-trim-right output #\newline)
            #f)))
    (lambda (key . args) #f)))

(define (command-exists? cmd)
  "Check if command exists in PATH."
  (let ((result (run-command (format #f "command -v ~a" cmd))))
    (and result (not (string-null? result)))))

;;; Helper Functions - File Checks

(define (check-file root-path filepath description critical?)
  "Check if file exists and report its size.
Returns #t if file exists, #f otherwise."
  (let ((full-path (string-append root-path filepath)))
    (if (file-exists? full-path)
        (let* ((size (get-file-size full-path))
               (size-str (format-size-mb size)))
          (ok-msg "~a: ~a (~a)" description filepath size-str)
          #t)
        (begin
          (if critical?
              (error-msg "~a: ~a NOT FOUND (CRITICAL)" description filepath)
              (warn-msg "~a: ~a NOT FOUND" description filepath))
          #f))))

(define (check-pattern root-path pattern description critical?)
  "Check if files matching glob pattern exist and report first match.
Returns #t if any match found, #f otherwise."
  (let* ((full-pattern (string-append root-path pattern))
         (dir (dirname full-pattern))
         (base-pattern (basename full-pattern)))
    (catch #t
      (lambda ()
        (let ((entries (scandir dir 
                                (lambda (entry)
                                  ;; Simple glob matching for * patterns
                                  (let ((regex-pattern 
                                         (regexp-substitute/global 
                                          #f "\\*" base-pattern 
                                          'pre ".*" 'post)))
                                    (string-match regex-pattern entry))))))
          (if (and entries (not (null? entries)))
              (let* ((first-file (car entries))
                     (full-path (string-append dir "/" first-file))
                     (size (get-file-size full-path))
                     (size-str (format-size-mb size)))
                (ok-msg "~a: ~a (~a)" description first-file size-str)
                (when (> (length entries) 1)
                  (info-msg "  Found ~a files matching pattern" (length entries)))
                #t)
              (begin
                (if critical?
                    (error-msg "~a NOT FOUND: ~a (CRITICAL)" description pattern)
                    (warn-msg "~a NOT FOUND: ~a" description pattern))
                #f))))
      (lambda (key . args)
        (if critical?
            (error-msg "~a NOT FOUND: ~a (CRITICAL)" description pattern)
            (warn-msg "~a NOT FOUND: ~a" description pattern))
        #f))))

(define (check-dir root-path dirpath description)
  "Check if directory exists.
Returns #t if directory exists, #f otherwise."
  (let ((full-path (string-append root-path dirpath)))
    (if (file-exists? full-path)
        (begin
          (ok-msg "~a: ~a" description dirpath)
          #t)
        (begin
          (error-msg "~a: ~a NOT FOUND" description dirpath)
          #f))))

;;; Helper Functions - Configuration Parsing

(define (extract-config-value root-path pattern label)
  "Extract a configuration value from config.scm using grep pattern."
  (let* ((config-path (string-append root-path "/etc/config.scm"))
         (cmd (format #f "grep ~s ~s | head -1" pattern config-path))
         (result (run-command cmd)))
    (if result
        (let ((match (string-match "\"([^\"]*)\"" result)))
          (if match
              (begin
                (info-msg "  ~a: ~a" label (match:substring match 1))
                #t)
              #f))
        #f)))

(define (check-kernel-args root-path)
  "Check for problematic kernel arguments in config.scm."
  (let* ((config-path (string-append root-path "/etc/config.scm"))
         (cmd (format #f "grep 'kernel-arguments' ~s | head -1" config-path))
         (result (run-command cmd)))
    (when result
      (info-msg "  Kernel args: ~a" result)
      (when (or (string-contains result "acpi=off")
                (string-contains result "noapic")
                (string-contains result "nolapic"))
        (warn-msg "  Aggressive kernel parameters detected (may cause boot issues)")))))

;;; Verification Sections

(define (check-critical-boot-files root-path)
  "Check critical boot files (kernel, initrd, GRUB config)."
  (display "\n=== Critical Boot Files ===\n\n")
  
  ;; Kernel
  (check-pattern root-path "/boot/vmlinuz-*" "Kernel" #t)
  
  ;; Initrd
  (check-pattern root-path "/boot/initrd-*" "Initrd" #t)
  
  ;; GRUB config
  (check-file root-path "/boot/grub/grub.cfg" "GRUB config" #t))

(define (check-efi-boot-files root-path)
  "Check EFI boot files."
  (display "\n=== EFI Boot Files ===\n\n")
  
  (let ((efi-dir (string-append root-path "/boot/efi/EFI")))
    (if (file-exists? efi-dir)
        (begin
          (ok-msg "EFI partition mounted at /boot/efi")
          
          ;; Check GRUB EFI binary (could be in different locations)
          (cond
           ((file-exists? (string-append root-path "/boot/efi/EFI/Guix/grubx64.efi"))
            (check-file root-path "/boot/efi/EFI/Guix/grubx64.efi" 
                       "GRUB EFI binary (Guix)" #t))
           ((file-exists? (string-append root-path "/boot/efi/EFI/BOOT/BOOTX64.EFI"))
            (check-file root-path "/boot/efi/EFI/BOOT/BOOTX64.EFI" 
                       "GRUB EFI binary (fallback)" #t))
           (else
            (error-msg "GRUB EFI binary NOT FOUND in /boot/efi/EFI/")))
          
          ;; GRUB EFI config
          (check-file root-path "/boot/efi/EFI/Guix/grub.cfg" "GRUB EFI config" #f))
        (error-msg "EFI partition not mounted at /boot/efi"))))

(define (check-system-configuration root-path)
  "Check system configuration file and parse basic info."
  (display "\n=== System Configuration ===\n\n")
  
  (when (check-file root-path "/etc/config.scm" "System configuration" #t)
    ;; Parse hostname
    (extract-config-value root-path "host-name" "Hostname")
    
    ;; Parse timezone
    (extract-config-value root-path "timezone" "Timezone")
    
    ;; Check kernel arguments
    (check-kernel-args root-path)))

(define (check-user-accounts root-path)
  "Check user accounts and home directories."
  (display "\n=== User Accounts ===\n\n")
  
  (let ((passwd-path (string-append root-path "/etc/passwd")))
    (if (file-exists? passwd-path)
        (begin
          (ok-msg "Password file exists")
          
          ;; List non-system users
          (let* ((cmd (format #f "grep -E ':/home/.*:(bash|sh)' ~s | cut -d: -f1" 
                             passwd-path))
                 (users-output (run-command cmd)))
            (if (and users-output (not (string-null? users-output)))
                (let ((users (string-split users-output #\newline)))
                  (for-each
                   (lambda (user)
                     (unless (string-null? user)
                       (ok-msg "  User: ~a" user)
                       (let ((home-path (string-append root-path "/home/" user)))
                         (if (file-exists? home-path)
                             (info-msg "    Home: /home/~a exists" user)
                             (warn-msg "    Home: /home/~a NOT FOUND" user)))))
                   users))
                (begin
                  (warn-msg "No regular user accounts found (only system accounts)")
                  (info-msg "  You may need to set a password: guix system chroot /mnt && passwd USERNAME")))))
        (error-msg "Password file /etc/passwd NOT FOUND"))))

(define (check-gnu-store root-path)
  "Check GNU store population."
  (display "\n=== GNU Store ===\n\n")
  
  (let ((store-path (string-append root-path "/gnu/store")))
    (if (file-exists? store-path)
        (let ((count (get-directory-count store-path)))
          (cond
           ((> count 100)
            (ok-msg "GNU store populated: ~a items" count))
           ((> count 0)
            (warn-msg "GNU store has only ~a items (seems low)" count))
           (else
            (error-msg "GNU store is empty"))))
        (error-msg "GNU store directory NOT FOUND"))))

(define (check-filesystem-mounts root-path)
  "Check filesystem mount labels."
  (display "\n=== Filesystem Mounts ===\n\n")
  
  (when (command-exists? "blkid")
    ;; Check GUIX_ROOT label
    (let ((guix-dev (run-command "blkid -t LABEL=GUIX_ROOT -o device")))
      (if guix-dev
          (ok-msg "GUIX_ROOT label found: ~a" guix-dev)
          (warn-msg "GUIX_ROOT label not found (may need to be set)")))
    
    ;; Check EFI label
    (let ((efi-dev (run-command "blkid -t LABEL=EFI -o device")))
      (if efi-dev
          (ok-msg "EFI label found: ~a" efi-dev)
          (warn-msg "EFI label not found (may need to be set)")))))

(define (check-disk-space root-path)
  "Check available disk space."
  (display "\n=== Disk Space ===\n\n")
  
  (when (command-exists? "df")
    (let* ((mount-point (if (string-null? root-path) "/" root-path))
           (cmd (format #f "df -BG ~s | tail -1 | awk '{print $4}' | sed 's/G//'" 
                       mount-point))
           (avail-str (run-command cmd)))
      (when avail-str
        (catch #t
          (lambda ()
            (let ((avail (string->number avail-str)))
              (when avail
                (if (> avail 5)
                    (ok-msg "Available space on root: ~aG" avail)
                    (warn-msg "Low disk space on root: ~aG" avail)))))
          (lambda (key . args) #f))))))

;;; Main Verification Logic

(define (print-header root-path context)
  "Print verification header."
  (display "\n")
  (display "==========================================\n")
  (display "  Guix Installation Verification\n")
  (display "==========================================\n")
  (display "\n")
  (format #t "Context: ~a~%" context)
  (format #t "Checking: ~a~%" (if (string-null? root-path) "/" root-path))
  (display "\n"))

(define (print-summary)
  "Print verification summary and return exit code."
  (display "\n")
  (display "==========================================\n")
  (display "  Verification Summary\n")
  (display "==========================================\n")
  (display "\n")
  
  (cond
   ((and (zero? *errors*) (zero? *warnings*))
    (format #t "~a✓ ALL CHECKS PASSED~a~%" color-green color-reset)
    (display "\n")
    (display "Installation appears complete and healthy.\n")
    (display "System should be ready to boot.\n")
    0)
   
   ((zero? *errors*)
    (format #t "~a⚠ PASSED WITH WARNINGS~a~%" color-yellow color-reset)
    (display "\n")
    (format #t "Errors: ~a~%" *errors*)
    (format #t "Warnings: ~a~%" *warnings*)
    (display "\n")
    (display "Installation is likely OK, but review warnings above.\n")
    0)
   
   (else
    (format #t "~a✗ VERIFICATION FAILED~a~%" color-red color-reset)
    (display "\n")
    (format #t "Errors: ~a~%" *errors*)
    (format #t "Warnings: ~a~%" *warnings*)
    (display "\n")
    (display "CRITICAL ISSUES DETECTED!\n")
    (display "DO NOT REBOOT until errors are resolved.\n")
    (display "\n")
    (display "Common fixes:\n")
    (display "  - Re-run: guix system init /mnt/etc/config.scm /mnt\n")
    (display "  - Set user password: guix system chroot /mnt && passwd USERNAME\n")
    (display "  - Check disk space: df -h /mnt\n")
    (display "  - Review installation logs\n")
    (display "\n")
    1)))

(define (main args)
  "Main entry point for verification script."
  ;; Reset counters
  (set! *errors* 0)
  (set! *warnings* 0)
  
  ;; Determine context
  (let* ((context-pair (determine-root-context))
         (root-path (car context-pair))
         (context (cdr context-pair)))
    
    ;; Print header
    (print-header root-path context)
    
    ;; Run all checks
    (check-critical-boot-files root-path)
    (check-efi-boot-files root-path)
    (check-system-configuration root-path)
    (check-user-accounts root-path)
    (check-gnu-store root-path)
    (check-filesystem-mounts root-path)
    (check-disk-space root-path)
    
    ;; Print summary and exit
    (exit (print-summary))))

;; Run main if executed as script
(when (batch-mode?)
  (main (command-line)))
