# Converted Guile Script

```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Recovery script to complete Guix installation after time-machine
;;; Use this when guix time-machine ran but didn't complete fully
;;;
;;; This script assumes:
;;; - Partitions are formatted and mounted at /mnt
;;; - /mnt/etc/config.scm exists
;;; - /tmp/channels.scm exists (for nonguix)
;;; - guix time-machine was run but may have failed or been interrupted

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1)
             (srfi srfi-26))

;;; Configuration

(define *mnt-root* "/mnt")
(define *mnt-boot-efi* "/mnt/boot/efi")
(define *config-path* "/mnt/etc/config.scm")
(define *channels-path* "/tmp/channels.scm")
(define *tmpdir* "/mnt/var/tmp")
(define *cache-dir* "/mnt/var/cache")
(define *repo-base* "https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main")

;;; Helper Functions

(define (display-banner)
  "Display the recovery script banner."
  (display "=== Guix Installation Recovery Script ===\n")
  (newline)
  (display "This script will:\n")
  (display "  1. Verify current installation state\n")
  (display "  2. Re-run guix system init if needed (with time-machine)\n")
  (display "  3. Set user password\n")
  (display "  4. Download customization tools\n")
  (display "  5. Configure dual-boot GRUB (if framework-dual)\n")
  (display "  6. Write installation receipt\n")
  (display "  7. Prepare for reboot\n")
  (newline))

(define (prompt-continue)
  "Prompt user to continue or abort."
  (display "Continue? [Y/n] ")
  (force-output)
  (let ((response (read-line)))
    (when (or (string-ci=? response "n")
              (string-ci=? response "no"))
      (display "Aborted.\n")
      (exit 0))))

(define (run-command cmd)
  "Execute command and return output string and exit status."
  (let* ((port (open-input-pipe cmd))
         (output (read-string port))
         (status (close-pipe port)))
    (values output status)))

(define (run-command-success? cmd)
  "Execute command and return #t if exit status is 0."
  (let-values (((output status) (run-command cmd)))
    (zero? status)))

(define (system* cmd)
  "Execute command and return #t if successful."
  (zero? (system cmd)))

(define (file-exists-in-dir? pattern dir)
  "Check if files matching glob pattern exist in directory."
  (let ((cmd (format #f "ls ~a~a 2>/dev/null" dir pattern)))
    (run-command-success? cmd)))

(define (get-first-matching-file pattern dir)
  "Get first filename matching pattern in directory."
  (let* ((cmd (format #f "ls ~a~a 2>/dev/null | head -1" dir pattern))
         (port (open-input-pipe cmd))
         (output (string-trim-right (read-string port)))
         (status (close-pipe port)))
    (if (and (zero? status) (not (string=? output "")))
        (basename output)
        #f)))

(define (mountpoint? path)
  "Check if path is a mount point."
  (run-command-success? (format #f "mountpoint -q ~a" path)))

(define (grep-config pattern)
  "Grep for pattern in config.scm and return first match."
  (let* ((cmd (format #f "grep -oP '~a' ~a | head -1" pattern *config-path*))
         (port (open-input-pipe cmd))
         (output (string-trim-right (read-string port)))
         (status (close-pipe port)))
    (if (and (zero? status) (not (string=? output "")))
        output
        #f)))

(define (chroot-command cmd)
  "Execute command in chroot environment."
  (format #f "chroot ~a /run/current-system/profile/bin/bash -c '~a'" *mnt-root* cmd))

(define (chroot-run cmd)
  "Execute command in chroot and return success status."
  (run-command-success? (chroot-command cmd)))

(define (mkdir-p path)
  "Create directory and parents if needed."
  (system* (format #f "mkdir -p ~a" path)))

(define (read-tty)
  "Read line from /dev/tty."
  (call-with-input-file "/dev/tty" read-line))

(define (prompt-user message)
  "Display prompt and read response from TTY."
  (display message)
  (force-output)
  (read-tty))

;;; Verification Functions

(define (check-guix-iso)
  "Verify we're running on Guix ISO."
  (unless (file-exists? "/run/current-system/profile/bin/guix")
    (display "[ERROR] Not running on Guix ISO - cannot continue\n")
    (exit 1)))

(define (verify-mounts)
  "Verify required filesystems are mounted."
  (display "\n=== Verifying Mounts ===\n")
  
  ;; Check root mount
  (unless (mountpoint? *mnt-root*)
    (display "[ERROR] /mnt is not mounted!\n")
    (display "Please run the mount step first:\n")
    (display "  mount LABEL=GUIX_ROOT /mnt\n")
    (display "  mkdir -p /mnt/boot/efi\n")
    (display "  mount LABEL=EFI /mnt/boot/efi\n")
    (exit 1))
  (display "[OK] /mnt is mounted\n")
  
  ;; Check EFI mount
  (unless (mountpoint? *mnt-boot-efi*)
    (display "[ERROR] /mnt/boot/efi is not mounted!\n")
    (display "Please mount EFI:\n")
    (display "  mkdir -p /mnt/boot/efi\n")
    (display "  mount LABEL=EFI /mnt/boot/efi\n")
    (exit 1))
  (display "[OK] /mnt/boot/efi is mounted\n"))

(define (verify-config-exists)
  "Verify config.scm exists."
  (unless (file-exists? *config-path*)
    (display "[ERROR] /mnt/etc/config.scm not found!\n")
    (display "Please generate config first (step 3)\n")
    (exit 1))
  (display "[OK] Config exists: /mnt/etc/config.scm\n"))

(define (check-channels)
  "Check if channels.scm exists and return #t if time-machine should be used."
  (if (file-exists? *channels-path*)
      (begin
        (format #t "[OK] Channels file exists: ~a\n" *channels-path*)
        #t)
      (begin
        (display "[WARN] No channels.scm - using plain guix system init\n")
        #f)))

;;; Installation State Checks

(define (check-installation-state)
  "Check current installation state and return list of (has-kernel? has-initrd? has-grub?)."
  (display "\n=== Checking Installation State ===\n")
  
  (let ((has-kernel? (file-exists-in-dir? "vmlinuz-*" "/mnt/boot/"))
        (has-initrd? (file-exists-in-dir? "initrd-*" "/mnt/boot/"))
        (has-grub? (or (file-exists? "/mnt/boot/efi/EFI/Guix/grubx64.efi")
                       (file-exists? "/mnt/boot/efi/EFI/guix/grubx64.efi"))))
    
    (if has-kernel?
        (let ((kernel-name (get-first-matching-file "vmlinuz-*" "/mnt/boot/")))
          (format #t "[OK] Kernel found: ~a\n" kernel-name))
        (display "[MISSING] No kernel in /mnt/boot/\n"))
    
    (if has-initrd?
        (let ((initrd-name (get-first-matching-file "initrd-*" "/mnt/boot/")))
          (format #t "[OK] Initrd found: ~a\n" initrd-name))
        (display "[MISSING] No initrd in /mnt/boot/\n"))
    
    (if has-grub?
        (display "[OK] GRUB EFI bootloader found\n")
        (display "[MISSING] No GRUB EFI bootloader\n"))
    
    (list has-kernel? has-initrd? has-grub?)))

(define (needs-system-init? state)
  "Determine if system init needs to be run based on state."
  (let ((has-kernel? (first state))
        (has-initrd? (second state)))
    (if (or (not has-kernel?) (not has-initrd?))
        (begin
          (newline)
          (display "[!] Installation is INCOMPLETE - system init must be run\n")
          #t)
        #f)))

;;; Environment Setup

(define (setup-environment)
  "Setup environment variables and directories for installation."
  (display "\n=== Setting Up Environment ===\n")
  
  ;; Create directories
  (mkdir-p *tmpdir*)
  (mkdir-p *cache-dir*)
  
  ;; Set environment variables
  (setenv "TMPDIR" *tmpdir*)
  (setenv "XDG_CACHE_HOME" *cache-dir*)
  
  (format #t "TMPDIR=~a\n" *tmpdir*)
  (format #t "XDG_CACHE_HOME=~a\n" *cache-dir*)
  
  ;; Clear substitute cache
  (display "Clearing substitute cache...\n")
  (system "rm -rf /var/guix/substitute-cache/ || true"))

;;; System Init

(define (verify-efi-partition)
  "Verify EFI partition is vfat filesystem."
  (display "Verifying EFI partition...\n")
  (let-values (((output status) (run-command 
                                 (format #f "df -T ~a | grep -q vfat" *mnt-boot-efi*))))
    (unless (zero? status)
      (display "[ERROR] /mnt/boot/efi is not vfat filesystem!\n")
      (system* (format #f "df -T ~a" *mnt-boot-efi*))
      (exit 1)))
  (display "[OK] EFI partition is vfat\n"))

(define (start-cow-store)
  "Start cow-store if not already running."
  (display "Starting cow-store...\n")
  (let-values (((output status) (run-command "herd status cow-store | grep -q running")))
    (when (not (zero? status))
      (system* (format #f "herd start cow-store ~a" *mnt-root*))
      (sleep 2)))
  (display "[OK] cow-store is running\n"))

(define (wait-for-daemon)
  "Wait for guix-daemon to become responsive."
  (display "Checking guix-daemon...\n")
  (let loop ((attempt 1))
    (if (> attempt 10)
        (begin
          (display "[ERROR] guix-daemon not responsive after 10 attempts!\n")
          (exit 1))
        (if (run-command-success? "guix build --version >/dev/null 2>&1")
            (display "[OK] guix-daemon is responsive\n")
            (begin
              (format #t "Waiting for daemon... (~a/10)\n" attempt)
              (sleep 3)
              (loop (+ attempt 1)))))))

(define (build-init-command use-time-machine?)
  "Build the system init command string."
  (if use-time-machine?
      (string-append
       "guix time-machine -C /tmp/channels.scm -- "
       "system init --fallback -v6 /mnt/etc/config.scm /mnt "
       "--substitute-urls=\"https://substitutes.nonguix.org "
       "https://ci.guix.gnu.org https://bordeaux.guix.gnu.org\"")
      (string-append
       "guix system init --fallback -v6 /mnt/etc/config.scm /mnt "
       "--substitute-urls=\"https://ci.guix.gnu.org "
       "https://bordeaux.guix.gnu.org\"")))

(define (run-system-init use-time-machine?)
  "Run guix system init with appropriate options."
  (display "\n=== Running System Init ===\n")
  
  (verify-efi-partition)
  (start-cow-store)
  (wait-for-daemon)
  
  (let ((cmd (build-init-command use-time-machine?)))
    (newline)
    (format #t "Running: ~a\n" cmd)
    (newline)
    (display "This will take 5-15 minutes. Do NOT interrupt!\n")
    (display "If it fails, you can re-run this script.\n")
    (newline)
    
    (unless (system* cmd)
      (newline)
      (display "[ERROR] System init failed!\n")
      (newline)
      (display "You can try running it manually:\n")
      (format #t "  ~a\n" cmd)
      (newline)
      (display "Or re-run this recovery script after fixing the issue.\n")
      (exit 1))
    
    (newline)
    (display "[OK] System init completed successfully\n")))

(define (verify-post-init)
  "Verify installation after system init."
  (display "\n=== Verifying Installation (Post-Init) ===\n")
  
  (unless (file-exists-in-dir? "vmlinuz-*" "/mnt/boot/")
    (display "[ERROR] Still no kernel after system init!\n")
    (display "System init may have failed silently.\n")
    (exit 1))
  
  (unless (file-exists-in-dir? "initrd-*" "/mnt/boot/")
    (display "[ERROR] Still no initrd after system init!\n")
    (display "System init may have failed silently.\n")
    (exit 1))
  
  (display "[OK] Kernel and initrd are now present\n"))

;;; User Configuration

(define (detect-username)
  "Detect username from config.scm."
  (display "\n=== Detecting Username ===\n")
  (let ((username (grep-config "(?<=\\(name \")[^\"]+")))
    (if username
        (begin
          (format #t "Username: ~a\n" username)
          username)
        (begin
          (display "[WARN] Could not detect username from config.scm\n")
          (prompt-user "Enter your username: ")))))

(define (password-already-set? username)
  "Check if password is already set for user."
  (let ((cmd (format #f "grep '^~a:' /etc/shadow | grep -v ':!:'" username)))
    (chroot-run cmd)))

(define (set-user-password username)
  "Set password for user."
  (display "\n=== Setting User Password ===\n")
  (display "You need this password to log in after first boot.\n")
  (newline)
  
  (let ((already-set? (password-already-set? username)))
    (if already-set?
        (begin
          (format #t "[OK] Password already set for ~a\n" username)
          (let ((response (prompt-user "Do you want to change it? [y/N] ")))
            (when (or (string-ci=? response "y")
                      (string-ci=? response "yes"))
              (set! already-set? #f))))
        #f)
    
    (unless already-set?
      (if (chroot-run (format #f "passwd ~a" username))
          (begin
            (newline)
            (format #t "[OK] Password set successfully for ~a\n" username))
          (begin
            (newline)
            (display "[ERROR] Failed to set password!\n")
            (display "You can set it manually after first boot.\n"))))))

;;; Customization Tools

(define (download-customization-tools username)
  "Download customization tools to user's home directory."
  (display "\n=== Downloading Customization Tools ===\n")
  
  (let* ((platform (or (getenv "GUIX_PLATFORM") "framework-dual"))
         (user-home (format #f "/mnt/home/~a" username))
         (customize-dir (format #f "~a/guix-customize" user-home)))
    
    (if (file-exists? customize-dir)
        (format #t "[OK] Customization tools already present at ~a\n" customize-dir)
        (begin
          (format #t "Downloading to ~a...\n" customize-dir)
          (mkdir-p customize-dir)
          
          (let* ((url (format #f "~a/~a/postinstall/customize" *repo-base* platform))
                 (dest (format #f "~a/customize" customize-dir))
                 (cmd (format #f "wget -q -O ~a ~a" dest url)))
            (if (system* cmd)
                (begin
                  (system* (format #f "chmod +x ~a" dest))
                  (display "[OK] Downloaded customize script\n"))
                (begin
                  (display "[WARN] Failed to download customization tools\n")
                  (display "      You can download manually after first boot\n"))))
          
          ;; Set ownership
          (chroot-run (format #f "chown -R ~a:users /home/~a/guix-customize 2>/dev/null || true"
                             username username))))))

;;; Dual-Boot Configuration

(define (configure-dual-boot)
  "Configure dual-boot GRUB if on framework-dual platform."
  (let ((platform (or (getenv "GUIX_PLATFORM") "framework-dual")))
    (when (string=? platform "framework-dual")
      (display "\n=== Configuring Dual-Boot GRUB ===\n")
      
      (if (chroot-run "command -v os-prober")
          (begin
            (display "Enabling os-prober...\n")
            (chroot-run "echo 'GRUB_DISABLE_OS_PROBER=false' >> /etc/default/grub 2>/dev/null || true")
            
            (display "Running os-prober to detect Pop!_OS...\n")
            (chroot-run "os-prober || true")
            
            (display "Updating GRUB configuration...\n")
            (unless (chroot-run "grub-mkconfig -o /boot/grub/grub.cfg")
              (display "[WARN] Failed to update GRUB config\n")
              (display "      You can run this after first boot:\n")
              (display "      sudo os-prober && sudo grub-mkconfig -o /boot/grub/grub.cfg\n")))
          (begin
            (display "[WARN] os-prober not found in installed system\n")
            (display "      You'll need to manually configure dual-boot after first boot\n"))))))

;;; Installation Receipt

(define (write-installation-receipt username)
  "Write installation receipt to /root."
  (display "\n=== Writing Installation Receipt ===\n")
  
  (let* ((receipt-path "/mnt/root/install-receipt.txt")
         (platform (or (getenv "GUIX_PLATFORM") "framework-dual"))
         (hostname (or (grep-config "(?<=host-name \")[^\"]+") "unknown"))
         (channels-info (if (file-exists? *channels-path*)
                           "/tmp/channels.scm (nonguix)"
                           "default"))
         (date-str (let-values (((output status) (run-command "date")))
                    (string-trim-right output))))
    
    (call-with-output-file receipt-path
      (lambda (port)
        (display "Guix System Installation Receipt\n" port)
        (display "=================================\n" port)
        (format port "Date: ~a\n" date-str)
        (format port "Platform: ~a\n" platform)
        (format port "Username: ~a\n" username)
        (format port "Hostname: ~a\n" hostname)
        (newline port)
        (display "Installation completed via recovery script.\n" port)
        (newline port)
        (display "Config: /etc/config.scm\n" port)
        (format port "Channels: ~a\n" channels-info)
        (newline port)
        (display "Next steps:\n" port)
        (display "1. Log in with your username and password\n" port)
        (display "2. Run: ~/guix-customize/customize (to add SSH, desktop, packages)\n" port)
        (display "3. For dual-boot: Access Pop!_OS via F12 boot menu or GRUB menu\n" port)
        (newline port)
        (display "Installation log: /tmp/guix-install.log (if available)\n" port)))
    
    (format #t "[OK] Installation receipt written to ~a\n" receipt-path)))

;;; Final Verification

(define (basic-verification-checks)
  "Perform basic verification checks."
  (let ((all-good? #t))
    
    (if (file-exists-in-dir? "vmlinuz-*" "/mnt/boot/")
        (let ((kernel (get-first-matching-file "vmlinuz-*" "/mnt/boot/")))
          (format #t "[OK] Kernel: ~a\n" kernel))
        (begin
          (display "[ERROR] No kernel installed!\n")
          (set! all-good? #f)))
    
    (if (file-exists-in-dir? "initrd-*" "/mnt/boot/")
        (let ((initrd (get-first-matching-file "initrd-*" "/mnt/boot/")))
          (format #t "[OK] Initrd: ~a\n" initrd))
        (begin
          (display "[ERROR] No initrd installed!\n")
          (set! all-good? #f)))
    
    (if (or (file-exists? "/mnt/boot/efi/EFI/Guix/grubx64.efi")
            (file-exists? "/mnt/boot/efi/EFI/guix/grubx64.efi"))
        (display "[OK] GRUB EFI bootloader installed\n")
        (begin
          (display "[ERROR] No GRUB EFI bootloader!\n")
          (set! all-good? #f)))
    
    (if (file-exists? "/mnt/boot/grub/grub.cfg")
        (display "[OK] GRUB config exists\n")
        (begin
          (display "[ERROR] No GRUB config!\n")
          (set! all-good? #f)))
    
    all-good?))

(define (run-comprehensive-verification)
  "Try to run comprehensive verification script, fall back to basic checks."
  (let ((verify-script (cond
                        ((file-exists? "/root/verify-guix-install.sh")
                         "/root/verify-guix-install.sh")
                        ((file-exists? "./verify-guix-install.sh")
                         "./verify-guix-install.sh")
                        (else #f))))
    
    (if verify-script
        (begin
          (display "Running comprehensive verification...\n")
          (newline)
          (system* (format #f "bash ~a" verify-script)))
        (begin
          (display "Note: Using basic verification (verify-guix-install.sh not found)\n")
          (newline)
          (basic-verification-checks)))))

(define (final-verification username)
  "Perform final verification and prompt for reboot."
  (display "\n=== Final Verification ===\n")
  
  (let ((all-good? (run-comprehensive-verification)))
    (newline)
    
    (if all-good?
        (begin
          (display "=== Installation Complete! ===\n")
          (newline)
          (display "The system is ready to boot.\n")
          (newline)
          (display "Next steps:\n")
          (display "  1. Sync and unmount:\n")
          (display "     sync\n")
          (display "     umount -R /mnt\n")
          (display "  2. Reboot:\n")
          (display "     reboot\n")
          (newline)
          (display "After first boot:\n")
          (format #t "  - Log in with username: ~a\n" username)
          (display "  - Run: ~/guix-customize/customize\n")
          (display "  - For Pop!_OS: Press F12 at boot or select from GRUB menu\n")
          (newline)
          
          (let ((response (prompt-user "Unmount and reboot now? [y/N] ")))
            (when (or (string-ci=? response "y")
                      (string-ci=? response "yes"))
              (system "sync")
              (system "umount -R /mnt")
              (system "reboot")))
          
          (unless (or (string-ci=? response "y")
                      (string-ci=? response "yes"))
            (newline)
            (display "Remember to unmount before rebooting:\n")
            (display "  sync && umount -R /mnt && reboot\n")))
        (begin
          (display "=== Installation INCOMPLETE ===\n")
          (newline)
          (display "Critical files are missing. DO NOT REBOOT.\n")
          (newline)
          (display "Please review the errors above and:\n")
          (display "  1. Check /tmp/guix-install.log for errors\n")
          (display "  2. Try re-running the system init manually\n")
          (display "  3. Or re-run this recovery script\n")
          (exit 1)))))

;;; Main Entry Point

(define (main args)
  "Main entry point for recovery script."
  (display-banner)
  (prompt-continue)
  
  ;; Verification phase
  (check-guix-iso)
  (verify-mounts)
  (verify-config-exists)
  (let ((use-time-machine? (check-channels)))
    
    ;; Check installation state
    (let* ((state (check-installation-state))
           (need-init? (needs-system-init? state)))
      
      ;; Setup environment
      (setup-environment)
      
      ;; Run system init if needed
      (when need-init?
        (run-system-init use-time-machine?)
        (verify-post-init))
      
      (unless need-init?
        (newline)
        (display "[OK] System init already complete - skipping\n"))
      
      ;; User configuration
      (let ((username (detect-username)))
        (set-user-password username)
        (download-customization-tools username)
        (configure-dual-boot)
        (write-installation-receipt username)
        (final-verification username)))))

;; Run main if executed as script
(when (batch-mode?)
  (main (command-line)))
```