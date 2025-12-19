#!/usr/bin/env -S guile --no-auto-compile
!#

;;; Finish Installation Script
;;; Run this after installation completes successfully to safely reboot

(use-modules (ice-9 format)
             (ice-9 textual-ports)
             (ice-9 regex)
             (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 string-fun)
             (srfi srfi-1))

(define (detect-username)
  "Detect username from config.scm, environment, or /etc/passwd."
  (display "\n=== Detecting Username ===\n")
  
  (or
   ;; Try config.scm first
   (let ((config-path "/mnt/etc/config.scm"))
     (and (file-exists? config-path)
          (let ((content (call-with-input-file config-path get-string-all)))
            (let ((match (string-match "\\(name\\s+\"([^\"]+)\"" content)))
              (and match
                   (let ((username (match:substring match 1)))
                     (and (> (string-length username) 0)
                          (< (string-length username) 50)
                          (begin
                            (format #t "Username: ~a\n" username)
                            username))))))))
   
   ;; Try environment variable
   (let ((username (getenv "USER_NAME")))
     (and username
          (> (string-length username) 0)
          (begin
            (format #t "Username from USER_NAME env var: ~a\n" username)
            username)))
   
   ;; Try /etc/passwd
   (let ((passwd-path "/mnt/etc/passwd"))
     (and (file-exists? passwd-path)
          (call-with-input-file passwd-path
            (lambda (port)
              (let loop ((line (read-line port)))
                (if (eof-object? line)
                    #f
                    (let ((parts (string-split line #\:)))
                      (if (>= (length parts) 3)
                          (let ((username (list-ref parts 0))
                                (uid-str (list-ref parts 2)))
                            (let ((uid (string->number uid-str)))
                              (if (and uid
                                       (>= uid 1000)
                                       (< uid 65534)
                                       (not (member username '("nobody" "nixbld" "guix"))))
                                  (begin
                                    (format #t "Username from /etc/passwd: ~a\n" username)
                                    username)
                                  (loop (read-line port)))))
                          (loop (read-line port))))))))))
   
   ;; Final fallback: prompt user
   (begin
     (display "Could not detect username from config.scm, environment, or /etc/passwd.\n")
     (display "Enter your username: ")
     (let ((username (read-line)))
       (if (or (eof-object? username)
               (not username)
               (= (string-length username) 0))
           (begin
             (display "[ERROR] Username cannot be empty\n")
             (exit 1))
           username)))))

(define (password-set? username)
  "Check if password is already set for user."
  (let ((cmd (format #f "chroot /mnt /run/current-system/profile/bin/bash -c \"grep '^~a:' /etc/shadow | grep -v ':!:'\" 2>/dev/null" username)))
    (let ((port (open-input-pipe cmd)))
      (let ((result (read-line port)))
        (close-pipe port)
        (and result (not (eof-object? result)) (> (string-length result) 0))))))

(define (check-file-exists path)
  "Check if file exists and return size if it does."
  (if (file-exists? path)
      (stat:size (stat path))
      #f))

(define (format-size bytes)
  "Format file size in human-readable format."
  (cond
   ((>= bytes 1073741824) (format #f "~,1f GiB" (/ bytes 1073741824.0)))
   ((>= bytes 1048576) (format #f "~,1f MiB" (/ bytes 1048576.0)))
   ((>= bytes 1024) (format #f "~,1f KiB" (/ bytes 1024.0)))
   (else (format #f "~a B" bytes))))

(define (run-verification username)
  "Run final verification before reboot."
  (display "\n=== Final Verification ===\n\n")
  
  (let ((all-good #t))
    
    ;; Check kernel
    (let ((kernel-file #f)
          (kernel-size #f))
      (let ((cmd "ls /mnt/boot/vmlinuz* /mnt/boot/vmlinuz 2>/dev/null | head -1"))
        (let ((port (open-input-pipe cmd)))
          (let ((result (read-line port)))
            (close-pipe port)
            (when (and result (not (eof-object? result)))
              (set! kernel-file (string-trim-both result))
              (set! kernel-size (check-file-exists kernel-file))))))
      
      (if (and kernel-file kernel-size (> kernel-size 5000000))
          (format #t "[OK] Kernel: ~a (~a)\n" 
                  (basename kernel-file) (format-size kernel-size))
          (begin
            (display "[ERROR] Kernel not found or too small\n")
            (set! all-good #f))))
    
    ;; Check initrd
    (let ((initrd-file #f)
          (initrd-size #f))
      (let ((cmd "ls /mnt/boot/initrd* /mnt/boot/initrd 2>/dev/null | head -1"))
        (let ((port (open-input-pipe cmd)))
          (let ((result (read-line port)))
            (close-pipe port)
            (when (and result (not (eof-object? result)))
              (set! initrd-file (string-trim-both result))
              (set! initrd-size (check-file-exists initrd-file))))))
      
      (if (and initrd-file initrd-size (> initrd-size 10000000))
          (format #t "[OK] Initrd: ~a (~a)\n"
                  (basename initrd-file) (format-size initrd-size))
          (begin
            (display "[ERROR] Initrd not found or too small\n")
            (set! all-good #f))))
    
    ;; Check GRUB config
    (if (file-exists? "/mnt/boot/grub/grub.cfg")
        (display "[OK] GRUB config exists\n")
        (display "[WARN] GRUB config not found (may be OK if EFI boot)\n"))
    
    ;; Check system symlink
    (if (and (file-exists? "/mnt/run/current-system")
             (eq? 'symlink (stat:type (lstat "/mnt/run/current-system"))))
        (let ((target (readlink "/mnt/run/current-system")))
          (if (file-exists? target)
              (format #t "[OK] System symlink: /mnt/run/current-system -> ~a\n" target)
              (begin
                (display "[ERROR] System symlink target does not exist\n")
                (set! all-good #f))))
        (begin
          (display "[ERROR] System symlink missing or broken\n")
          (set! all-good #f)))
    
    (newline)
    
    (if (not all-good)
        (begin
          (display "========================================\n")
          (display "  VERIFICATION FAILED\n")
          (display "========================================\n\n")
          (display "Critical files are missing. DO NOT REBOOT.\n\n")
          (display "Please run the recovery script:\n")
          (display "  /root/recovery-complete-install.sh\n\n")
          (exit 1))
        (begin
          (display "========================================\n")
          (display "  INSTALLATION COMPLETE!\n")
          (display "========================================\n\n")
          (display "Your Guix system is ready to boot.\n\n")
          
          ;; Check password status
          (let ((password-set (password-set? username)))
            (if (not password-set)
                (begin
                  (display "[WARN] IMPORTANT: Password not set!\n")
                  (format #t "   After first boot, run:\n")
                  (format #t "     sudo passwd ~a\n\n" username))
                (newline))
            
            ;; Show next steps
            (display "Next steps:\n")
            (display "  1. Sync filesystems:\n")
            (display "     sync\n")
            (display "  2. Unmount all filesystems:\n")
            (display "     umount -R /mnt\n")
            (display "  3. Reboot:\n")
            (display "     reboot\n\n")
            
            (display "After first boot:\n")
            (when (not password-set)
              (format #t "  - Set password: sudo passwd ~a\n" username))
            (format #t "  - Log in with username: ~a\n" username)
            (display "  - Run customization: ~/guix-customize/customize\n")
            
            ;; Check if framework-dual
            (when (file-exists? "/mnt/etc/config.scm")
              (let ((content (call-with-input-file "/mnt/etc/config.scm" get-string-all)))
                (when (string-contains content "framework-dual")
                  (display "  - For Pop!_OS: Press F12 at boot or select from GRUB menu\n"))))
            
            (newline)
            
            ;; Prompt for reboot
            (display "Sync, unmount, and reboot now? [y/N] ")
            (let ((response (read-line)))
              (if (and response
                       (not (eof-object? response))
                       (or (string-ci=? (string-trim-both response) "y")
                           (string-ci=? (string-trim-both response) "yes")))
                  (begin
                    (display "\nSyncing filesystems...\n")
                    (system "sync")
                    
                    (display "Unmounting filesystems...\n")
                    
                    ;; Try normal unmount first
                    (let ((result (system "umount -R /mnt 2>&1")))
                      (if (zero? result)
                          (display "[OK] All filesystems unmounted successfully\n")
                          (begin
                            (display "[WARN] Some filesystems are busy (this is normal)\n")
                            (display "       Trying individual unmounts...\n")
                            
                            ;; Unmount virtual filesystems first (they're usually safe)
                            (system "umount /mnt/proc 2>/dev/null || true")
                            (system "umount /mnt/sys 2>/dev/null || true")
                            (system "umount /mnt/dev 2>/dev/null || true")
                            (system "umount /mnt/boot/efi 2>/dev/null || true")
                            
                            ;; Try normal unmount of main filesystem
                            (let ((main-result (system "umount /mnt 2>&1")))
                              (if (zero? main-result)
                                  (display "[OK] Main filesystem unmounted\n")
                                  (begin
                                    (display "[INFO] Using lazy unmount (detaches immediately, cleans up on reboot)\n")
                                    ;; Lazy unmount: detaches immediately, cleans up when processes finish
                                    ;; This is safe for rebooting - kernel will clean up on shutdown
                                    (system "umount -l /mnt 2>/dev/null || true")
                                    (display "[OK] Filesystem marked for lazy unmount\n")))))))
                    
                    (display "\nRebooting in 3 seconds...\n")
                    (sleep 3)
                    (system "reboot"))
                  (begin
                    (newline)
                    (display "Remember to finish installation manually:\n")
                    (display "  cd /\n")
                    (display "  sync\n")
                    (display "  umount -R /mnt || umount -l /mnt\n")
                    (display "  reboot\n")
                    (when (not password-set)
                      (format #t "\nAnd after first boot, set your password:\n")
                      (format #t "  sudo passwd ~a\n" username))))))))))

;;; Main entry point
(let ((username (detect-username)))
  (run-verification username))
