;;; Guix System image for Oracle Cloud Infrastructure (OCI) Always Free tier.
;;;
;;; Unlike every other platform in this repository, OCI cannot boot an ISO, so
;;; there is no "boot the installer, partition, guix system init" flow here.
;;; Instead this file is built into a disk image locally and uploaded.
;;;
;;; Build:
;;;
;;;   guix system image -t qcow2 --image-size=50G \
;;;        oracle/image/oracle-image.scm
;;;
;;; Import into OCI with launch mode PARAVIRTUALIZED.
;;;
;;; Every setting below is justified in oracle-image_purpose.txt.  Read that
;;; before changing anything -- several values are load-bearing in ways that
;;; are not obvious from the code (the root file system label in particular).

(use-modules (gnu)
             (guix gexp))

(use-service-modules networking shepherd ssh)
(use-package-modules base linux ssh)


;;;
;;; Site-specific settings.
;;;

(define %user-name "guix")
(define %full-name "Guix User")
(define %host-name "guix-oracle")
(define %timezone "America/New_York")

;; Public half of the SSH key permitted to log in.  Guix has no cloud-init, so
;; there is no mechanism to inject a key at launch time: it must be baked into
;; the image.  Get this wrong and the instance is unreachable over SSH, because
;; password authentication is disabled below.  Recovery would be via the OCI
;; serial console, which is why the serial console is configured to work.
(define %authorized-key (local-file "authorized-key.pub"))

;; VM.Standard.E2.1.Micro has 1 GiB of RAM.  'guix pull' and 'guix system
;; reconfigure' are memory-hungry and get OOM-killed without swap.
(define %swapfile "/swapfile")
(define %swapfile-size-mib 2048)


;;;
;;; Swap file, created on first boot.
;;;
;;; The 'swap-devices' field cannot be used here: it expects the swap area to
;;; already exist, and nothing in a freshly built image has created one.  So
;;; this is a one-shot shepherd service that creates the file if absent and
;;; enables it, which makes it idempotent across reboots.

(define %swapfile-service
  (simple-service
   'oracle-swapfile shepherd-root-service-type
   (list
    (shepherd-service
     (provision '(swapfile))
     (requirement '(file-systems))
     (documentation "Create a swap file if absent, then enable it.")
     (one-shot? #t)
     (start
      #~(lambda _
          (define (run . args)
            (zero? (apply system* args)))
          (and (or (file-exists? #$%swapfile)
                   ;; dd rather than fallocate: fallocate produces unwritten
                   ;; extents on ext4 and swapon refuses to use such a file.
                   (and (run #$(file-append coreutils "/bin/dd")
                             "if=/dev/zero"
                             (string-append "of=" #$%swapfile)
                             "bs=1M"
                             #$(string-append
                                "count=" (number->string %swapfile-size-mib)))
                        (begin (chmod #$%swapfile #o600) #t)
                        (run #$(file-append util-linux "/sbin/mkswap")
                             #$%swapfile)))
               (run #$(file-append util-linux "/sbin/swapon") #$%swapfile))))
     (stop
      #~(lambda _
          (system* #$(file-append util-linux "/sbin/swapoff") #$%swapfile)
          #f))))))


;;;
;;; The system.
;;;

(operating-system
 (host-name %host-name)
 (timezone %timezone)
 (locale "en_US.utf8")

 ;; Free software only, consistent with the cloudzy platform.  linux-libre is
 ;; sufficient here: OCI paravirtualized instances present virtio devices, and
 ;; virtio needs no redistributable firmware.
 (kernel linux-libre)

 ;; Initrd modules are deliberately NOT overridden.  %base-initrd-modules
 ;; already contains virtio_pci, virtio_blk, virtio_net and virtio_scsi, which
 ;; is everything a paravirtualized OCI instance needs to find its root disk
 ;; and its network card.

 ;; console=tty0 keeps output on the emulated VGA console; console=ttyS0 is
 ;; what the OCI serial console attaches to.  Listing ttyS0 last makes it the
 ;; primary console, so kernel panics are visible in the OCI console.  This
 ;; also gives a login prompt on the serial line for free: %base-services runs
 ;; agetty with (tty #f), which auto-detects the console from this very line.
 (kernel-arguments
  (append '("console=tty0" "console=ttyS0,115200n8")
          %default-kernel-arguments))

 (bootloader
  (bootloader-configuration
   ;; BIOS GRUB, matching the 'qcow2' image type (MBR + hybrid ESP) and the
   ;; PARAVIRTUALIZED launch mode.  Using grub-efi-bootloader here would
   ;; require the 'qcow2-gpt' image type and the NATIVE launch mode instead.
   (bootloader grub-bootloader)
   (targets '("/dev/vda"))
   ;; Mirror the boot menu onto the serial line so the OCI console can be used
   ;; to pick an older generation when a reconfigure breaks the system.
   (terminal-outputs '(console serial_0))
   (terminal-inputs '(console serial_0))
   (serial-unit 0)
   (serial-speed 115200)
   (timeout 3)))

 ;; "Guix_image" is not a name we chose: it is the label 'guix system image'
 ;; writes onto the root partition (gnu/system/image.scm, root-label).  If this
 ;; string does not match, the initrd cannot find the root file system and the
 ;; instance drops to a Guile rescue REPL on the serial console.
 (file-systems
  (cons (file-system
         (mount-point "/")
         (device (file-system-label "Guix_image"))
         (type "ext4"))
        %base-file-systems))

 (users (cons (user-account
               (name %user-name)
               (comment %full-name)
               (group "users")
               (home-directory (string-append "/home/" %user-name))
               ;; No password field: the account gets a locked password, so
               ;; password login is impossible while SSH key login still works.
               (supplementary-groups '("wheel" "netdev")))
              %base-user-accounts))

 ;; Passwordless sudo for wheel.  This is not gratuitous: the account above has
 ;; no password by design, so ordinary sudo would prompt for one that does not
 ;; exist and the user could never become root.  This mirrors what cloud-init
 ;; does for the default user on other distributions' cloud images.
 (sudoers-file
  (plain-file "sudoers"
              (string-append "root ALL=(ALL) ALL\n"
                             "%wheel ALL=NOPASSWD:ALL\n")))

 ;; Minimal: nss-certs is already in %base-packages (via
 ;; %base-packages-networking), so 'guix pull' has working TLS out of the box.
 (packages %base-packages)

 (services
  (append
   (list
    ;; OCI hands out addresses, routes and DNS over DHCP on the VNIC.
    ;; dhcpcd-service-type, not dhcp-client-service-type: the latter is
    ;; deprecated in this Guix (17c2142) and warns on every evaluation.
    (service dhcpcd-service-type)

    (service openssh-service-type
             (openssh-configuration
              ;; -sans-x avoids pulling X11 into a headless server image.
              (openssh openssh-sans-x)
              (permit-root-login #f)
              (password-authentication? #f)
              (authorized-keys
               `((,%user-name ,%authorized-key)))))

    %swapfile-service)

   %base-services)))
