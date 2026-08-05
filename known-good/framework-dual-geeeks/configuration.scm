;; Framework 13 AMD (Ryzen AI 300 / Strix Point) - Dual-Boot Minimal Config
;; Revised 2026-08-02.
;;
;; Changes from the previously deployed /etc/config.scm:
;;
;;   1. kernel-arguments now APPENDS to %default-kernel-arguments instead of
;;      replacing it, and "nomodeset" is gone.
;;
;;      %default-kernel-arguments is (list "modprobe.blacklist=usbmouse,usbkbd"
;;      "quiet").  Replacing it dropped the usbkbd blacklist, which upstream
;;      sets because usbkbd races usbhid (bugs.gnu.org/35574).
;;
;;      "nomodeset" disabled kernel modesetting while this same config loads
;;      amdgpu and linux-firmware.  It cannot supply missing firmware; it only
;;      guarantees an unaccelerated console.
;;
;;   2. initrd-modules is now %base-initrd-modules, unmodified.
;;
;;      - "amdgpu" removed: not needed to mount root, and loading the GPU
;;        driver from the initrd also requires its firmware in the initrd,
;;        which is a second way to fail before any console exists.
;;      - "usbhid" removed: already in %base-initrd-modules.  Also irrelevant
;;        to this laptop's internal keyboard, which is i8042
;;        ("AT Translated Set 2 keyboard", IRQ 1), not USB.
;;      - "i2c_piix4" removed: SMBus for sensors, nothing to do with booting.
;;      - The (remove ...) filter on "nvme" / "xhci_pci" was a no-op: neither
;;        name appears in %base-initrd-modules (see default-initrd-modules in
;;        gnu/system/linux-initrd.scm).  Hardcoding "built into 6.6.16" is also
;;        a claim about one pinned kernel; under a newer pin it would
;;        eventually strip a module needed to mount root.
;;
;;   3. Everything else is unchanged, deliberately -- including
;;      (initrd microcode-initrd), which is correct on AMD, and the /data
;;      file-system using (flags '(no-atime)) rather than (options "noatime").
;;
;; Build with the pinned channels, NOT with the host guix:
;;   guix time-machine -C ~/channels-framework-dual.scm -- \
;;     system init /mnt/guixroot/etc/config.scm /mnt/guixroot

(use-modules (gnu)
             (gnu packages linux)
             (gnu services dbus)       ;dbus-root-service-type, polkit-service-type
             (gnu services networking) ;network-manager, wpa-supplicant, ntp
             (gnu system nss)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (srfi srfi-1))

(operating-system
 (host-name "geeeks")
 (timezone "America/New_York")
 (locale "en_US.utf8")

 (keyboard-layout
  (keyboard-layout "us"
                   #:options '("ctrl:swapcaps")))

 ;; Linux kernel with proprietary firmware support (from nonguix).
 ;; At the pinned nonguix commit this is 7.1, well past the 6.10 that
 ;; gfx11.5 (Radeon 890M, 1002:1114) support requires.
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))

 ;; The initrd only has to mount root; udev loads everything else once the
 ;; real system is up.  See note 2 in the header.
 (initrd-modules %base-initrd-modules)

 ;; APPEND, never replace.  See note 1 in the header.
 (kernel-arguments (append '("loglevel=3") %default-kernel-arguments))

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (timeout 5)))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device (file-system-label "GUIX_ROOT"))
          (type "ext4"))
         (file-system
          (mount-point "/boot/efi")
          (device (file-system-label "EFI"))
          (type "vfat"))
         (file-system
          (mount-point "/data")
          (device (file-system-label "DATA"))
          (type "ext4")
          ;; no-atime belongs in flags (mount(2) bits), never in options
          ;; (filesystem-specific data string).  "ext4: Unknown parameter
          ;; 'noatime'" makes file-system-/data fail, which fails the
          ;; file-systems target, which means no login ttys at all.
          (flags '(no-atime)))
         %base-file-systems))

 (users (cons* (user-account
                (name "durant")
                (comment "Durant Schoon")
                (group "users")
                (home-directory "/home/durant")
                (supplementary-groups '("wheel" "netdev")))
               %base-user-accounts))

 ;; Minimal packages - add more after installation
 (packages %base-packages)

 ;; Minimal services PLUS networking.
 ;;
 ;; This machine's only network interface is a MediaTek MT7925 wireless card
 ;; (14c3:0717, driver mt7925e).  There is no built-in ethernet -- Framework 13
 ;; uses expansion cards -- so without WiFi the installed system has no network
 ;; at all and cannot even guix pull to repair itself.  %base-services provides
 ;; only loopback, so NetworkManager has to be here from the start.
 ;;
 ;; dbus and polkit are NOT optional extras: network-manager-service-type
 ;; declares service-extensions onto dbus-root-service-type and
 ;; polkit-service-type, and Guix aborts with "no target of type ..." if either
 ;; is absent.  %base-services instantiates neither.  wpa-supplicant is what
 ;; %desktop-services pairs with NetworkManager, annotated in Guix's own source
 ;; as ";needed by NetworkManager".
 ;;
 ;; The mt7925e driver does not exist before Linux 6.7, which is why the
 ;; previously installed 6.6.16 system had no wireless driver at all.  Its
 ;; firmware (mediatek/mt7925/*) comes from nonguix's linux-firmware, declared
 ;; above.
 ;;
 ;; After booting: log in on tty1, then run `nmtui' (curses UI, works on a
 ;; plain console) or:
 ;;   nmcli device wifi list
 ;;   nmcli device wifi connect "SSID" password "..."
 ;; nmcli/nmtui land on PATH automatically -- the service extends
 ;; profile-service-type.
 (services
  (append
   (list (service network-manager-service-type)
         (service wpa-supplicant-service-type)
         (service dbus-root-service-type)
         (service polkit-service-type)
         (service ntp-service-type))
   %base-services)))
