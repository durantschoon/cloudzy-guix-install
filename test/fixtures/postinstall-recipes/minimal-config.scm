;; Minimal Guix System configuration for testing
;; This represents a fresh install with no customizations

(use-modules (gnu))

(operating-system
  (host-name "test-system")
  (timezone "America/New_York")

  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))))

  (file-systems
   (cons* (file-system
           (device (file-system-label "guix-root"))
           (mount-point "/")
           (type "ext4"))
          %base-file-systems))

  (users (cons (user-account
                (name "test")
                (group "users")
                (supplementary-groups '("wheel" "netdev")))
               %base-user-accounts))

  (packages %base-packages)

  (services %base-services))
