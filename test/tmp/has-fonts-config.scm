(use-modules (gnu) (gnu packages))

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
 (packages
  (append
   (list (specification->package "emacs")
         (specification->package "font-fira-code")
         (specification->package "git")
         (specification->package "font-jetbrains-mono")
         (specification->package "font-dejavu")
         (specification->package "font-liberation")
         (specification->package "font-gnu-freefont")
         (specification->package "font-awesome")
         (specification->package "font-google-noto"))
   %base-packages))
 (services %base-services))

