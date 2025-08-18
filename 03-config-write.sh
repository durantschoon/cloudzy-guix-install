#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

eval $(blkid -s UUID -o value $ROOT | awk '{print "UUID="$1}')
[ -n "$UUID" ] && echo "UUID set: $UUID" || echo "UUID not set"

mkdir -p /mnt/etc
cat > /mnt/etc/config.scm <<'EOF'
(use-modules (gnu)
             (gnu system nss)
             (gnu services ssh)
             (gnu services desktop))
(operating-system
 (host-name "guix-system")
 (timezone "Europe/Rome")
 (locale "en_US.utf8")
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets '("/boot/efi"))
   (keyboard-layout (keyboard-layout "us"))))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device (uuid "REPLACE_WITH_ROOT_UUID" 'ext4))
          (type "ext4"))
         (file-system
          (mount-point "/boot/efi")
          (device "REPLACE_WITH_EFI") ;; eg. /dev/sda1
          (type "vfat"))
         %base-file-systems))
 (users (cons* (user-account
                (name "USER_NAME")
                (comment "FULL_NAME")
                (group "users")
                (home-directory "/home/USER_NAME")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 (packages
  (append (list (specification->package "emacs")
                (specification->package "git")
                (specification->package "vim")) ; add more if you want
          %base-packages))
 (services
  (append
   (list (service openssh-service-type)
         (service gnome-desktop-service-type))
   (modify-services %base-services
                    (guix-service-type
                     config => (guix-configuration
                                (substitute-urls
                                 (list "https://ci.guix.gnu.org"
                                       "https://bordeaux.guix.gnu.org"))))))))
EOF

sed -i "
    s/REPLACE_WITH_ROOT_UUID/$UUID/g
    s|REPLACE_WITH_EFI|$EFI|g
    s/FULL_NAME/$FULL_NAME/g
    s/USER_NAME/$USER_NAME/g
" /mnt/etc/config.scm

cat /mnt/etc/config.scm
