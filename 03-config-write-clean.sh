#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

eval $(blkid -s UUID -o value $ROOT | awk '{print "UUID="$1}')
[ -n "$UUID" ] && echo "UUID set: $UUID" || echo "UUID not set"

# Detect boot mode and set appropriate bootloader
detect_boot_mode() {
  if [ -d /sys/firmware/efi ] || [ -d /boot/efi ]; then
    echo "uefi"
  else
    echo "bios"
  fi
}

# Allow user override with environment variable, otherwise auto-detect
BOOT_MODE="${BOOT_MODE:-$(detect_boot_mode)}"

if [ "$BOOT_MODE" = "uefi" ]; then
  BOOTLOADER="grub-efi-bootloader"
  TARGETS='("/boot/efi")'
  echo "UEFI boot detected - using grub-efi-bootloader"
else
  BOOTLOADER="grub-bootloader"
  TARGETS='("'$DEVICE'")'
  echo "BIOS boot detected - using grub-bootloader"
fi

# Configure desktop environment
configure_desktop() {
  case "$1" in
    gnome)
      echo "gnome-desktop-service-type"
      ;;
    xfce)
      echo "xfce-desktop-service-type"
      ;;
    mate)
      echo "mate-desktop-service-type"
      ;;
    lxqt)
      echo "lxqt-desktop-service-type"
      ;;
    none)
      echo ""
      ;;
    *)
      echo "Error: Unknown desktop environment '$1'" >&2
      echo "" >&2
      echo "Supported desktop environments:" >&2
      echo "  gnome  - Full-featured, modern desktop (default)" >&2
      echo "  xfce   - Lightweight, fast, traditional desktop" >&2
      echo "  mate   - Classic GNOME 2 experience" >&2
      echo "  lxqt   - Very lightweight, minimal resource usage" >&2
      echo "  none   - Server mode, no desktop environment" >&2
      echo "" >&2
      echo "Example: DESKTOP_ENV=\"xfce\"" >&2
      exit 1
      ;;
  esac
}

# Allow user override with environment variable, otherwise use default
DESKTOP_ENV="${DESKTOP_ENV:-gnome}"
DESKTOP_SERVICE=$(configure_desktop "$DESKTOP_ENV")

if [ -n "$DESKTOP_SERVICE" ]; then
  echo "Desktop environment: $DESKTOP_ENV - using $DESKTOP_SERVICE"
else
  echo "Desktop environment: none - server mode (no desktop)"
fi

echo ""
echo "Note: You can change the desktop environment by setting DESKTOP_ENV:"
echo "  DESKTOP_ENV=\"xfce\"   # Lightweight desktop"
echo "  DESKTOP_ENV=\"lxqt\"   # Minimal resource usage"
echo "  DESKTOP_ENV=\"none\"   # Server mode (no desktop)"
echo ""

mkdir -p /mnt/etc
cat > /mnt/etc/config.scm <<'EOF'
(use-modules (gnu)
             (gnu system nss)
             (gnu services ssh)
             (gnu services desktop))

(operating-system
 (host-name "HOST_NAME")               ; e.g. "guix-vps"
 (timezone "TIMEZONE_PLACEHOLDER")     ; will be replaced with $TIMEZONE
 (locale "en_US.utf8")

 (bootloader
  (bootloader-configuration
   (bootloader BOOTLOADER_PLACEHOLDER)
   (targets 'TARGETS_PLACEHOLDER)
   (keyboard-layout (keyboard-layout "us"))))

 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device (uuid "REPLACE_WITH_ROOT_UUID" 'ext4))
          (type "ext4"))
         (file-system
          (mount-point "/boot/efi")
          (device "REPLACE_WITH_EFI")  ; e.g. /dev/sda1 or /dev/vda1
          (type "vfat"))
         %base-file-systems))

 (users (cons* (user-account
                (name "USER_NAME")             ; login name
                (comment "FULL_NAME")          ; your full name
                (group "users")
                (home-directory "/home/USER_NAME")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 (packages
  (append (list (specification->package "emacs")
                (specification->package "git")
                (specification->package "vim"))
          %base-packages))

 (services
  (append
   (list (service openssh-service-type)
         DESKTOP_SERVICE_PLACEHOLDER)
   (modify-services %base-services
                    (guix-service-type
                     config => (guix-configuration
                                (substitute-urls
                                 (list "https://ci.guix.gnu.org"
                                       "https://bordeaux.guix.gnu.org"))))))))
EOF

escape() { printf '%s' "$1" | sed -e 's/[\/&]/\\&/g'; }

UUID_ESC=$(escape "$UUID")
EFI_ESC=$(escape "$EFI")
FULL_ESC=$(escape "$FULL_NAME")
USER_ESC=$(escape "$USER_NAME")
TZ_ESC=$(escape "$TIMEZONE")
HOST_ESC=$(escape "$HOST_NAME")
BOOTLOADER_ESC=$(escape "$BOOTLOADER")
TARGETS_ESC=$(escape "$TARGETS")
DESKTOP_SERVICE_ESC=$(escape "$DESKTOP_SERVICE")

sed -i \
  -e "s|REPLACE_WITH_ROOT_UUID|$UUID_ESC|g" \
  -e "s|REPLACE_WITH_EFI|$EFI_ESC|g" \
  -e "s|FULL_NAME|$FULL_ESC|g" \
  -e "s|USER_NAME|$USER_ESC|g" \
  -e "s|TIMEZONE_PLACEHOLDER|$TZ_ESC|g" \
  -e "s|HOST_NAME|$HOST_ESC|g" \
  -e "s|BOOTLOADER_PLACEHOLDER|$BOOTLOADER_ESC|g" \
  -e "s|TARGETS_PLACEHOLDER|$TARGETS_ESC|g" \
  -e "s|DESKTOP_SERVICE_PLACEHOLDER|$DESKTOP_SERVICE_ESC|g" \
  /mnt/etc/config.scm

cat /mnt/etc/config.scm
