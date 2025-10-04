#!/usr/bin/env bash
# lib/common.sh - Shared functions for Guix installation scripts

# Source mirror configuration if available
SCRIPT_DIR_COMMON="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [[ -f "$SCRIPT_DIR_COMMON/mirrors.sh" ]]; then
  source "$SCRIPT_DIR_COMMON/mirrors.sh"
fi

# Get UUID of root partition
get_root_uuid() {
  local root_device="${1:-$ROOT}"
  if [[ -z "$root_device" ]]; then
    echo "Error: ROOT device not specified" >&2
    return 1
  fi

  local uuid=$(blkid -s UUID -o value "$root_device")
  if [[ -z "$uuid" ]]; then
    echo "Error: Could not get UUID for $root_device" >&2
    return 1
  fi

  echo "$uuid"
}

# Detect boot mode (UEFI or BIOS)
detect_boot_mode() {
  if [ -d /sys/firmware/efi ] || [ -d /boot/efi ]; then
    echo "uefi"
  else
    echo "bios"
  fi
}

# Configure desktop environment
# Returns the service type string for config.scm
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
      return 1
      ;;
  esac
}

# Escape special characters for sed
escape() {
  printf '%s' "$1" | sed -e 's/[\/&]/\\&/g'
}

# Generate minimal base Guix config.scm
# Usage: generate_minimal_config_scm > /mnt/etc/config.scm
# This creates the absolute minimum bootable system
generate_minimal_config_scm() {
  cat <<'EOF'
;; Minimal Guix System Configuration
;; This is the bare minimum to get a bootable system.
;; Customize after installation using: guix-customize

(use-modules (gnu)
             (gnu system nss))

(operating-system
 (host-name "HOST_NAME")
 (timezone "TIMEZONE_PLACEHOLDER")
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
          (device "REPLACE_WITH_EFI")
          (type "vfat"))
         %base-file-systems))

 (users (cons* (user-account
                (name "USER_NAME")
                (comment "FULL_NAME")
                (group "users")
                (home-directory "/home/USER_NAME")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 ;; Minimal packages - add more after installation
 (packages %base-packages)

 ;; Minimal services - add SSH, desktop, etc. after installation
 (services %base-services))
EOF
}

# Generate channels.scm with configured mirrors
# Usage: generate_channels_scm > ~/.config/guix/channels.scm
generate_channels_scm() {
  # Get mirrors if not already loaded
  if [[ -z "${GUIX_GIT_URL:-}" ]]; then
    get_mirrors >/dev/null 2>&1 || true
  fi

  local guix_url="${GUIX_GIT_URL:-https://git.savannah.gnu.org/git/guix.git}"
  local nonguix_url="${NONGUIX_GIT_URL:-https://gitlab.com/nonguix/nonguix.git}"

  cat <<EOF
;; Guix channels configuration
;; Generated with regional mirror optimization

(list (channel
        (name 'guix)
        (url "$guix_url")
        (branch "master"))
      (channel
        (name 'nonguix)
        (url "$nonguix_url")
        (branch "master")))
EOF
}

# Generate substitute-urls list for config.scm
# Usage: SUBSTITUTE_URLS_SCHEME=$(generate_substitute_urls)
generate_substitute_urls() {
  # Get mirrors if not already loaded
  if [[ -z "${SUBSTITUTE_URLS:-}" ]]; then
    get_mirrors >/dev/null 2>&1 || true
  fi

  local urls=("${SUBSTITUTE_URLS[@]:-https://ci.guix.gnu.org https://bordeaux.guix.gnu.org}")
  local result="(list"

  for url in "${urls[@]}"; do
    result="$result \"$url\""
  done

  result="$result)"
  echo "$result"
}

# Generate base Guix config.scm (backward compatibility)
# Usage: generate_base_config_scm > /mnt/etc/config.scm
generate_base_config_scm() {
  generate_minimal_config_scm
}

# Apply substitutions to config.scm
# Usage: apply_config_substitutions /mnt/etc/config.scm
apply_config_substitutions() {
  local config_file="$1"

  if [[ ! -f "$config_file" ]]; then
    echo "Error: Config file not found: $config_file" >&2
    return 1
  fi

  # Escape all variables for sed
  local uuid_esc=$(escape "$UUID")
  local efi_esc=$(escape "$EFI")
  local full_esc=$(escape "$FULL_NAME")
  local user_esc=$(escape "$USER_NAME")
  local tz_esc=$(escape "$TIMEZONE")
  local host_esc=$(escape "$HOST_NAME")
  local bootloader_esc=$(escape "$BOOTLOADER")
  local targets_esc=$(escape "$TARGETS")

  # Apply substitutions
  sed -i \
    -e "s|REPLACE_WITH_ROOT_UUID|$uuid_esc|g" \
    -e "s|REPLACE_WITH_EFI|$efi_esc|g" \
    -e "s|FULL_NAME|$full_esc|g" \
    -e "s|USER_NAME|$user_esc|g" \
    -e "s|TIMEZONE_PLACEHOLDER|$tz_esc|g" \
    -e "s|HOST_NAME|$host_esc|g" \
    -e "s|BOOTLOADER_PLACEHOLDER|$bootloader_esc|g" \
    -e "s|TARGETS_PLACEHOLDER|$targets_esc|g" \
    "$config_file"
}
