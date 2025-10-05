#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Source common library functions
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../../lib/common.sh"

# Get UUID of root partition
UUID=$(get_root_uuid "$ROOT")
[ -n "$UUID" ] && echo "UUID set: $UUID" || echo "UUID not set"

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

echo ""
echo "=== Generating Minimal Config ==="
echo "This creates a bare-bones bootable system with:"
echo "  - Base system packages only"
echo "  - No desktop environment"
echo "  - No SSH (add after installation)"
echo ""
echo "To customize after installation, use the guix-customize script"
echo ""

# Generate minimal config.scm using lib/common.sh
mkdir -p /mnt/etc
generate_minimal_config_scm > /mnt/etc/config.scm

# Apply substitutions using lib/common.sh
apply_config_substitutions /mnt/etc/config.scm

cat /mnt/etc/config.scm

# Output variables for Go program to capture and pass to next script
echo "###GUIX_INSTALL_VARS###"
echo "DEVICE=$DEVICE EFI=$EFI ROOT=$ROOT"
