#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Source common library functions
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../../lib/common.sh"

# Get UUID of root partition
UUID=$(get_root_uuid "$ROOT")
[ -n "$UUID" ] && echo "UUID set: $UUID" || echo "UUID not set"

# Framework 13 uses UEFI - force it for dual-boot
BOOT_MODE="${BOOT_MODE:-uefi}"

if [ "$BOOT_MODE" = "uefi" ]; then
  BOOTLOADER="grub-efi-bootloader"
  TARGETS='("/boot/efi")'
  echo "UEFI boot mode - using grub-efi-bootloader"
else
  echo "ERROR: Dual-boot configuration requires UEFI mode"
  echo "Framework 13 uses UEFI. Set BOOT_MODE=uefi or leave unset for auto-detection."
  exit 1
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

echo ""
echo "=== Generated config.scm ==="
cat /mnt/etc/config.scm
echo ""
echo "✓ Configuration written to /mnt/etc/config.scm"
echo "  This will install GRUB to the existing ESP alongside Pop!_OS"
