#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Verify configuration variables
echo "=== System Configuration Setup ==="
echo ""
echo "Required environment variables:"
echo "  USER_NAME: ${USER_NAME:-[NOT SET]}"
echo "  FULL_NAME: ${FULL_NAME:-[NOT SET]}"
echo "  TIMEZONE: ${TIMEZONE:-[NOT SET]}"
echo "  HOST_NAME: ${HOST_NAME:-[NOT SET]}"
echo ""
echo "Optional environment variables:"
echo "  BOOT_MODE: ${BOOT_MODE:-auto-detect (will use UEFI for Framework)}"
echo ""

# Check required variables
MISSING_VARS=()
[[ -z "${USER_NAME:-}" ]] && MISSING_VARS+=("USER_NAME")
[[ -z "${FULL_NAME:-}" ]] && MISSING_VARS+=("FULL_NAME")
[[ -z "${TIMEZONE:-}" ]] && MISSING_VARS+=("TIMEZONE")
[[ -z "${HOST_NAME:-}" ]] && MISSING_VARS+=("HOST_NAME")

if [[ ${#MISSING_VARS[@]} -gt 0 ]]; then
  echo "⚠️  ERROR: Missing required environment variables:"
  for var in "${MISSING_VARS[@]}"; do
    echo "    - $var"
  done
  echo ""
  echo "Example configuration:"
  echo '  export USER_NAME="yourname"'
  echo '  export FULL_NAME="Your Full Name"'
  echo '  export TIMEZONE="America/New_York"'
  echo '  export HOST_NAME="framework-guix"'
  echo ""
  exit 1
fi

echo "✓ All required variables are set"
echo ""
echo "This script will generate /mnt/etc/config.scm with:"
echo "  - Dual-boot GRUB configuration (UEFI)"
echo "  - User account: $USER_NAME ($FULL_NAME)"
echo "  - Timezone: $TIMEZONE"
echo "  - Hostname: $HOST_NAME"
echo "  - Minimal system (no desktop, add via customize tool after boot)"
echo ""
