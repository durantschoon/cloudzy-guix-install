#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Validate required environment variables
required_vars=("USER_NAME" "FULL_NAME" "TIMEZONE" "HOST_NAME" "ROOT" "EFI")
missing_vars=()

for var in "${required_vars[@]}"; do
  if [[ -z "${!var:-}" ]]; then
    missing_vars+=("$var")
  fi
done

if [[ ${#missing_vars[@]} -gt 0 ]]; then
  echo "Error: Missing required environment variables:"
  printf "  - %s\n" "${missing_vars[@]}"
  echo ""
  echo "Please set these variables before running this script:"
  echo "  USER_NAME=\"your_username\""
  echo "  FULL_NAME=\"Your Full Name\""
  echo "  TIMEZONE=\"America/New_York\""
  echo "  HOST_NAME=\"guix-vps\""
  echo "  ROOT and EFI are set by previous scripts"
  echo ""
  echo "Optional variables:"
  echo "  DESKTOP_ENV=\"gnome\"  # Desktop: gnome, xfce, mate, lxqt, none"
  echo "  BOOT_MODE=\"uefi\"     # Boot: uefi, bios (auto-detected)"
  echo "  SWAP_SIZE=\"4G\"       # Swap size: 2G, 4G, 8G, etc."
  exit 1
fi

# Export all validated variables for the clean script (directly available in main context)
export USER_NAME FULL_NAME TIMEZONE HOST_NAME ROOT EFI
export DESKTOP_ENV BOOT_MODE SWAP_SIZE
