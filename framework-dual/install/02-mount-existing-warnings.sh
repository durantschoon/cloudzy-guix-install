#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Verify required variables from previous step (passed via environment)
if [[ -z "${ROOT:-}" ]] || [[ -z "${EFI:-}" ]]; then
  echo "Error: Required variables not set (ROOT, EFI)"
  echo "Make sure the partition-check step completed successfully."
  exit 1
fi

echo "=== Mount and Store Setup ==="
echo "ROOT partition: $ROOT"
echo "EFI partition: $EFI"
echo ""
echo "This script will:"
echo "  1. Mount the new Guix root partition to /mnt"
echo "  2. Mount the existing ESP to /mnt/boot/efi"
echo "  3. Copy Guix store from ISO to target"
echo "  4. Set up bind mounts for /gnu and /var/guix"
echo ""
