#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

export PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin:/run/setuid-programs:$PATH

if [[ -n "${DEVICE:-}" ]]; then
  if [[ ! -b "$DEVICE" ]]; then
    echo "Error: Specified device $DEVICE is not a block device"
    exit 1
  fi
  echo "Using user-specified device: $DEVICE"
else
  for d in /dev/sda /dev/vda /dev/nvme0n1; do [ -b "$d" ] && DEVICE=$d && break; done
  
  if [[ -z "${DEVICE:-}" ]]; then
    echo "Error: No suitable block device found. Expected one of: /dev/sda, /dev/vda, /dev/nvme0n1"
    echo "Available block devices:"
    lsblk -d -n -o NAME,SIZE,TYPE | grep -E '^(sd|vd|nvme)' || echo "None found"
    echo "You can override by setting DEVICE environment variable (e.g., DEVICE=/dev/sdb)"
    exit 1
  fi
  echo "Auto-detected device: $DEVICE"
fi

parted --script "$DEVICE" \
	mklabel gpt \
	mkpart ESP fat32 1MiB 513MiB \
	set 1 esp on \
	mkpart root ext4 513MiB 100%
	
EFI=${DEVICE}1
ROOT=${DEVICE}2

echo "EFI is $EFI and ROOT is $ROOT"

# Export variables for subsequent scripts
export EFI ROOT
echo "export EFI=$EFI" > "/tmp/script_vars.sh"
echo "export ROOT=$ROOT" >> "/tmp/script_vars.sh"

mkfs.vfat -F32 "$EFI"
mkfs.ext4 "$ROOT"
