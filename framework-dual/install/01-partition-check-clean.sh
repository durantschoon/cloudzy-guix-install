#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

export PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin:/run/setuid-programs:$PATH

# Verify required variables are set
if [[ -z "${DEVICE:-}" ]] || [[ -z "${EFI:-}" ]]; then
  echo "Error: Required variables not set (DEVICE, EFI)"
  echo "Make sure the warnings script ran successfully."
  exit 1
fi

# Check if a partition named 'guix-root' already exists
echo "Looking for existing partition labeled 'guix-root'..."
GUIX_ROOT_PARTNUM=$(parted "$DEVICE" print | grep "guix-root" | awk '{print $1}')

if [[ -n "$GUIX_ROOT_PARTNUM" ]]; then
  # Found existing guix-root partition
  if [[ "$DEVICE" == *"nvme"* ]] || [[ "$DEVICE" == *"mmcblk"* ]]; then
    ROOT="${DEVICE}p${GUIX_ROOT_PARTNUM}"
  else
    ROOT="${DEVICE}${GUIX_ROOT_PARTNUM}"
  fi

  echo "Found existing partition labeled 'guix-root': $ROOT"
  echo "This partition will be formatted (all data will be lost)"
  echo ""
  read -p "Type 'YES' to format $ROOT: " confirmation
  if [[ "$confirmation" != "YES" ]]; then
    echo "Aborted by user."
    exit 1
  fi

  # Format the existing partition
  echo "Formatting $ROOT as ext4..."
  mkfs.ext4 -F "$ROOT"
else
  # No existing guix-root partition, create a new one
  echo "No partition labeled 'guix-root' found."
  echo "Creating new Guix partition on $DEVICE"
  echo "Existing EFI partition: $EFI (will be reused)"

  # Get the end of the last partition to find where free space starts
  LAST_PARTITION_END=$(parted "$DEVICE" unit MiB print free | grep "^ " | tail -1 | awk '{print $2}' | sed 's/MiB//')

  # Find the first free space block that's large enough
  FREE_START=$(parted "$DEVICE" unit MiB print free | grep "Free Space" | awk '{if ($3+0 > 40000) {print $1; exit}}' | sed 's/MiB//')

  if [[ -z "$FREE_START" ]]; then
    echo "Error: Could not find suitable free space (need at least 40GB)"
    parted "$DEVICE" unit GiB print free
    exit 1
  fi

  # Create the Guix root partition in the free space
  # Use 100% of remaining space
  echo "Creating Guix root partition starting at ${FREE_START}MiB"
  parted --script "$DEVICE" \
    mkpart guix-root ext4 "${FREE_START}MiB" 100%

  # Determine the partition number that was just created
  # Get the last partition number
  if [[ "$DEVICE" == *"nvme"* ]] || [[ "$DEVICE" == *"mmcblk"* ]]; then
    # NVMe/eMMC devices use p prefix (e.g., nvme0n1p3)
    ROOT=$(lsblk -n -o NAME "$DEVICE" | grep "^${DEVICE#/dev/}p" | tail -1)
    ROOT="/dev/$ROOT"
  else
    # SATA devices don't use p prefix (e.g., sda3)
    ROOT=$(lsblk -n -o NAME "$DEVICE" | grep "^${DEVICE#/dev/}" | tail -1)
    ROOT="/dev/$ROOT"
  fi

  echo "Created Guix root partition: $ROOT"

  # Format the new Guix partition
  echo "Formatting $ROOT as ext4..."
  mkfs.ext4 -F "$ROOT"
fi

# Export variables for subsequent scripts
export ROOT
echo "ROOT is $ROOT and EFI is $EFI"

# Script completed successfully - variables are now available in main context
