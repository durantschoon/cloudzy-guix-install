#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Dual-boot partition checker for Framework 13 with existing Pop!_OS
echo "=== Guix Dual-Boot Installation (Framework 13) ==="
echo "This script will create a new partition for Guix alongside Pop!_OS"
echo ""

# Check if we're running from a live ISO (expected scenario)
if [ ! -f /etc/os-release ] || ! grep -q "Guix" /etc/os-release 2>/dev/null; then
  echo "WARNING: This doesn't appear to be a Guix live ISO environment!"
  echo "   This script is designed to run from a Guix live ISO."
  echo "   If you're not sure you're in the right environment, STOP NOW!"
  echo ""
  echo "   Press Ctrl+C to abort, or wait 10 seconds to continue..."
  sleep 10
  echo ""
fi

# Detect NVMe device (Framework 13 typically uses NVMe)
if [[ -n "${DEVICE:-}" ]]; then
  if [[ ! -b "$DEVICE" ]]; then
    echo "Error: Specified device $DEVICE is not a block device"
    exit 1
  fi
  echo "Using user-specified device: $DEVICE"
else
  # Framework 13 typically uses nvme0n1
  for d in /dev/nvme0n1 /dev/nvme1n1 /dev/sda; do
    [ -b "$d" ] && DEVICE=$d && break
  done

  if [[ -z "${DEVICE:-}" ]]; then
    echo "Error: No suitable block device found. Expected one of: /dev/nvme0n1, /dev/nvme1n1, /dev/sda"
    echo "Available block devices:"
    lsblk -d -n -o NAME,SIZE,TYPE | grep -E '^(nvme|sd)' || echo "None found"
    echo "You can override by setting DEVICE environment variable (e.g., DEVICE=/dev/nvme0n1)"
    exit 1
  fi
  echo "Auto-detected device: $DEVICE"
fi

# Show current partition layout
echo ""
echo "=== Current Partition Layout ==="
lsblk "$DEVICE" -o NAME,SIZE,TYPE,FSTYPE,MOUNTPOINT,LABEL
echo ""

# Check for existing EFI System Partition
echo "=== Checking for existing EFI partition ==="
EFI_PARTITIONS=$(lsblk -n -o NAME,PARTTYPE "$DEVICE" | grep -i "c12a7328-f81f-11d2-ba4b-00a0c93ec93b" | awk '{print $1}' || true)

if [[ -z "$EFI_PARTITIONS" ]]; then
  echo "ERROR: No EFI System Partition found!"
  echo "   This script requires an existing Pop!_OS installation with an ESP."
  echo "   Please install Pop!_OS first, then run this script."
  exit 1
fi

# Use the first ESP found (should only be one)
EFI_PARTITION=$(echo "$EFI_PARTITIONS" | head -1)
# Convert partition name to device path (e.g., nvme0n1p1 -> /dev/nvme0n1p1)
if [[ "$DEVICE" == *"nvme"* ]]; then
  EFI="${DEVICE}p${EFI_PARTITION##*p}"
else
  EFI="${DEVICE}${EFI_PARTITION##*[a-z]}"
fi

echo "Found existing EFI partition: $EFI"

# Check for existing Pop!_OS installation
echo ""
echo "=== Checking for Pop!_OS installation ==="
POPOS_FOUND=false
lsblk -n -o NAME,FSTYPE,LABEL "$DEVICE" | while read name fstype label; do
  if [[ "$label" == *"Pop_OS"* ]] || [[ "$label" == *"PopOS"* ]]; then
    POPOS_FOUND=true
    echo "Found Pop!_OS partition: /dev/$name ($label)"
  fi
done

# Check if a partition named 'guix-root' already exists
echo ""
echo "=== Checking for existing guix-root partition ==="
GUIX_ROOT_PARTNUM=$(parted "$DEVICE" print | grep "guix-root" | awk '{print $1}')

if [[ -n "$GUIX_ROOT_PARTNUM" ]]; then
  # Found existing guix-root partition
  if [[ "$DEVICE" == *"nvme"* ]] || [[ "$DEVICE" == *"mmcblk"* ]]; then
    GUIX_ROOT_PART="${DEVICE}p${GUIX_ROOT_PARTNUM}"
  else
    GUIX_ROOT_PART="${DEVICE}${GUIX_ROOT_PARTNUM}"
  fi

  echo "Found existing partition labeled 'guix-root': $GUIX_ROOT_PART"
  PART_SIZE=$(lsblk -b -n -o SIZE "$GUIX_ROOT_PART" | awk '{printf "%.1f", $1/1024/1024/1024}')
  echo "Partition size: ${PART_SIZE}GiB"
  echo ""
  echo "=== Partition Plan ==="
  echo "This script will:"
  echo "  1. Keep existing EFI partition: $EFI"
  echo "  2. Keep all existing Pop!_OS partitions (untouched)"
  echo "  3. Format existing guix-root partition: $GUIX_ROOT_PART (${PART_SIZE}GiB)"
  echo "  4. Install Guix bootloader to ESP (will chain to Pop!_OS)"
else
  # No existing guix-root partition, need to create one
  echo "No partition labeled 'guix-root' found."

  # Show free space
  echo ""
  echo "=== Available Free Space ==="
  parted "$DEVICE" unit GiB print free | grep "Free Space" || echo "No unallocated space found"
  echo ""

  # Check if there's sufficient free space (at least 40GB recommended)
  FREE_SPACE_GB=$(parted "$DEVICE" unit GiB print free 2>/dev/null | grep "Free Space" | tail -1 | awk '{print $3}' | sed 's/GiB//' || echo "0")
  if (( $(echo "$FREE_SPACE_GB < 40" | bc -l) )); then
    echo "WARNING: Less than 40GB of free space available (found: ${FREE_SPACE_GB}GiB)"
    echo "   Recommended: At least 40-60GB for Guix root partition"
    echo "   You may need to shrink your Pop!_OS partition first."
    echo "   OR: Create a partition labeled 'guix-root' using GParted/parted"
    echo ""
    echo "   Press Ctrl+C to abort, or wait 10 seconds to continue anyway..."
    sleep 10
    echo ""
  fi

  echo "=== Partition Plan ==="
  echo "This script will:"
  echo "  1. Keep existing EFI partition: $EFI"
  echo "  2. Keep all existing Pop!_OS partitions (untouched)"
  echo "  3. Create a new partition for Guix in available free space"
  echo "  4. Install Guix bootloader to ESP (will chain to Pop!_OS)"
fi
echo ""
echo "IMPORTANT: Make sure you have backed up your data!"
echo "   While this script tries to be safe, disk operations are always risky."
echo ""

# Check for a separate home partition
echo ""
echo "=== Checking for separate home partition ==="
# Look for partition mounted at /home or labeled 'home'
HOME_PARTITION=""

# Check for partition labeled 'home' (case insensitive)
HOME_PARTNUM=$(parted "$DEVICE" print | grep -i "home" | awk '{print $1}' | head -1)
if [[ -n "$HOME_PARTNUM" ]]; then
  if [[ "$DEVICE" == *"nvme"* ]] || [[ "$DEVICE" == *"mmcblk"* ]]; then
    HOME_PARTITION="${DEVICE}p${HOME_PARTNUM}"
  else
    HOME_PARTITION="${DEVICE}${HOME_PARTNUM}"
  fi
  echo "Found home partition: $HOME_PARTITION"
  HOME_SIZE=$(lsblk -b -n -o SIZE "$HOME_PARTITION" 2>/dev/null | awk '{printf "%.1f", $1/1024/1024/1024}')
  echo "Home partition size: ${HOME_SIZE}GiB"
  echo "This partition will be mounted at /home and shared between Pop!_OS and Guix"
else
  echo "No separate home partition found - home directories will be in root partition"
fi

# Export variables for the clean script
export DEVICE EFI HOME_PARTITION
