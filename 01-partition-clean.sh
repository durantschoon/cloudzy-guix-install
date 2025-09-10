#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

export PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin:/run/setuid-programs:$PATH

# Check if device is in use before partitioning
if mount | grep -q "^$DEVICE"; then
    echo "Error: Device $DEVICE is currently mounted. Please unmount it first."
    exit 1
fi

# Check if partitions exist and are in use
if lsblk -n -o MOUNTPOINT "$DEVICE" 2>/dev/null | grep -q -v "^$"; then
    echo "Error: Partition(s) on $DEVICE are being used."
    echo "Please unmount all partitions on $DEVICE before running this script."
    exit 1
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
echo "export EFI=$EFI" >> "/tmp/script_vars.sh"
echo "export ROOT=$ROOT" >> "/tmp/script_vars.sh"

mkfs.vfat -F32 "$EFI"
mkfs.ext4 "$ROOT"

# Write completion marker only if everything succeeded
echo "01-partition-completed" > "/tmp/01-partition-completion.marker"
