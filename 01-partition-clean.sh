#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

export PATH=/run/current-system/profile/bin:/run/current-system/profile/sbin:/run/setuid-programs:$PATH

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
