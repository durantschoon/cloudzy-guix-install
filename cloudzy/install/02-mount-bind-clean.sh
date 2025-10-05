#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

mount | grep -q " on /mnt " || mount $ROOT /mnt

mkdir -p /mnt/gnu/store
mkdir -p /mnt/var/guix

if command -v herd >/dev/null 2>&1; then
  herd stop guix-daemon
else
  pkill -TERM -x guix-daemon
fi

start=$(date +%s)

if command -v rsync >/dev/null 2>&1; then
  echo "Syncing /gnu/store to /mnt/gnu/store using rsync..."
  if ! rsync -aHAX --info=progress2,stats /gnu/store/. /mnt/gnu/store/.; then
    echo "ERROR: rsync failed with exit code $?"
    exit 1
  fi
  echo "rsync completed successfully"
else
  echo "rsync not available, using cp instead..."
  if ! cp -a /gnu/store/. /mnt/gnu/store/; then
    echo "ERROR: cp failed with exit code $?"
    exit 1
  fi
  echo "cp completed successfully"
fi

end=$(date +%s)
echo "Time taken: $((end - start)) seconds"

mkdir -p /mnt/boot/efi
mount $EFI /mnt/boot/efi

# Create the bind mount targets and copy the store
mkdir -p /mnt/gnu /mnt/var/guix
echo "Copying /gnu/store to /mnt/gnu/store for bind mount..."
cp -a /gnu/store /mnt/gnu/
echo "Copying /var/guix to /mnt/var/guix for bind mount..."
cp -a /var/guix /mnt/var/

herd stop guix-daemon

echo "Setting up bind mounts..."
mount --bind /mnt/gnu /gnu
mount --bind /mnt/var/guix /var/guix

df -h /gnu /var/guix

herd start guix-daemon

# Output variables for Go program to capture and pass to next script
echo "###GUIX_INSTALL_VARS###"
# Build outgoing vars: keep incoming vars and add/update new ones
OUTGOING_VARS="${INCOMING_VARS:-} DEVICE=$DEVICE EFI=$EFI ROOT=$ROOT USER_NAME=${USER_NAME:-} FULL_NAME=${FULL_NAME:-} TIMEZONE=${TIMEZONE:-} HOST_NAME=${HOST_NAME:-} BOOT_MODE=${BOOT_MODE:-} SWAP_SIZE=${SWAP_SIZE:-}"
echo "export $OUTGOING_VARS"
