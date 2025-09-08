#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Validate required files exist
if [[ ! -f "/mnt/etc/config.scm" ]]; then
  echo "Error: System configuration file not found: /mnt/etc/config.scm"
  echo "Make sure you've run the previous scripts in order:"
  echo "  01-partition.sh -> 02-mount-bind.sh -> 03-config-write.sh"
  exit 1
fi

export TMPDIR=/mnt/var/tmp
mkdir -p "$TMPDIR" && chmod 1777 "$TMPDIR"
rm -rf /var/guix/substitute-cache/* 2>/dev/null

# Set default swap size if not provided (4G is good for most VPS configurations)
SWAP_SIZE="${SWAP_SIZE:-4G}"

# Convert swap size to bytes for fallocate, or use dd as fallback
if [[ "$SWAP_SIZE" =~ ^([0-9]+)([GMK]?)$ ]]; then
  size_num="${BASH_REMATCH[1]}"
  size_unit="${BASH_REMATCH[2]}"
  
  case "$size_unit" in
    G) size_bytes=$((size_num * 1024 * 1024 * 1024)) ;;
    M) size_bytes=$((size_num * 1024 * 1024)) ;;
    K) size_bytes=$((size_num * 1024)) ;;
    *) size_bytes=$((size_num * 1024 * 1024 * 1024)) ;; # Default to GB
  esac
  
  echo "Creating ${SWAP_SIZE} swap file..."
  fallocate -l "$size_bytes" /mnt/swapfile || dd if=/dev/zero of=/mnt/swapfile bs=1M count=$((size_bytes / 1024 / 1024))
else
  echo "Warning: Invalid SWAP_SIZE format '$SWAP_SIZE', using default 4G"
  fallocate -l 4G /mnt/swapfile || dd if=/dev/zero of=/mnt/swapfile bs=1M count=4096
fi
chmod 600 /mnt/swapfile
mkswap /mnt/swapfile
swapon /mnt/swapfile

echo "Verify swap is active and memory is available (swapon --show and free -h):"
swapon --show
free -h

export GIT_HTTP_MAX_REQUESTS=2
export GIT_HTTP_LOW_SPEED_LIMIT=1000
export GIT_HTTP_LOW_SPEED_TIME=60

guix pull --commit="${GUIX_VERSION:-v1.4.0}"

guix system init /mnt/etc/config.scm /mnt

sync
umount -R /mnt
reboot
