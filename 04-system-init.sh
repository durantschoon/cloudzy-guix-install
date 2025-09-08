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

fallocate -l 4G /mnt/swapfile || dd if=/dev/zero of=/mnt/swapfile bs=1M count=4096
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
