#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

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
