#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Validate required environment variables
required_vars=("ROOT" "EFI")
missing_vars=()

for var in "${required_vars[@]}"; do
  if [[ -z "${!var:-}" ]]; then
    missing_vars+=("$var")
  fi
done

if [[ ${#missing_vars[@]} -gt 0 ]]; then
  echo "Error: Missing required environment variables:"
  printf "  - %s\n" "${missing_vars[@]}"
  echo ""
  echo "These variables should be set by the previous script (01-partition.sh)"
  echo "Make sure you're running the scripts in order."
  exit 1
fi

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
  rsync -aHAX --info=progress2 /gnu/store/. /mnt/gnu/store/.
else
  cp -a /gnu/store/. /mnt/gnu/store/
fi

end=$(date +%s)
echo "Time taken: $((end - start)) seconds"

mkdir -p /mnt/boot/efi
mount $EFI /mnt/boot/efi

mkdir -p /mnt/gnu /mnt/var/guix
herd stop guix-daemon

mount --bind /mnt/gnu /gnu
mount --bind /mnt/var/guix /var/guix

df -h /gnu /var/guix

herd start guix-daemon
