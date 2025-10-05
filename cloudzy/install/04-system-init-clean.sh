#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Source mirror configuration for regional optimization
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../../lib/mirrors.sh"

# Get best mirrors for user's region
get_mirrors

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

echo "Pulling Guix from configured mirror: $GUIX_GIT_URL"
guix pull --url="$GUIX_GIT_URL" --commit="${GUIX_VERSION:-v1.4.0}"

echo ""
echo "=== Installing Customization Tools ==="
echo "Copying customize script and recipes to /mnt/root/guix-customize/"
echo ""

# Determine platform (default to cloudzy if not set)
INSTALL_PLATFORM="${GUIX_PLATFORM:-cloudzy}"

# Set up GitHub raw URL for downloading scripts
REPO_OWNER="${GUIX_INSTALL_REPO:-durantschoon/cloudzy-guix-install}"
REPO_REF="${GUIX_INSTALL_REF:-main}"
RAW_BASE="https://raw.githubusercontent.com/${REPO_OWNER}/${REPO_REF}"

# Create destination directory
mkdir -p /mnt/root/guix-customize/recipes

# Download platform-specific customize script
echo "Downloading ${INSTALL_PLATFORM} customize script..."
if curl -fsSL "${RAW_BASE}/${INSTALL_PLATFORM}/postinstall/customize" -o /mnt/root/guix-customize/customize 2>/dev/null; then
  chmod +x /mnt/root/guix-customize/customize
  echo "✓ Customize script installed"
else
  echo "⚠ Warning: Could not download customize script (network issue?)"
  echo "  You can manually download it after first boot"
fi

# Download shared recipes
echo "Downloading shared recipes..."
for recipe in add-spacemacs.sh add-development.sh add-fonts.sh; do
  if curl -fsSL "${RAW_BASE}/postinstall/recipes/${recipe}" -o "/mnt/root/guix-customize/recipes/${recipe}" 2>/dev/null; then
    chmod +x "/mnt/root/guix-customize/recipes/${recipe}"
    echo "✓ ${recipe}"
  else
    echo "⚠ ${recipe} (skipped)"
  fi
done

# Create a helpful README
cat > /mnt/root/guix-customize/README.txt <<'EOF'
# Guix System Customization Tools

This directory contains tools to customize your minimal Guix installation.

## Quick Start

After first boot, run:

    cd ~/guix-customize
    ./customize

This will launch an interactive menu to add:
- SSH service (critical for VPS!)
- Desktop environments
- Common packages
- And more...

## Manual Customization

You can also edit /etc/config.scm directly:

    sudo nano /etc/config.scm
    sudo guix system reconfigure /etc/config.scm

## Shared Recipes

The recipes/ directory contains modular scripts:
- add-spacemacs.sh - Install Spacemacs editor
- add-development.sh - Install dev tools (git, vim, gcc, etc.)
- add-fonts.sh - Install programming and system fonts

Run them individually:

    ./recipes/add-spacemacs.sh

## Documentation

For more examples and detailed guides, see:
https://github.com/durantschoon/cloudzy-guix-install/blob/main/CUSTOMIZATION.md
EOF

echo ""
echo "✓ Customization tools installed to /root/guix-customize/"
echo "  After first boot, run: cd ~/guix-customize && ./customize"
echo ""

guix system init /mnt/etc/config.scm /mnt

sync
umount -R /mnt
reboot
