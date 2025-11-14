#!/run/current-system/profile/bin/bash
set -euo pipefail

# Clean Install Script
# Removes all installer artifacts to prepare for a fresh installation
# Run this from the Guix ISO before re-running the installer

echo "=== Guix Installer Cleanup ==="
echo ""
echo "This script will remove all installer artifacts to prepare for a clean reinstall."
echo ""
echo "Files that will be removed:"
echo "  - /mnt/etc/config.scm (generated configuration)"
echo "  - /tmp/channels.scm (nonguix channel configuration)"
echo "  - /root/bootstrap-installer.sh (downloaded bootstrap script)"
echo "  - /root/bootstrap.sh (alternative name, if present)"
echo "  - /root/run-remote-steps (compiled installer binary)"
echo "  - /root/guix-init-time-machine.sh (helper script)"
echo "  - /root/recovery-complete-install.sh (recovery script)"
echo "  - /tmp/guix-install.log (installation log)"
echo ""
echo "WARNING: This will NOT remove:"
echo "  - Partitions or filesystem data"
echo "  - /mnt/gnu/store/ (already downloaded packages)"
echo "  - /mnt/boot/ files (if any exist)"
echo ""
read -p "Continue with cleanup? [y/N] " -r </dev/tty
echo ""

response=$(echo "$REPLY" | tr '[:upper:]' '[:lower:]')
if [[ ! "$response" =~ ^(y|yes)$ ]]; then
    echo "Cleanup cancelled."
    exit 0
fi

echo "Starting cleanup..."
echo ""

# Track what was actually removed
removed=()
not_found=()

# Function to remove file if it exists
remove_if_exists() {
    local file="$1"
    if [ -f "$file" ]; then
        rm -f "$file"
        removed+=("$file")
        echo "[REMOVED] $file"
    else
        not_found+=("$file")
        echo "[NOT FOUND] $file"
    fi
}

# Remove generated configuration files
echo "=== Configuration Files ==="
remove_if_exists "/mnt/etc/config.scm"
remove_if_exists "/tmp/channels.scm"
echo ""

# Remove bootstrap scripts (both possible names)
echo "=== Bootstrap Scripts ==="
remove_if_exists "/root/bootstrap-installer.sh"
remove_if_exists "/root/bootstrap.sh"
echo ""

# Remove compiled binary and helper scripts
echo "=== Compiled Binaries and Helper Scripts ==="
remove_if_exists "/root/run-remote-steps"
remove_if_exists "/root/guix-init-time-machine.sh"
remove_if_exists "/root/recovery-complete-install.sh"
echo ""

# Remove installation logs
echo "=== Installation Logs ==="
remove_if_exists "/tmp/guix-install.log"
echo ""

# Summary
echo "=== Cleanup Summary ==="
echo "Removed: ${#removed[@]} files"
echo "Not found: ${#not_found[@]} files"
echo ""

if [ ${#removed[@]} -gt 0 ]; then
    echo "Files removed:"
    for file in "${removed[@]}"; do
        echo "  - $file"
    done
    echo ""
fi

echo "[OK] Cleanup complete!"
echo ""
echo "Next steps:"
echo "  1. Download the latest bootstrap installer:"
echo "     curl -fsSL https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/bootstrap-installer.sh -o bootstrap-installer.sh"
echo ""
echo "  2. Run the installer with your platform:"
echo "     bash bootstrap-installer.sh framework-dual"
echo ""
echo "  3. The installer will download fresh source code and compile a new binary"
echo ""
