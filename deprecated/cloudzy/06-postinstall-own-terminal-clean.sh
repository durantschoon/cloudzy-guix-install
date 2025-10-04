#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

echo "1. Setting up Guix channels (including nonguix for additional packages):"
mkdir -p /mnt/etc/guix
cat > /mnt/etc/guix/channels.scm <<'EOF'
(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master"))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix.git")
        (branch "master")))
EOF

echo "2. Updating Guix with new channels:"
guix pull -C /mnt/etc/guix/channels.scm

echo ""
echo "3. Reconfiguring system with updated channels:"
guix system reconfigure

echo ""
echo "=== Remote Terminal Setup Complete! ==="
echo ""
echo "Your Guix system is now fully configured with:"
echo "  - Main Guix channel (official packages)"
echo "  - Nonguix channel (additional packages like proprietary software)"
echo "  - Updated system configuration"
echo ""
echo "You can now install additional packages using:"
echo "  guix install <package-name>"
echo ""
echo "Example packages from nonguix:"
echo "  guix install nonguix:firefox"
echo "  guix install nonguix:steam"
