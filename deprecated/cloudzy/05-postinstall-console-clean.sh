#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

echo "1. Setting root password (required for SSH access):"
passwd

echo ""
echo "2. Starting SSH daemon for remote access:"
herd start ssh-daemon

echo ""
echo "=== Setup Complete! ==="
echo ""
echo "You can now SSH into your system from another terminal:"
echo "  ssh root@<your-server-ip>"
echo ""
echo "To find your server's IP address, run:"
echo "  ip addr show | grep 'inet ' | grep -v '127.0.0.1'"
echo ""
echo "Example:"
echo "  ssh root@192.168.1.100"
echo ""
echo "Use the password you just set above."
