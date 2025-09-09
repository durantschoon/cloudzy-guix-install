#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Validate we're in the right environment
if [[ ! -d "/mnt" ]]; then
  echo "Error: /mnt directory not found. This script should be run after system installation."
  exit 1
fi

echo "=== Post-Installation Remote Terminal Setup ==="
echo ""
echo "Configuring additional Guix channels and updating system..."
echo ""
