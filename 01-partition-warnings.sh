#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Simple detection for obvious wrong situations
echo "=== Guix Installation Script ==="
echo "WARNING: This script will DESTROY ALL DATA on the target device!"
echo ""

# Check if we're running from a live ISO (expected scenario)
if [ ! -f /etc/os-release ] || ! grep -q "Guix" /etc/os-release 2>/dev/null; then
  echo "⚠️  WARNING: This doesn't appear to be a Guix live ISO environment!"
  echo "   This script is designed to run from a Guix live ISO on a fresh VPS."
  echo "   If you're not sure you're in the right environment, STOP NOW!"
  echo ""
  echo "   Expected: Guix live ISO on a fresh VPS instance"
  echo "   Current:  $(cat /etc/os-release 2>/dev/null | head -1 || echo "Unknown system")"
  echo ""
  echo "   Press Ctrl+C to abort, or wait 10 seconds to continue..."
  sleep 10
  echo ""
fi

# Check for multiple mounted filesystems (might indicate existing system)
mounted_count=$(mount | grep -c "^/dev/" || true)
if [ "$mounted_count" -gt 3 ]; then
  echo "⚠️  WARNING: Multiple filesystems are mounted ($mounted_count found)!"
  echo "   This might indicate an existing system with data."
  echo "   Expected: Fresh VPS with minimal mounts"
  echo ""
  echo "   Current mounts:"
  mount | grep "^/dev/" | head -5
  echo "   ..."
  echo ""
  echo "   Press Ctrl+C to abort, or wait 10 seconds to continue..."
  sleep 10
  echo ""
fi

if [[ -n "${DEVICE:-}" ]]; then
  if [[ ! -b "$DEVICE" ]]; then
    echo "Error: Specified device $DEVICE is not a block device"
    exit 1
  fi
  echo "Using user-specified device: $DEVICE"
else
  # Common VPS device names in order of preference
  for d in /dev/sda /dev/vda /dev/xvda /dev/nvme0n1 /dev/nvme1n1 /dev/sdb /dev/vdb; do 
    [ -b "$d" ] && DEVICE=$d && break
  done
  
  if [[ -z "${DEVICE:-}" ]]; then
    echo "Error: No suitable block device found. Expected one of: /dev/sda, /dev/vda, /dev/xvda, /dev/nvme0n1, /dev/nvme1n1, /dev/sdb, /dev/vdb"
    echo "Available block devices:"
    lsblk -d -n -o NAME,SIZE,TYPE | grep -E '^(sd|vd|nvme)' || echo "None found"
    echo "You can override by setting DEVICE environment variable (e.g., DEVICE=/dev/sdb)"
    exit 1
  fi
  echo "Auto-detected device: $DEVICE"
fi
