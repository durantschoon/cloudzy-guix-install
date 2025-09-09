#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Validate required files exist
if [[ ! -f "/mnt/etc/config.scm" ]]; then
  echo "Error: System configuration file not found: /mnt/etc/config.scm"
  echo "Make sure you've run the previous scripts in order:"
  echo "  01-partition-clean.sh -> 02-mount-bind-clean.sh -> 03-config-write-clean.sh"
  exit 1
fi
