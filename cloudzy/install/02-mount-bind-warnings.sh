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
  echo "These variables should be set by the previous script (01-partition-clean.sh)"
  echo "Make sure you're running the scripts in order."
  exit 1
fi
