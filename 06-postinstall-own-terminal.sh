#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

# Validate we're in the right environment
if [[ ! -d "/mnt" ]]; then
  echo "Error: /mnt directory not found. This script should be run after system installation."
  exit 1
fi

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

guix pull -C /mnt/etc/guix/channels.scm
guix system reconfigure
