#!/usr/bin/env bash
set -euo pipefail  # Safety: exit on error, undefined vars, and pipeline failures

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
