#!/usr/bin/env bash
# lib/postinstall.sh - Shared functions for post-installation customization scripts
# These functions run AFTER the system boots (not on the ISO)

# Generate channels.scm with configured mirrors
# Usage: generate_channels_scm > ~/.config/guix/channels.scm
generate_channels_scm() {
  local guix_url="${GUIX_GIT_URL:-https://git.savannah.gnu.org/git/guix.git}"
  local nonguix_url="${NONGUIX_GIT_URL:-https://gitlab.com/nonguix/nonguix.git}"

  cat <<EOF
;; Guix channels configuration
;; Generated with regional mirror optimization

(list (channel
        (name 'guix)
        (url "$guix_url")
        (branch "master"))
      (channel
        (name 'nonguix)
        (url "$nonguix_url")
        (branch "master")))
EOF
}
