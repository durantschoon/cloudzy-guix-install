#!/usr/bin/env bash
# Shared recipe: Install common development tools
# Can be called from any platform's customize tool

set -euo pipefail

CONFIG_FILE="${CONFIG_FILE:-/etc/config.scm}"

# Colors
info() { printf "  %s\n" "$*"; }
success() { printf "\n\033[1;32m[✓]\033[0m %s\n" "$*"; }

add_development() {
  echo ""
  echo "=== Installing Development Tools ==="
  echo ""

  info "Adding development packages to config.scm..."

  # List of common development tools
  local dev_packages=(
    "git"
    "vim"
    "emacs"
    "make"
    "gcc-toolchain"
    "python"
    "node"
    "go"
    "curl"
    "wget"
    "ripgrep"
    "fd"
    "tmux"
    "htop"
  )

  # Check if packages section exists
  if grep -q "(packages %base-packages)" "$CONFIG_FILE"; then
    # Minimal config - need to create packages list
    local package_list=""
    for pkg in "${dev_packages[@]}"; do
      package_list+="                (specification->package \"$pkg\")\n"
    done

    sed -i "s|(packages %base-packages)|(packages\n  (append\n   (list\n${package_list}         )\n   %base-packages))|" "$CONFIG_FILE"
  else
    # Already has packages, add development tools
    for pkg in "${dev_packages[@]}"; do
      if ! grep -q "\"$pkg\"" "$CONFIG_FILE"; then
        # Find the packages section and add the package
        sed -i "/specification->package/a\                (specification->package \"$pkg\")" "$CONFIG_FILE"
      fi
    done
  fi

  success "Development tools added to configuration!"
  echo ""
  info "Packages added:"
  for pkg in "${dev_packages[@]}"; do
    info "  - $pkg"
  done
  echo ""
  info "Apply changes with: sudo guix system reconfigure /etc/config.scm"
}

# Run if called directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  add_development
fi
