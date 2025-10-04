#!/usr/bin/env bash
# Shared recipe: Install common fonts for programming and general use
# Can be called from any platform's customize tool

set -euo pipefail

CONFIG_FILE="${CONFIG_FILE:-/etc/config.scm}"

# Colors
info() { printf "  %s\n" "$*"; }
success() { printf "\n\033[1;32m[✓]\033[0m %s\n" "$*"; }

add_fonts() {
  echo ""
  echo "=== Installing Common Fonts ==="
  echo ""

  info "Adding font packages to config.scm..."

  # List of useful fonts
  local font_packages=(
    "font-fira-code"           # Popular programming font with ligatures
    "font-jetbrains-mono"      # JetBrains programming font
    "font-dejavu"              # DejaVu fonts (good coverage)
    "font-liberation"          # Liberation fonts (metric-compatible with Arial, etc.)
    "font-gnu-freefont"        # GNU FreeFont
    "font-awesome"             # Icon font
    "font-google-noto"         # Google Noto (comprehensive Unicode)
  )

  # Check if packages section exists
  if grep -q "(packages %base-packages)" "$CONFIG_FILE"; then
    # Minimal config - need to create packages list
    local package_list=""
    for pkg in "${font_packages[@]}"; do
      package_list+="                (specification->package \"$pkg\")\n"
    done

    sed -i "s|(packages %base-packages)|(packages\n  (append\n   (list\n${package_list}         )\n   %base-packages))|" "$CONFIG_FILE"
  else
    # Already has packages, add fonts
    for pkg in "${font_packages[@]}"; do
      if ! grep -q "\"$pkg\"" "$CONFIG_FILE"; then
        sed -i "/specification->package/a\                (specification->package \"$pkg\")" "$CONFIG_FILE"
      fi
    done
  fi

  success "Font packages added to configuration!"
  echo ""
  info "Fonts added:"
  info "  - Fira Code (programming, ligatures)"
  info "  - JetBrains Mono (programming)"
  info "  - DejaVu (general use)"
  info "  - Liberation (MS-compatible)"
  info "  - GNU FreeFont (Unicode coverage)"
  info "  - Font Awesome (icons)"
  info "  - Google Noto (comprehensive)"
  echo ""
  info "After reconfiguring, you may need to rebuild font cache:"
  info "  fc-cache -f -v"
  echo ""
  info "Apply changes with: sudo guix system reconfigure /etc/config.scm"
}

# Run if called directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  add_fonts
fi
