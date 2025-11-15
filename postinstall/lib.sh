#!/usr/bin/env bash
# postinstall/lib.sh - Shared functions for post-installation customization scripts
# Source this file from platform-specific customize scripts
# These functions run AFTER the system boots (not on the ISO)

# Colors for output
msg() { printf "\n\033[1;34m==> %s\033[0m\n" "$*"; }
warn() { printf "\n\033[1;33m[warn]\033[0m %s\n" "$*"; }
err() { printf "\n\033[1;31m[err]\033[0m  %s\n" "$*"; }
info() { printf "  %s\n" "$*"; }
success() { printf "\n\033[1;32m✓ %s\033[0m\n" "$*"; }

# Ask yes/no question
# Usage: ask_yes "prompt" [default]
# Returns 0 (true) if yes, 1 (false) if no
ask_yes() {
  local prompt="$1"
  local default="${2:-N}"
  local default_prompt="[y/N]"
  if [[ "$default" =~ ^[Yy]$ ]]; then
    default_prompt="[Y/n]"
  fi
  read -r -p "$prompt $default_prompt " ans
  if [[ -z "$ans" ]]; then
    [[ "$default" =~ ^[Yy]$ ]]
  else
    [[ "$ans" =~ ^[Yy]$ ]]
  fi
}

# Backup current config
# Sets CONFIG_FILE and BACKUP_DIR (should be set by calling script)
backup_config() {
  mkdir -p "$BACKUP_DIR"
  local backup_file="$BACKUP_DIR/config.scm.$(date +%Y%m%d-%H%M%S)"
  if [[ -w "$CONFIG_FILE" ]]; then
    cp "$CONFIG_FILE" "$backup_file"
  else
    sudo cp "$CONFIG_FILE" "$backup_file"
    sudo chown "$USER" "$backup_file"
  fi
  info "Backed up config to: $backup_file"
}

# Helper function to add service using Guile S-expression parser
# Usage: guile_add_service "module" "service-expression"
# Example: guile_add_service "(gnu services ssh)" "(service openssh-service-type)"
guile_add_service() {
  local module="$1"
  local service="$2"
  local tmp_file=$(mktemp)

  # Copy config to temp file
  if [[ -w "$CONFIG_FILE" ]]; then
    cp "$CONFIG_FILE" "$tmp_file"
  else
    sudo cp "$CONFIG_FILE" "$tmp_file"
    sudo chown "$USER" "$tmp_file"
  fi

  # Use Guile helper to add service
  # Find lib/guile-config-helper.scm relative to INSTALL_ROOT
  # INSTALL_ROOT is set by customize scripts (e.g., ~/guix-customize)
  local install_root="${INSTALL_ROOT:-}"
  if [[ -z "$install_root" ]]; then
    # Fallback: calculate from this script's location
    # postinstall/lib.sh is at INSTALL_ROOT/postinstall/lib.sh
    local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    install_root="$(cd "$script_dir/.." && pwd)"
  fi
  local guile_helper="$install_root/lib/guile-config-helper.scm"

  if guile --no-auto-compile -s "$guile_helper" add-service "$tmp_file" "$module" "$service"; then
    # Copy back
    if [[ -w "$CONFIG_FILE" ]]; then
      cp "$tmp_file" "$CONFIG_FILE"
    else
      sudo cp "$tmp_file" "$CONFIG_FILE"
    fi
    rm -f "$tmp_file"
    return 0
  else
    rm -f "$tmp_file"
    return 1
  fi
}

# Safe sed-based config editing helper
# Usage: safe_edit_config "sed command"
# Example: safe_edit_config 's|(packages %base-packages)|(packages (append ...))|'
safe_edit_config() {
  local sed_cmd="$1"
  local tmp_file=$(mktemp)

  # Copy config to temp file (readable by user)
  if [[ -w "$CONFIG_FILE" ]]; then
    cp "$CONFIG_FILE" "$tmp_file"
  else
    sudo cp "$CONFIG_FILE" "$tmp_file"
    sudo chown "$USER" "$tmp_file"
  fi

  # Apply sed command to temp file
  eval "sed -i '$sed_cmd' '$tmp_file'"

  # Copy back
  if [[ -w "$CONFIG_FILE" ]]; then
    cp "$tmp_file" "$CONFIG_FILE"
  else
    sudo cp "$tmp_file" "$CONFIG_FILE"
  fi
  rm -f "$tmp_file"
}

# Add SSH service
add_ssh() {
  msg "Adding SSH Service"

  if grep -q "openssh-service-type" "$CONFIG_FILE"; then
    warn "SSH service already configured"
    return
  fi

  backup_config

  # Use Guile helper to add SSH service (preferred method)
  if guile_add_service "(gnu services ssh)" "(service openssh-service-type)"; then
    info "✓ SSH service added"
    info "After reconfigure, SSH will be available on port 22"
  else
    err "Failed to add SSH service"
    err "Please add SSH manually to /etc/config.scm"
    return 1
  fi
}

# Add desktop environment
add_desktop() {
  msg "Desktop Environment Selection"
  echo ""
  echo "Available desktop environments:"
  echo "  1) GNOME   - Full-featured, modern desktop"
  echo "  2) Xfce    - Lightweight, traditional desktop"
  echo "  3) MATE    - Classic GNOME 2 experience"
  echo "  4) LXQt    - Very lightweight, minimal resources"
  echo "  0) Cancel"
  echo ""
  read -r -p "Select desktop [1-4, 0 to cancel]: " choice

  case "$choice" in
    1) desktop="gnome"; service="gnome-desktop-service-type" ;;
    2) desktop="xfce"; service="xfce-desktop-service-type" ;;
    3) desktop="mate"; service="mate-desktop-service-type" ;;
    4) desktop="lxqt"; service="lxqt-desktop-service-type" ;;
    0) info "Cancelled"; return ;;
    *) err "Invalid choice"; return 1 ;;
  esac

  if grep -q "desktop-service-type" "$CONFIG_FILE"; then
    warn "Desktop environment already configured"
    return
  fi

  backup_config

  # Use Guile helper to add desktop service
  if guile_add_service "(gnu services desktop)" "(service $service)"; then
    info "✓ $desktop desktop added"
  else
    err "Failed to add desktop service"
    err "Please add $desktop manually to /etc/config.scm"
    return 1
  fi
}

# Add common packages
add_packages() {
  msg "Adding Common Packages"

  if grep -q "specification->package" "$CONFIG_FILE"; then
    warn "Custom packages already defined"
    return
  fi

  backup_config

  # Replace minimal packages with useful defaults
  safe_edit_config 's|(packages %base-packages)|(packages\n  (append (list (specification->package "emacs")\n                (specification->package "git")\n                (specification->package "vim")\n                (specification->package "htop")\n                (specification->package "curl")\n                (specification->package "wget")\n                (specification->package "go"))\n          %base-packages))|'

  info "✓ Added: emacs, git, vim, htop, curl, wget, go"
}

# Add nonguix channel info
add_nonguix_info() {
  msg "Nonguix Channel (for proprietary software/firmware)"
  echo ""
  info "Nonguix provides:"
  info "  - Proprietary firmware (WiFi, graphics drivers)"
  info "  - Non-free software (Steam, Discord, etc.)"
  echo ""

  # Source postinstall library for generate_channels_scm
  # Use INSTALL_ROOT if set, otherwise calculate from script location
  local install_root="${INSTALL_ROOT:-}"
  if [[ -z "$install_root" ]]; then
    # Fallback: calculate from this script's location
    # postinstall/lib.sh is at INSTALL_ROOT/postinstall/lib.sh
    local script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    install_root="$(cd "$script_dir/.." && pwd)"
  fi
  if [[ -f "$install_root/lib/postinstall.sh" ]]; then
    source "$install_root/lib/postinstall.sh"
  fi

  # Generate channels.scm with regional mirrors
  local channels_file="$HOME/.config/guix/channels.scm"
  mkdir -p "$(dirname "$channels_file")"

  if ask_yes "Generate channels.scm with regional mirror optimization?" "Y"; then
    generate_channels_scm > "$channels_file"
    success "Created $channels_file with optimized mirrors"
    echo ""
    cat "$channels_file"
    echo ""
    info "Then run: guix pull && sudo guix system reconfigure /etc/config.scm"
  else
    info "To add nonguix channel manually, create ~/.config/guix/channels.scm:"
    echo ""
    cat <<'EOF'
(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)
EOF
    echo ""
    info "Then run: guix pull && sudo guix system reconfigure /etc/config.scm"
  fi
}

# Apply changes (reconfigure system)
reconfigure() {
  msg "System Reconfigure"
  echo ""
  info "This will apply all changes to your system."
  info "Current config: $CONFIG_FILE"
  echo ""

  if ! ask_yes "Proceed with system reconfigure?"; then
    info "Cancelled"
    return
  fi

  sudo guix system reconfigure "$CONFIG_FILE"
}

# Edit config file with fallback editors
edit_config() {
  if command -v "${EDITOR}" >/dev/null 2>&1; then
    "${EDITOR}" "$CONFIG_FILE"
  elif command -v nano >/dev/null 2>&1; then
    nano "$CONFIG_FILE"
  elif command -v vi >/dev/null 2>&1; then
    vi "$CONFIG_FILE"
  else
    err "No editor found. Install nano or set EDITOR variable"
    return 1
  fi
}

# View config file with fallback pagers
view_config() {
  if command -v less >/dev/null 2>&1; then
    less "$CONFIG_FILE"
  elif command -v more >/dev/null 2>&1; then
    more "$CONFIG_FILE"
  else
    cat "$CONFIG_FILE" | head -100
    info "Install 'less' for better viewing (currently showing first 100 lines)"
    read -p "Press Enter to continue..."
  fi
}

# Clear screen with fallback
clear_screen() {
  command -v clear >/dev/null 2>&1 && clear || printf "\n\n========================================\n\n"
}

