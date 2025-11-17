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
  
  # Debug: verify path exists
  if [[ ! -f "$guile_helper" ]]; then
    err "guile-config-helper.scm not found at: $guile_helper"
    err "INSTALL_ROOT: ${INSTALL_ROOT:-<not set>}"
    err "Calculated install_root: $install_root"
    return 1
  fi

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

  # Check if we need to switch from %base-services to %desktop-services
  # Desktop environments require %desktop-services for proper display manager support
  local needs_desktop_services=false
  if grep -q "%base-services" "$CONFIG_FILE" && ! grep -q "%desktop-services" "$CONFIG_FILE"; then
    needs_desktop_services=true
    info "Switching from %base-services to %desktop-services for desktop support..."
    
    # Replace %base-services with %desktop-services
    if safe_edit_config 's|%base-services|%desktop-services|g'; then
      info "✓ Switched to %desktop-services"
      info "  Note: %desktop-services includes NetworkManager and wpa-supplicant"
      info "        You may want to remove explicit NetworkManager/wpa-supplicant entries"
    else
      warn "Could not automatically switch to %desktop-services"
      warn "Please manually change %base-services to %desktop-services in config.scm"
      warn "This is required for display manager (GDM/LightDM) to start properly"
    fi
  fi

  # Use Guile helper to add desktop service
  if guile_add_service "(gnu services desktop)" "(service $service)"; then
    info "✓ $desktop desktop added"
    echo ""
    
    # Warn about NetworkManager if %desktop-services is used
    if [ "$needs_desktop_services" = true ] || grep -q "%desktop-services" "$CONFIG_FILE"; then
      if grep -q "network-manager-service-type\|wpa-supplicant-service-type" "$CONFIG_FILE"; then
        warn "Note: %desktop-services already includes NetworkManager and wpa-supplicant"
        warn "      You may have duplicate entries - remove explicit NetworkManager/wpa-supplicant"
        warn "      to avoid 'service provided more than once' errors"
      fi
    fi
    
    # For GNOME: Check if keyboard layout has options and configure accordingly
    if [ "$desktop" = "gnome" ]; then
      # Check if keyboard layout has options (like ctrl:swapcaps)
      if grep -q "keyboard-layout" "$CONFIG_FILE" && grep -q "#:options" "$CONFIG_FILE"; then
        info "Detected keyboard layout with options - configuring GNOME keyboard layout..."
        
        # Extract keyboard layout and options from config.scm
        # Look for pattern like: (keyboard-layout "us" #:options '("ctrl:swapcaps"))
        local layout=$(grep -A 3 "keyboard-layout" "$CONFIG_FILE" | grep -oP 'keyboard-layout "\K[^"]+' | head -1)
        local options=$(grep -A 3 "keyboard-layout" "$CONFIG_FILE" | grep -oP '#:options.*"\K[^"]+' | head -1)
        
        if [ -n "$layout" ] && [ -n "$options" ]; then
          # Add setxkbmap package if not already present
          if ! grep -q "setxkbmap" "$CONFIG_FILE"; then
            info "Adding setxkbmap package for GNOME keyboard layout configuration..."
            
            # Add setxkbmap to packages
            if grep -q "(packages %base-packages)" "$CONFIG_FILE"; then
              # Minimal config - expand to use append
              safe_edit_config 's|(packages %base-packages)|(packages\n  (append (list (specification->package "setxkbmap"))\n          %base-packages))|'
            elif grep -q "specification->package" "$CONFIG_FILE"; then
              # Already has packages, add setxkbmap if not present
              if ! grep -q '"setxkbmap"' "$CONFIG_FILE"; then
                safe_edit_config '/specification->package/a\                (specification->package "setxkbmap")'
              fi
            fi
            info "✓ setxkbmap package added"
          fi
          
          # Create GNOME autostart script to set keyboard layout (if it doesn't exist)
          local autostart_dir="$HOME/.config/autostart"
          local autostart_file="$autostart_dir/keyboard-layout.desktop"
          
          if [ ! -f "$autostart_file" ]; then
            mkdir -p "$autostart_dir"
            
            # Convert options format (e.g., "ctrl:swapcaps" -> "-option ctrl:swapcaps")
            local setxkbmap_options="-option $options"
            
            cat > "$autostart_file" << EOF
[Desktop Entry]
Type=Application
Name=Set Keyboard Layout
Exec=setxkbmap $layout $setxkbmap_options
Hidden=false
NoDisplay=false
X-GNOME-Autostart-enabled=true
EOF
            
            info "✓ GNOME autostart script created: ~/.config/autostart/keyboard-layout.desktop"
            info "  This will automatically set keyboard layout ($layout with $options) when GNOME starts"
            echo ""
          else
            info "GNOME autostart script already exists - skipping creation"
          fi
        else
          warn "Could not extract keyboard layout/options from config.scm"
          warn "You may need to manually configure GNOME keyboard layout"
        fi
      elif grep -q "keyboard-layout" "$CONFIG_FILE"; then
        # Keyboard layout exists but no options - still might want to configure GNOME
        info "Keyboard layout detected without options - GNOME should inherit system layout"
      fi
    fi
    
    echo ""
    info "IMPORTANT: After running 'r' to reconfigure, you will need to:"
    info "  1. Log out of your current session"
    info "  2. Log back in at the graphical login screen"
    info "  3. Select '$desktop' from the session menu (if multiple desktops installed)"
    echo ""
    info "The display manager will start automatically after reconfigure."
    info ""
    info "If you see text login instead of graphical login after reconfigure:"
    info "  - Check service status: sudo herd status"
    info "  - Look for 'gdm' service (for GNOME) or 'lightdm' (for Xfce/MATE/LXQt)"
    info "  - Try starting manually: sudo herd start gdm"
  else
    err "Failed to add desktop service"
    err "Please add $desktop manually to /etc/config.scm"
    return 1
  fi
}

# Add console font configuration (for high-DPI displays)
add_console_font() {
  msg "Console Font Configuration"
  echo ""
  echo "This will configure a larger console font for all TTYs (tty1-tty6)."
  echo "Recommended for high-DPI displays like Framework 13."
  echo ""
  
  if grep -q "console-font-service-type" "$CONFIG_FILE"; then
    warn "Console font already configured"
    return
  fi
  
  # List available large fonts (in multiple columns)
  echo "Available large fonts:"
  if [ -d "/run/current-system/profile/share/consolefonts" ]; then
    # Get list of fonts and display in columns
    FONTS=($(ls /run/current-system/profile/share/consolefonts/ 2>/dev/null | grep -E '24|32|36' | head -10 | sed 's/\.psf.*$//' | sort -u))
    if [ ${#FONTS[@]} -gt 0 ]; then
      COLS=3
      ROWS=$(( (${#FONTS[@]} + COLS - 1) / COLS ))
      for ((row=0; row<ROWS; row++)); do
        printf "  "
        for ((col=0; col<COLS; col++)); do
          idx=$((row + col * ROWS))
          if [ $idx -lt ${#FONTS[@]} ]; then
            printf "%-25s" "${FONTS[$idx]}"
          fi
        done
        echo ""
      done
    else
      echo "  (No large fonts found - will use default)"
    fi
  else
    echo "  (Font directory not found - will use default)"
  fi
  echo ""
  
  read -r -p "Enter font name (default: solar24x32): " font_choice
  font_name="${font_choice:-solar24x32}"
  
  backup_config
  
  # Add console-font-service-type module if needed
  if ! grep -q "(gnu services base)" "$CONFIG_FILE"; then
    safe_edit_config '/^(use-modules/a\             (gnu services base)'
  fi
  
  # Use Guile helper to add console font service
  # Format: (service console-font-service-type (map (lambda (tty) (cons tty "font-name")) '("tty1" "tty2" ...)))
  local font_service_expr="(service console-font-service-type (map (lambda (tty) (cons tty \"$font_name\")) '(\"tty1\" \"tty2\" \"tty3\" \"tty4\" \"tty5\" \"tty6\")))"
  
  if guile_add_service "(gnu services base)" "$font_service_expr"; then
    info "✓ Console font '$font_name' configured for all TTYs"
    info "Font will be applied after running 'r' to reconfigure"
  else
    err "Failed to add console font service"
    err "You can add it manually to /etc/config.scm:"
    echo ""
    echo "  (service console-font-service-type"
    echo "           (map (lambda (tty)"
    echo "                  (cons tty \"$font_name\"))"
    echo "                '(\"tty1\" \"tty2\" \"tty3\" \"tty4\" \"tty5\" \"tty6\")))"
    echo ""
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

  # Check if desktop environment is configured
  local has_desktop=false
  if grep -q "desktop-service-type" "$CONFIG_FILE"; then
    has_desktop=true
  fi

  sudo guix system reconfigure "$CONFIG_FILE"
  
  # Remind user about logging out if desktop was configured
  if [ "$has_desktop" = true ]; then
    echo ""
    info "=========================================="
    info "Desktop Environment Configured"
    info "=========================================="
    echo ""
    info "To start using your desktop environment:"
    info "  1. Log out of your current session"
    info "  2. Log back in at the graphical login screen"
    info "  3. Select your desktop from the session menu (if multiple installed)"
    echo ""
    info "The display manager should now be running."
    info "If you're on a text console, switch to tty1 or tty7 to see the login screen."
    echo ""
  fi
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

