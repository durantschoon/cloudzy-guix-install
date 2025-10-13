#!/usr/bin/env bash
# Shared recipe: Install and configure Spacemacs
# Can be called from any platform's customize tool

set -euo pipefail

CONFIG_FILE="${CONFIG_FILE:-/etc/config.scm}"

# Colors
info() { printf "  %s\n" "$*"; }
warn() { printf "\n\033[1;33m[warn]\033[0m %s\n" "$*"; }
success() { printf "\n\033[1;32m[✓]\033[0m %s\n" "$*"; }

add_spacemacs() {
  echo ""
  echo "=== Installing Spacemacs ==="
  echo ""

  # Check if Emacs is already installed
  if ! grep -q "emacs" "$CONFIG_FILE"; then
    info "Adding Emacs package to config.scm..."

    # Add use-package-modules declaration if not present
    if ! grep -q "(use-package-modules" "$CONFIG_FILE"; then
      # Add after use-modules line
      sed -i '/(use-modules/a\             (gnu packages emacs)\n             (gnu packages version-control))' "$CONFIG_FILE"
    else
      # Add to existing use-package-modules if emacs not there
      if ! grep -q "gnu packages emacs" "$CONFIG_FILE"; then
        sed -i '/(use-modules/a\             (gnu packages emacs)\n             (gnu packages version-control))' "$CONFIG_FILE"
      fi
    fi

    # Add emacs to packages
    if grep -q "(packages %base-packages)" "$CONFIG_FILE"; then
      # Minimal config - expand to use append
      sed -i 's|(packages %base-packages)|(packages\n  (append\n   (list emacs\n         git)\n   %base-packages))|' "$CONFIG_FILE"
    else
      # Already has packages list, add emacs to it
      sed -i '/(packages/,/))/ { /list/ { s/list/list emacs\n         git/ } }' "$CONFIG_FILE"
    fi

    info "Emacs added to configuration"
  else
    info "Emacs already in configuration"
  fi

  # Ask if user wants to import existing config
  echo ""
  read -r -p "Do you have an existing Spacemacs config (.spacemacs) to import? [y/N] " import_config </dev/tty

  if [[ "$import_config" =~ ^[Yy]$ ]]; then
    echo ""
    info "Import options:"
    info "  1. Git repository URL (for .spacemacs file)"
    info "  2. Skip import (install fresh, import manually later)"
    echo ""
    read -r -p "Enter choice [1-2]: " choice </dev/tty

    case "$choice" in
      1)
        read -r -p "Enter Git repository URL (e.g., https://github.com/user/dotfiles): " repo_url </dev/tty
        read -r -p "Path to .spacemacs in repo (e.g., .spacemacs or emacs/.spacemacs): " spacemacs_path </dev/tty
        if [[ -n "$repo_url" ]]; then
          info "Will import .spacemacs from: $repo_url"
          IMPORT_SPACEMACS_REPO="$repo_url"
          IMPORT_SPACEMACS_PATH="${spacemacs_path:-.spacemacs}"
        else
          warn "No URL provided, will create default config"
        fi
        ;;
      2)
        info "Skipping import - you can manually import later"
        info "See: ~/guix-customize/EMACS_IMPORT_GUIDE.md"
        ;;
      *)
        warn "Invalid choice, will create default config"
        ;;
    esac
  fi

  # Install Spacemacs
  info "Installing Spacemacs to ~/.emacs.d..."

  if [[ -d "$HOME/.emacs.d" ]]; then
    warn "~/.emacs.d already exists"
    read -r -p "Backup and replace with Spacemacs? [y/N] " ans </dev/tty
    if [[ "$ans" =~ ^[Yy]$ ]]; then
      mv "$HOME/.emacs.d" "$HOME/.emacs.d.backup.$(date +%Y%m%d-%H%M%S)"
      info "Backed up existing .emacs.d"
    else
      info "Skipping Spacemacs installation"
      return 0
    fi
  fi

  git clone https://github.com/syl20bnr/spacemacs "$HOME/.emacs.d"

  # Import user's .spacemacs or create default
  if [[ -n "$IMPORT_SPACEMACS_REPO" ]]; then
    info "Importing your .spacemacs from repository..."
    if git clone "$IMPORT_SPACEMACS_REPO" "$HOME/.spacemacs-tmp"; then
      if [[ -f "$HOME/.spacemacs-tmp/$IMPORT_SPACEMACS_PATH" ]]; then
        cp "$HOME/.spacemacs-tmp/$IMPORT_SPACEMACS_PATH" "$HOME/.spacemacs"
        rm -rf "$HOME/.spacemacs-tmp"
        success ".spacemacs imported successfully!"
      else
        warn "Could not find $IMPORT_SPACEMACS_PATH in repository"
        rm -rf "$HOME/.spacemacs-tmp"
        IMPORT_SPACEMACS_REPO=""
      fi
    else
      warn "Failed to clone repository, creating default config instead"
      IMPORT_SPACEMACS_REPO=""
    fi
  fi

  # Create basic .spacemacs configuration if not imported
  if [[ -z "$IMPORT_SPACEMACS_REPO" ]] && [[ ! -f "$HOME/.spacemacs" ]]; then
    info "Creating basic .spacemacs configuration..."
    cat > "$HOME/.spacemacs" <<'EOF'
;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; Basic Spacemacs configuration

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()

   dotspacemacs-configuration-layers
   '(
     ;; Add your layers here
     helm
     auto-completion
     better-defaults
     emacs-lisp
     git
     markdown
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     syntax-checking
     version-control
     )

   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                 (projects . 7))
   dotspacemacs-themes '(spacemacs-dark
                          spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                                :size 13
                                :weight normal
                                :width normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.")

(defun dotspacemacs/user-config ()
  "Configuration function for user code.")

;; Do not write anything past this comment.
EOF
    success "Created .spacemacs configuration"
  else
    info ".spacemacs already exists, keeping your configuration"
  fi

  success "Spacemacs installation complete!"
  echo ""
  info "Next steps:"
  info "  1. Apply config: sudo guix system reconfigure /etc/config.scm"
  info "  2. Launch Emacs to complete Spacemacs setup"
  if [[ -z "$IMPORT_SPACEMACS_REPO" ]]; then
    info "  3. First launch will download and install packages (may take a while)"
  else
    info "  3. Imported config will install its packages on first launch"
  fi
  echo ""
  info "Spacemacs quick start:"
  info "  SPC f f - Find file"
  info "  SPC p f - Find file in project"
  info "  SPC b b - Switch buffer"
  info "  SPC w / - Split window vertically"
  info "  SPC w - - Split window horizontally"
  info "  SPC q q - Quit"
  echo ""
  if [[ -z "$IMPORT_SPACEMACS_REPO" ]]; then
    info "To import your config later, see: ~/guix-customize/EMACS_IMPORT_GUIDE.md"
  fi
  echo ""
}

# Run if called directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  add_spacemacs
fi
