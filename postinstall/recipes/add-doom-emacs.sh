#!/usr/bin/env bash
# Shared recipe: Install and configure Doom Emacs
# Can be called from any platform's customize tool

set -euo pipefail

CONFIG_FILE="${CONFIG_FILE:-/etc/config.scm}"

# Colors
info() { printf "  %s\n" "$*"; }
warn() { printf "\n\033[1;33m[warn]\033[0m %s\n" "$*"; }
success() { printf "\n\033[1;32m[OK]\033[0m %s\n" "$*"; }

add_doom_emacs() {
  echo ""
  echo "=== Installing Doom Emacs ==="
  echo ""

  # Check if Emacs is already installed
  if ! grep -q "emacs" "$CONFIG_FILE"; then
    info "Adding Emacs package to config.scm..."

    # Add package modules if not present
    if ! grep -q "gnu packages emacs" "$CONFIG_FILE"; then
      sed -i '/(use-modules/a\             (gnu packages emacs)\n             (gnu packages version-control)\n             (gnu packages rust-apps))' "$CONFIG_FILE"
    fi

    # Add emacs and doom dependencies to packages
    if grep -q "(packages %base-packages)" "$CONFIG_FILE"; then
      # Minimal config - expand to use append
      sed -i 's|(packages %base-packages)|(packages\n  (append\n   (list emacs\n         git\n         ripgrep\n         fd)\n   %base-packages))|' "$CONFIG_FILE"
    else
      # Already has packages list, add to it
      sed -i '/(packages/,/))/ { /list/ { s/list/list emacs\n         git\n         ripgrep\n         fd/ } }' "$CONFIG_FILE"
    fi

    info "Emacs and dependencies added to configuration"
  else
    info "Emacs already in configuration"
  fi

  # Ask if user wants to import existing config
  echo ""
  read -r -p "Do you have an existing Doom Emacs config to import? [y/N] " import_config </dev/tty

  if [[ "$import_config" =~ ^[Yy]$ ]]; then
    echo ""
    info "Import options:"
    info "  1. Git repository URL"
    info "  2. Skip import (install fresh, import manually later)"
    echo ""
    read -r -p "Enter choice [1-2]: " choice </dev/tty

    case "$choice" in
      1)
        read -r -p "Enter Git repository URL (e.g., https://github.com/user/doom-config): " repo_url </dev/tty
        if [[ -n "$repo_url" ]]; then
          info "Will import config from: $repo_url"
          IMPORT_DOOM_CONFIG="$repo_url"
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

  # Install Doom Emacs framework
  info "Installing Doom Emacs to ~/.config/emacs..."

  if [[ -d "$HOME/.config/emacs" ]] || [[ -d "$HOME/.emacs.d" ]]; then
    warn "Existing Emacs configuration found"
    read -r -p "Backup and replace with Doom Emacs? [y/N] " ans </dev/tty
    if [[ "$ans" =~ ^[Yy]$ ]]; then
      [[ -d "$HOME/.config/emacs" ]] && mv "$HOME/.config/emacs" "$HOME/.config/emacs.backup.$(date +%Y%m%d-%H%M%S)"
      [[ -d "$HOME/.emacs.d" ]] && mv "$HOME/.emacs.d" "$HOME/.emacs.d.backup.$(date +%Y%m%d-%H%M%S)"
      info "Backed up existing Emacs configuration"
    else
      info "Skipping Doom Emacs installation"
      return 0
    fi
  fi

  # Clone Doom Emacs framework
  git clone --depth 1 https://github.com/doomemacs/doomemacs "$HOME/.config/emacs"

  # Install Doom
  info "Running Doom installer..."
  "$HOME/.config/emacs/bin/doom" install --no-env --no-fonts

  # Import user's config or create default
  mkdir -p "$HOME/.config/doom"

  if [[ -n "$IMPORT_DOOM_CONFIG" ]]; then
    info "Importing your Doom config from repository..."
    if git clone "$IMPORT_DOOM_CONFIG" "$HOME/.config/doom.tmp"; then
      mv "$HOME/.config/doom.tmp"/* "$HOME/.config/doom/"
      rm -rf "$HOME/.config/doom.tmp"
      success "Config imported successfully!"
      info "Running doom sync to install packages..."
      "$HOME/.config/emacs/bin/doom" sync
    else
      warn "Failed to clone repository, creating default config instead"
      IMPORT_DOOM_CONFIG=""
    fi
  fi

  if [[ -z "$IMPORT_DOOM_CONFIG" ]] && [[ ! -f "$HOME/.config/doom/init.el" ]]; then
    info "Creating basic Doom configuration..."
    cat > "$HOME/.config/doom/init.el" <<'EOF'
;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :completion
       company
       vertico

       :ui
       doom
       doom-dashboard
       hl-todo
       modeline
       ophints
       (popup +defaults)
       treemacs
       vc-gutter
       vi-tilde-fringe
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       multiple-cursors
       rotate-text
       snippets

       :emacs
       dired
       electric
       ibuffer
       undo
       vc

       :term
       vterm

       :checkers
       syntax

       :tools
       (eval +overlay)
       lookup
       lsp
       magit
       make
       tree-sitter

       :os
       (:if IS-MAC macos)
       tty

       :lang
       (cc +lsp)
       data
       emacs-lisp
       (go +lsp)
       (javascript +lsp)
       json
       (markdown +grip)
       (org +pretty)
       (python +lsp)
       (rust +lsp)
       sh
       web
       yaml

       :email

       :app

       :config
       (default +bindings +smartparens))
EOF
  fi

  if [[ ! -f "$HOME/.config/doom/config.el" ]]; then
    cat > "$HOME/.config/doom/config.el" <<'EOF'
;;; config.el -*- lexical-binding: t; -*-

;; Basic settings
(setq user-full-name "Your Name"
      user-mail-address "your@email.com")

;; Theme
(setq doom-theme 'doom-one)

;; Line numbers
(setq display-line-numbers-type 'relative)

;; Font
(setq doom-font (font-spec :family "monospace" :size 14))
EOF
  fi

  if [[ ! -f "$HOME/.config/doom/packages.el" ]]; then
    cat > "$HOME/.config/doom/packages.el" <<'EOF'
;; -*- no-byte-compile: t; -*-
;;; packages.el

;; Add your custom packages here
EOF
  fi

  success "Doom Emacs installation complete!"
  echo ""
  info "Next steps:"
  info "  1. Apply config: sudo guix system reconfigure /etc/config.scm"
  if [[ -z "$IMPORT_DOOM_CONFIG" ]]; then
    info "  2. Run: ~/.config/emacs/bin/doom sync"
    info "  3. Launch Emacs to complete setup"
  else
    info "  2. Launch Emacs (packages already synced)"
  fi
  echo ""
  info "Doom Emacs quick start:"
  info "  SPC f f - Find file"
  info "  SPC p f - Find file in project"
  info "  SPC b b - Switch buffer"
  info "  SPC w v - Split window vertically"
  info "  SPC w s - Split window horizontally"
  info "  SPC q q - Quit"
  echo ""
  if [[ -z "$IMPORT_DOOM_CONFIG" ]]; then
    info "To import your config later, see: ~/guix-customize/EMACS_IMPORT_GUIDE.md"
  fi
  info "Documentation: https://docs.doomemacs.org"
  echo ""
}

# Run if called directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  add_doom_emacs
fi
