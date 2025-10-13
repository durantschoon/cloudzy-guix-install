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

    # Add emacs to packages if not present
    if grep -q "(packages %base-packages)" "$CONFIG_FILE"; then
      # Minimal config - need to add packages list
      sed -i 's|(packages %base-packages)|(packages\n  (append\n   (list (specification->package "emacs")\n         (specification->package "git")\n         (specification->package "ripgrep")\n         (specification->package "fd"))\n   %base-packages))|' "$CONFIG_FILE"
    else
      # Already has packages, just add emacs and dependencies
      sed -i '/specification->package "emacs"/!{/(packages/a\                (specification->package "emacs")' "$CONFIG_FILE"
      sed -i '/specification->package "git"/!{/(packages/a\                (specification->package "git")' "$CONFIG_FILE"
      sed -i '/specification->package "ripgrep"/!{/(packages/a\                (specification->package "ripgrep")' "$CONFIG_FILE"
      sed -i '/specification->package "fd"/!{/(packages/a\                (specification->package "fd")' "$CONFIG_FILE"
    fi

    info "Emacs and dependencies added to configuration"
  else
    info "Emacs already in configuration"
  fi

  # Install Doom Emacs
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

  # Clone Doom Emacs
  git clone --depth 1 https://github.com/doomemacs/doomemacs "$HOME/.config/emacs"

  # Install Doom
  info "Running Doom installer..."
  "$HOME/.config/emacs/bin/doom" install --no-env --no-fonts

  # Create basic init.el if user wants to customize
  mkdir -p "$HOME/.config/doom"

  if [[ ! -f "$HOME/.config/doom/init.el" ]]; then
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
  info "  2. Run: ~/.config/emacs/bin/doom sync"
  info "  3. Launch Emacs to complete setup"
  echo ""
  info "Doom Emacs quick start:"
  info "  SPC f f - Find file"
  info "  SPC p f - Find file in project"
  info "  SPC b b - Switch buffer"
  info "  SPC w v - Split window vertically"
  info "  SPC w s - Split window horizontally"
  info "  SPC q q - Quit"
  echo ""
  info "Documentation: https://docs.doomemacs.org"
  echo ""
}

# Run if called directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  add_doom_emacs
fi
