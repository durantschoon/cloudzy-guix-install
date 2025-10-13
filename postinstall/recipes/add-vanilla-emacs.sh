#!/usr/bin/env bash
# Shared recipe: Install vanilla Emacs with minimal configuration
# Can be called from any platform's customize tool

set -euo pipefail

CONFIG_FILE="${CONFIG_FILE:-/etc/config.scm}"

# Colors
info() { printf "  %s\n" "$*"; }
warn() { printf "\n\033[1;33m[warn]\033[0m %s\n" "$*"; }
success() { printf "\n\033[1;32m[OK]\033[0m %s\n" "$*"; }

add_vanilla_emacs() {
  echo ""
  echo "=== Installing Vanilla Emacs ==="
  echo ""

  # Check if Emacs is already installed
  if ! grep -q "emacs" "$CONFIG_FILE"; then
    info "Adding Emacs package to config.scm..."

    # Add emacs to packages if not present
    if grep -q "(packages %base-packages)" "$CONFIG_FILE"; then
      # Minimal config - need to add packages list
      sed -i 's|(packages %base-packages)|(packages\n  (append\n   (list (specification->package "emacs")\n         (specification->package "git"))\n   %base-packages))|' "$CONFIG_FILE"
    else
      # Already has packages, just add emacs
      sed -i '/specification->package "emacs"/!{/(packages/a\                (specification->package "emacs")' "$CONFIG_FILE"
      sed -i '/specification->package "git"/!{/(packages/a\                (specification->package "git")' "$CONFIG_FILE"
    fi

    info "Emacs added to configuration"
  else
    info "Emacs already in configuration"
  fi

  # Create minimal Emacs configuration
  info "Creating minimal Emacs configuration..."

  if [[ -d "$HOME/.emacs.d" ]] || [[ -f "$HOME/.emacs" ]]; then
    warn "Existing Emacs configuration found"
    read -r -p "Backup and create minimal config? [y/N] " ans </dev/tty
    if [[ "$ans" =~ ^[Yy]$ ]]; then
      [[ -d "$HOME/.emacs.d" ]] && mv "$HOME/.emacs.d" "$HOME/.emacs.d.backup.$(date +%Y%m%d-%H%M%S)"
      [[ -f "$HOME/.emacs" ]] && mv "$HOME/.emacs" "$HOME/.emacs.backup.$(date +%Y%m%d-%H%M%S)"
      info "Backed up existing Emacs configuration"
    else
      info "Keeping existing configuration"
      return 0
    fi
  fi

  # Create init.el with sensible defaults
  mkdir -p "$HOME/.emacs.d"
  cat > "$HOME/.emacs.d/init.el" <<'EOF'
;;; init.el --- Minimal Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; A minimal Emacs configuration with sensible defaults

;;; Code:

;; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; UI improvements
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(show-paren-mode t)
(global-hl-line-mode t)

;; Line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Better defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq require-final-newline t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; IDO mode for better file/buffer switching
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)

;; Electric pair mode
(electric-pair-mode 1)

;; Whitespace cleanup
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Custom key bindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Theme (built-in)
(load-theme 'wombat t)

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
EOF

  success "Vanilla Emacs configuration created!"
  echo ""
  info "Next steps:"
  info "  1. Apply config: sudo guix system reconfigure /etc/config.scm"
  info "  2. Launch Emacs to start using it"
  info "  3. Customize ~/.emacs.d/init.el as needed"
  echo ""
  info "Basic Emacs commands:"
  info "  C-x C-f - Find file"
  info "  C-x C-s - Save file"
  info "  C-x b   - Switch buffer"
  info "  C-x 2   - Split window horizontally"
  info "  C-x 3   - Split window vertically"
  info "  C-x 1   - Close other windows"
  info "  C-x C-c - Quit Emacs"
  echo ""
  info "To add packages:"
  info "  M-x package-list-packages"
  info "  i (mark), x (install), d (mark delete)"
  echo ""
}

# Run if called directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  add_vanilla_emacs
fi
