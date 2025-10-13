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

    # Add package modules if not present
    if ! grep -q "gnu packages emacs" "$CONFIG_FILE"; then
      sed -i '/(use-modules/a\             (gnu packages emacs)\n             (gnu packages version-control))' "$CONFIG_FILE"
    fi

    # Add emacs to packages
    if grep -q "(packages %base-packages)" "$CONFIG_FILE"; then
      # Minimal config - expand to use append
      sed -i 's|(packages %base-packages)|(packages\n  (append\n   (list emacs\n         git)\n   %base-packages))|' "$CONFIG_FILE"
    else
      # Already has packages list, add to it
      sed -i '/(packages/,/))/ { /list/ { s/list/list emacs\n         git/ } }' "$CONFIG_FILE"
    fi

    info "Emacs added to configuration"
  else
    info "Emacs already in configuration"
  fi

  # Ask if user wants to import existing config
  echo ""
  read -r -p "Do you have an existing vanilla Emacs config to import? [y/N] " import_config </dev/tty

  if [[ "$import_config" =~ ^[Yy]$ ]]; then
    echo ""
    info "Import options:"
    info "  1. Git repository URL (for .emacs.d or init.el)"
    info "  2. Skip import (install fresh, import manually later)"
    echo ""
    read -r -p "Enter choice [1-2]: " choice </dev/tty

    case "$choice" in
      1)
        read -r -p "Enter Git repository URL (e.g., https://github.com/user/emacs-config): " repo_url </dev/tty
        if [[ -n "$repo_url" ]]; then
          info "Will import config from: $repo_url"
          IMPORT_EMACS_CONFIG="$repo_url"
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

  # Import user's config or create minimal configuration
  if [[ -n "$IMPORT_EMACS_CONFIG" ]]; then
    info "Importing your Emacs configuration..."

    if [[ -d "$HOME/.emacs.d" ]] || [[ -f "$HOME/.emacs" ]]; then
      warn "Existing Emacs configuration found"
      read -r -p "Backup and replace with imported config? [y/N] " ans </dev/tty
      if [[ "$ans" =~ ^[Yy]$ ]]; then
        [[ -d "$HOME/.emacs.d" ]] && mv "$HOME/.emacs.d" "$HOME/.emacs.d.backup.$(date +%Y%m%d-%H%M%S)"
        [[ -f "$HOME/.emacs" ]] && mv "$HOME/.emacs" "$HOME/.emacs.backup.$(date +%Y%m%d-%H%M%S)"
        info "Backed up existing Emacs configuration"
      else
        info "Keeping existing configuration"
        return 0
      fi
    fi

    if git clone "$IMPORT_EMACS_CONFIG" "$HOME/.emacs.d"; then
      success "Emacs config imported successfully!"
    else
      warn "Failed to clone repository, creating default config instead"
      IMPORT_EMACS_CONFIG=""
    fi
  fi

  # Create minimal config if not imported
  if [[ -z "$IMPORT_EMACS_CONFIG" ]]; then
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
  fi

  success "Vanilla Emacs configuration created!"
  echo ""
  info "Next steps:"
  info "  1. Apply config: sudo guix system reconfigure /etc/config.scm"
  info "  2. Launch Emacs to start using it"
  if [[ -z "$IMPORT_EMACS_CONFIG" ]]; then
    info "  3. Customize ~/.emacs.d/init.el as needed"
  fi
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
  if [[ -z "$IMPORT_EMACS_CONFIG" ]]; then
    info "To add packages:"
    info "  M-x package-list-packages"
    info "  i (mark), x (install), d (mark delete)"
    echo ""
    info "To import your config later, see: ~/guix-customize/EMACS_IMPORT_GUIDE.md"
  fi
  echo ""
}

# Run if called directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  add_vanilla_emacs
fi
