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

    # Add emacs to packages if not present
    if grep -q "(packages %base-packages)" "$CONFIG_FILE"; then
      # Minimal config - need to add packages list
      sed -i 's|(packages %base-packages)|(packages\n  (append\n   (list (specification->package "emacs")\n         (specification->package "git"))\n   %base-packages))|' "$CONFIG_FILE"
    else
      # Already has packages, just add emacs
      sed -i '/specification->package "emacs"/!{/(packages/a\                (specification->package "emacs")' "$CONFIG_FILE"
    fi

    info "Emacs added to configuration"
  else
    info "Emacs already in configuration"
  fi

  # Install Spacemacs
  info "Installing Spacemacs to ~/.emacs.d..."

  if [[ -d "$HOME/.emacs.d" ]]; then
    warn "~/.emacs.d already exists"
    read -r -p "Backup and replace with Spacemacs? [y/N] " ans
    if [[ "$ans" =~ ^[Yy]$ ]]; then
      mv "$HOME/.emacs.d" "$HOME/.emacs.d.backup.$(date +%Y%m%d-%H%M%S)"
      info "Backed up existing .emacs.d"
    else
      info "Skipping Spacemacs installation"
      return 0
    fi
  fi

  git clone https://github.com/syl20bnr/spacemacs "$HOME/.emacs.d"

  # Create basic .spacemacs configuration
  if [[ ! -f "$HOME/.spacemacs" ]]; then
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
  info "  3. First launch will download and install packages (may take a while)"
  echo ""
  info "Spacemacs quick start:"
  info "  SPC f f - Find file"
  info "  SPC p f - Find file in project"
  info "  SPC b b - Switch buffer"
  info "  SPC w / - Split window vertically"
  info "  SPC w - - Split window horizontally"
  info "  SPC q q - Quit"
  echo ""
}

# Run if called directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  add_spacemacs
fi
