# Converted Guile Script

```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Shared recipe: Install and configure Spacemacs
;;; Can be called from any platform's customize tool

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 regex)
             (srfi srfi-1))

;;; Configuration

(define config-file
  (or (getenv "CONFIG_FILE") "/etc/config.scm"))

;;; Helper Functions - Output Formatting

(define (info . args)
  (display "  ")
  (for-each display args)
  (newline))

(define (warn . args)
  (display "\n\033[1;33m[warn]\033[0m ")
  (for-each display args)
  (newline))

(define (success . args)
  (display "\n\033[1;32m[✓]\033[0m ")
  (for-each display args)
  (newline))

;;; Helper Functions - File Operations

(define (file-contains? filepath pattern)
  "Check if file contains a pattern (string or regex)."
  (call-with-input-file filepath
    (lambda (port)
      (let loop ((line (read-line port)))
        (cond
         ((eof-object? line) #f)
         ((if (string? pattern)
              (string-contains line pattern)
              (regexp-exec pattern line))
          #t)
         (else (loop (read-line port))))))))

(define (backup-file filepath)
  "Backup a file with timestamp."
  (let ((backup-name (format #f "~a.backup.~a"
                            filepath
                            (strftime "%Y%m%d-%H%M%S" (localtime (current-time))))))
    (system* "mv" filepath backup-name)
    backup-name))

;;; Helper Functions - User Input

(define (read-user-input prompt)
  "Read user input from /dev/tty."
  (display prompt)
  (flush-all-ports)
  (call-with-input-file "/dev/tty"
    (lambda (port)
      (read-line port))))

(define (yes-or-no? prompt)
  "Ask user a yes/no question. Returns #t for yes, #f for no."
  (let ((response (read-user-input prompt)))
    (and (string? response)
         (or (string-ci=? response "y")
             (string-ci=? response "yes")))))

;;; Helper Functions - Config Manipulation

(define (add-packages-to-config! config-path packages)
  "Add packages to config.scm file using sed commands."
  ;; First, ensure we have the package modules
  (unless (file-contains? config-path "gnu packages emacs")
    (system* "sed" "-i"
             "/(use-modules/a\\             (gnu packages emacs)\\n             (gnu packages version-control))"
             config-path))
  
  ;; Add packages to the packages list
  (cond
   ;; Case 1: Minimal config with just %base-packages
   ((file-contains? config-path "(packages %base-packages)")
    (let ((replacement (format #f "(packages\\n  (append\\n   (list ~a)\\n   %base-packages))"
                              (string-join packages "\n         "))))
      (system* "sed" "-i"
               (format #f "s|(packages %base-packages)|~a|" replacement)
               config-path)))
   ;; Case 2: Already has packages list, add to existing list
   (else
    (system* "sed" "-i"
             (format #f "/(packages/,/))/ { /list/ { s/list/list ~a/ } }"
                    (string-join packages "\n         "))
             config-path))))

;;; Helper Functions - Git Operations

(define (git-clone url destination)
  "Clone a git repository. Returns #t on success, #f on failure."
  (zero? (system* "git" "clone" url destination)))

;;; Helper Functions - Directory Operations

(define (directory-exists? path)
  "Check if directory exists."
  (access? path F_OK))

;;; Main Installation Logic

(define (install-spacemacs-repo home-dir)
  "Install Spacemacs to ~/.emacs.d."
  (let ((emacs-dir (string-append home-dir "/.emacs.d")))
    (cond
     ((directory-exists? emacs-dir)
      (warn "~/.emacs.d already exists")
      (when (yes-or-no? "Backup and replace with Spacemacs? [y/N] ")
        (let ((backup-name (backup-file emacs-dir)))
          (info "Backed up existing .emacs.d to " backup-name)
          (git-clone "https://github.com/syl20bnr/spacemacs" emacs-dir))))
     (else
      (info "Installing Spacemacs to ~/.emacs.d...")
      (git-clone "https://github.com/syl20bnr/spacemacs" emacs-dir)))))

(define (import-spacemacs-config home-dir repo-url config-path)
  "Import .spacemacs config from a git repository. Returns #t on success."
  (let ((tmp-dir (string-append home-dir "/.spacemacs-tmp"))
        (target-file (string-append home-dir "/.spacemacs")))
    (info "Importing your .spacemacs from repository...")
    (if (git-clone repo-url tmp-dir)
        (let ((source-file (string-append tmp-dir "/" config-path)))
          (if (access? source-file R_OK)
              (begin
                (system* "cp" source-file target-file)
                (system* "rm" "-rf" tmp-dir)
                (success ".spacemacs imported successfully!")
                #t)
              (begin
                (warn (format #f "Could not find ~a in repository" config-path))
                (system* "rm" "-rf" tmp-dir)
                #f)))
        (begin
          (warn "Failed to clone repository, creating default config instead")
          #f))))

(define (create-default-spacemacs-config home-dir)
  "Create a basic .spacemacs configuration file."
  (let ((config-path (string-append home-dir "/.spacemacs")))
    (unless (access? config-path F_OK)
      (info "Creating basic .spacemacs configuration...")
      (call-with-output-file config-path
        (lambda (port)
          (display ";; -*- mode: emacs-lisp; lexical-binding: t -*-
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
   dotspacemacs-default-font '(\"Source Code Pro\"
                                :size 13
                                :weight normal
                                :width normal)
   dotspacemacs-leader-key \"SPC\"
   dotspacemacs-emacs-command-key \"SPC\"
   dotspacemacs-ex-command-key \":\"
   dotspacemacs-emacs-leader-key \"M-m\"
   dotspacemacs-major-mode-leader-key \",\"
   dotspacemacs-major-mode-emacs-leader-key \"C-M-m\"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name \"Default\"
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
   dotspacemacs-search-tools '(\"ag\" \"pt\" \"ack\" \"grep\")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  \"Initialization function for user code.\")

(defun dotspacemacs/user-config ()
  \"Configuration function for user code.\")

;; Do not write anything past this comment.
" port)))
      (success "Created .spacemacs configuration"))))

(define (print-next-steps imported?)
  "Print next steps after installation."
  (success "Spacemacs installation complete!")
  (newline)
  (info "Next steps:")
  (info "  1. Apply config: sudo guix system reconfigure /etc/config.scm")
  (info "  2. Launch Emacs to complete Spacemacs setup")
  (if imported?
      (info "  3. Imported config will install its packages on first launch")
      (info "  3. First launch will download and install packages (may take a while)"))
  (newline)
  (info "Spacemacs quick start:")
  (info "  SPC f f - Find file")
  (info "  SPC p f - Find file in project")
  (info "  SPC b b - Switch buffer")
  (info "  SPC w / - Split window vertically")
  (info "  SPC w - - Split window horizontally")
  (info "  SPC q q - Quit")
  (newline)
  (unless imported?
    (info "To import your config later, see: ~/guix-customize/EMACS_IMPORT_GUIDE.md")
    (newline)))

;;; Main Entry Point

(define (add-spacemacs)
  "Main procedure to install and configure Spacemacs."
  (let ((home-dir (getenv "HOME")))
    (newline)
    (display "=== Installing Spacemacs ===")
    (newline)
    (newline)

    ;; Check if Emacs is already installed in config
    (if (file-contains? config-file "emacs")
        (info "Emacs already in configuration")
        (begin
          (info "Adding Emacs package to config.scm...")
          (add-packages-to-config! config-file '("emacs" "git"))
          (info "Emacs added to configuration")))

    ;; Ask about importing existing config
    (newline)
    (let ((import-config? (yes-or-no? "Do you have an existing Spacemacs config (.spacemacs) to import? [y/N] ")))
      (define imported? #f)
      
      (when import-config?
        (newline)
        (info "Import options:")
        (info "  1. Git repository URL (for .spacemacs file)")
        (info "  2. Skip import (install fresh, import manually later)")
        (newline)
        (let ((choice (read-user-input "Enter choice [1-2]: ")))
          (cond
           ((equal? choice "1")
            (let ((repo-url (read-user-input "Enter Git repository URL (e.g., https://github.com/user/dotfiles): "))
                  (spacemacs-path (read-user-input "Path to .spacemacs in repo (e.g., .spacemacs or emacs/.spacemacs): ")))
              (if (and (string? repo-url) (not (string=? repo-url "")))
                  (begin
                    (info "Will import .spacemacs from: " repo-url)
                    (set! imported?
                          (import-spacemacs-config home-dir
                                                  repo-url
                                                  (if (string=? spacemacs-path "")
                                                      ".spacemacs"
                                                      spacemacs-path))))
                  (warn "No URL provided, will create default config"))))
           ((equal? choice "2")
            (info "Skipping import - you can manually import later")
            (info "See: ~/guix-customize/EMACS_IMPORT_GUIDE.md"))
           (else
            (warn "Invalid choice, will create default config")))))

      ;; Install Spacemacs
      (install-spacemacs-repo home-dir)

      ;; Create default config if not imported
      (unless imported?
        (let ((spacemacs-config (string-append home-dir "/.spacemacs")))
          (if (access? spacemacs-config F_OK)
              (info ".spacemacs already exists, keeping your configuration")
              (create-default-spacemacs-config home-dir))))

      ;; Print next steps
      (print-next-steps imported?))))

;;; Script Entry Point

;; Run if called directly
(when (string=? (car (command-line)) (car (program-arguments)))
  (add-spacemacs))
```