#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Shared recipe: Install and configure Spacemacs
;;; Can be called from any platform's customize tool

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 match)
             (ice-9 pretty-print)
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
  (display "\n\033[1;32m[OK]\033[0m ")
  (for-each display args)
  (newline))

;;; Helper Functions - File Operations

(define (file-contains? filepath pattern)
  "Check if file contains a pattern (string)."
  (call-with-input-file filepath
    (lambda (port)
      (let loop ((line (read-line port)))
        (cond
         ((eof-object? line) #f)
         ((string-contains line pattern) #t)
         (else (loop (read-line port))))))))

(define (backup-file filepath)
  "Backup a file with timestamp."
  (let ((backup-name (format #f "~a.backup.~a"
                            filepath
                            (strftime "%Y%m%d-%H%M%S" (localtime (current-time))))))
    (system* "mv" filepath backup-name)
    (info "Backed up to " backup-name)
    backup-name))

;;; Helper Functions - User Input

(define (read-user-input prompt)
  "Read user input from stdin."
  (display prompt)
  (force-output)
  (read-line (current-input-port)))

(define (yes-or-no? prompt)
  "Ask user a yes/no question. Returns #t for yes, #f for no."
  (let ((response (read-user-input prompt)))
    (and (not (eof-object? response))
         (or (string-ci=? response "y")
             (string-ci=? response "yes")))))

;;; Config.scm manipulation helpers - S-expression approach

(define emacs-packages '("emacs" "git"))

(define (read-all-exprs filepath)
  "Read all S-expressions from a file and return them as a list."
  (call-with-input-file filepath
    (lambda (port)
      (let loop ((exprs '()))
        (let ((expr (read port)))
          (if (eof-object? expr)
              (reverse exprs)
              (loop (cons expr exprs))))))))

(define (write-all-exprs filepath exprs)
  "Write all S-expressions to a file with pretty-printing."
  (call-with-output-file filepath
    (lambda (port)
      (for-each (lambda (expr)
                  (pretty-print expr port)
                  (newline port))
                exprs))))

(define (has-minimal-packages? exprs)
  "Check if config uses minimal packages (just %base-packages)."
  (any (lambda (expr)
         (match expr
           (('operating-system fields ...)
            (any (lambda (field)
                   (match field
                     (('packages '%base-packages) #t)
                     (_ #f)))
                 fields))
           (_ #f)))
       exprs))

(define (create-package-list packages)
  "Create a list S-expression of package symbols (not specification->package)."
  (cons 'list (map string->symbol packages)))

(define (add-packages-to-minimal exprs packages)
  "Transform minimal config with %base-packages to include new packages."
  (map (lambda (expr)
         (match expr
           (('operating-system fields ...)
            (cons 'operating-system
                  (map (lambda (field)
                         (match field
                           (('packages '%base-packages)
                            (list 'packages
                                  (list 'append
                                        (create-package-list packages)
                                        '%base-packages)))
                           (_ field)))
                       fields)))
           (_ expr)))
       exprs))

(define (add-packages-to-existing exprs packages)
  "Add packages to existing packages list in config."
  (define (transform-expr expr)
    (match expr
      ;; Find packages form with append and list
      (('packages ('append ('list existing-pkgs ...) rest ...))
       (let* ((existing-pkg-names
               (filter-map (lambda (pkg)
                             (cond
                              ((symbol? pkg) (symbol->string pkg))
                              ((and (list? pkg) (eq? (car pkg) 'specification->package))
                               (cadr pkg))
                              (else #f)))
                           existing-pkgs))
              (new-pkgs (filter (lambda (pkg)
                                  (not (member pkg existing-pkg-names)))
                                packages))
              (new-pkg-syms (map string->symbol new-pkgs))
              (all-pkgs (append existing-pkgs new-pkg-syms)))
         (list 'packages
               (cons 'append
                     (cons (cons 'list all-pkgs)
                           rest)))))

      ;; Recursively process nested lists
      ((? list? lst)
       (map transform-expr lst))

      ;; Leave atoms unchanged
      (_ expr)))

  (map transform-expr exprs))

(define (add-emacs-to-packages)
  "Add Emacs and Git to the packages list in config.scm using S-expression manipulation."
  (let ((exprs (read-all-exprs config-file)))
    (let ((new-exprs
           (if (has-minimal-packages? exprs)
               (begin
                 (info "Detected minimal configuration, creating packages list...")
                 (add-packages-to-minimal exprs emacs-packages))
               (begin
                 (info "Adding to existing packages...")
                 (add-packages-to-existing exprs emacs-packages)))))
      (write-all-exprs config-file new-exprs)
      (info "Emacs and Git added to configuration"))))

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
          (add-emacs-to-packages)
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
              (if (and (not (eof-object? repo-url)) (not (string=? repo-url "")))
                  (begin
                    (info "Will import .spacemacs from: " repo-url)
                    (set! imported?
                          (import-spacemacs-config home-dir
                                                  repo-url
                                                  (if (or (eof-object? spacemacs-path) (string=? spacemacs-path ""))
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
(when (batch-mode?)
  (add-spacemacs))
