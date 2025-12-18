#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; add-doom-emacs.scm
;;; Shared recipe: Install and configure Doom Emacs
;;; Can be called from any platform's customize tool

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 match)
             (ice-9 pretty-print)
             (srfi srfi-1)
             (srfi srfi-19))

;;; Configuration

(define config-file
  (or (getenv "CONFIG_FILE") "/etc/config.scm"))

;;; Color output helpers

(define (info . args)
  (format #t "  ~a~%" (string-join (map (lambda (x) (format #f "~a" x)) args) "")))

(define (warn . args)
  (format #t "\n\033[1;33m[warn]\033[0m ~a~%"
          (string-join (map (lambda (x) (format #f "~a" x)) args) "")))

(define (success . args)
  (format #t "\n\033[1;32m[OK]\033[0m ~a~%"
          (string-join (map (lambda (x) (format #f "~a" x)) args) "")))

;;; File and command utilities

(define (file-contains? filepath pattern)
  "Check if file contains a pattern (string)."
  (call-with-input-file filepath
    (lambda (port)
      (let loop ((line (read-line port)))
        (cond
         ((eof-object? line) #f)
         ((string-contains line pattern) #t)
         (else (loop (read-line port))))))))

(define (run-command cmd)
  "Execute a shell command and return exit status."
  (system cmd))

(define (run-command/output cmd)
  "Execute a shell command and return output as string."
  (let* ((port (open-input-pipe cmd))
         (output (read-string port))
         (status (close-pipe port)))
    (values output status)))

(define (read-user-input prompt)
  "Read a line of input from the user with a prompt."
  (display prompt)
  (force-output)
  (read-line (current-input-port)))

(define (yes-or-no? prompt)
  "Ask user a yes/no question. Returns #t for yes, #f for no."
  (let ((response (read-user-input prompt)))
    (and (not (eof-object? response))
         (or (string-ci=? response "y")
             (string-ci=? response "yes")))))

(define (get-timestamp)
  "Get timestamp string for backups."
  (date->string (current-date) "~Y~m~d-~H~M~S"))

;;; Config.scm manipulation helpers - S-expression approach

(define emacs-packages '("emacs" "git" "ripgrep" "fd"))

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
  "Add Emacs and dependencies to the packages list in config.scm using S-expression manipulation."
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
      (info "Emacs and dependencies added to configuration"))))

;;; Directory management

(define (backup-directory! dir)
  "Backup a directory by renaming it with timestamp."
  (when (file-exists? dir)
    (let ((backup-name (format #f "~a.backup.~a" dir (get-timestamp))))
      (rename-file dir backup-name)
      (info (format #f "Backed up ~a to ~a" dir backup-name)))))

;;; Git operations

(define (git-clone repo-url dest-dir)
  "Clone a git repository. Returns #t on success, #f on failure."
  (let ((cmd (format #f "git clone --depth 1 ~s ~s" repo-url dest-dir)))
    (zero? (run-command cmd))))

(define (git-clone-config repo-url dest-dir)
  "Clone a git repository to temporary location, then move contents. Returns #t on success."
  (let ((tmp-dir (format #f "~a.tmp" dest-dir)))
    (if (git-clone repo-url tmp-dir)
        (begin
          ;; Move contents from tmp to dest
          (let ((cmd (format #f "mv ~a/* ~a/ 2>/dev/null || true" tmp-dir dest-dir)))
            (run-command cmd))
          ;; Move dotfiles
          (let ((cmd (format #f "mv ~a/.[!.]* ~a/ 2>/dev/null || true" tmp-dir dest-dir)))
            (run-command cmd))
          ;; Remove tmp directory
          (let ((cmd (format #f "rm -rf ~a" tmp-dir)))
            (run-command cmd))
          #t)
        #f)))

;;; Doom Emacs installation

(define (get-import-config-choice)
  "Ask user about importing existing Doom config. Returns repo URL or #f."
  (newline)
  (if (yes-or-no? "Do you have an existing Doom Emacs config to import? [y/N] ")
      (begin
        (newline)
        (info "Import options:")
        (info "  1. Git repository URL")
        (info "  2. Skip import (install fresh, import manually later)")
        (newline)
        (let ((choice (read-user-input "Enter choice [1-2]: ")))
          (cond
           ((or (string=? choice "1") (string=? choice ""))
            (let ((repo-url (read-user-input "Enter Git repository URL (e.g., https://github.com/user/doom-config): ")))
              (if (and (not (eof-object? repo-url))
                       (not (string=? repo-url "")))
                  (begin
                    (info (format #f "Will import config from: ~a" repo-url))
                    repo-url)
                  (begin
                    (warn "No URL provided, will create default config")
                    #f))))
           ((string=? choice "2")
            (info "Skipping import - you can manually import later")
            (info "See: ~/guix-customize/EMACS_IMPORT_GUIDE.md")
            #f)
           (else
            (warn "Invalid choice, will create default config")
            #f))))
      #f))

(define (handle-existing-emacs-config)
  "Check for existing Emacs configs and offer to back them up. Returns #t to continue, #f to skip."
  (let ((emacs-config (string-append (getenv "HOME") "/.config/emacs"))
        (emacs-d (string-append (getenv "HOME") "/.emacs.d")))
    (if (or (file-exists? emacs-config) (file-exists? emacs-d))
        (begin
          (warn "Existing Emacs configuration found")
          (if (yes-or-no? "Backup and replace with Doom Emacs? [y/N] ")
              (begin
                (backup-directory! emacs-config)
                (backup-directory! emacs-d)
                #t)
              (begin
                (info "Skipping Doom Emacs installation")
                #f)))
        #t)))

(define (install-doom-emacs)
  "Clone and install Doom Emacs framework."
  (let ((doom-dir (string-append (getenv "HOME") "/.config/emacs")))
    (info "Installing Doom Emacs to ~/.config/emacs...")
    (if (git-clone "https://github.com/doomemacs/doomemacs" doom-dir)
        (begin
          (info "Running Doom installer...")
          (let ((cmd (format #f "~a/bin/doom install --no-env --no-fonts" doom-dir)))
            (run-command cmd))
          #t)
        (begin
          (warn "Failed to clone Doom Emacs")
          #f))))

(define (create-doom-init-file doom-config-dir)
  "Create default init.el for Doom Emacs."
  (let ((init-file (string-append doom-config-dir "/init.el")))
    (call-with-output-file init-file
      (lambda (port)
        (display ";;; init.el -*- lexical-binding: t; -*-

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
" port)))))

(define (create-doom-config-file doom-config-dir)
  "Create default config.el for Doom Emacs."
  (let ((config-file (string-append doom-config-dir "/config.el")))
    (call-with-output-file config-file
      (lambda (port)
        (display ";;; config.el -*- lexical-binding: t; -*-

;; Basic settings
(setq user-full-name \"Your Name\"
      user-mail-address \"your@email.com\")

;; Theme
(setq doom-theme 'doom-one)

;; Line numbers
(setq display-line-numbers-type 'relative)

;; Font
(setq doom-font (font-spec :family \"monospace\" :size 14))
" port)))))

(define (create-doom-packages-file doom-config-dir)
  "Create default packages.el for Doom Emacs."
  (let ((packages-file (string-append doom-config-dir "/packages.el")))
    (call-with-output-file packages-file
      (lambda (port)
        (display ";; -*- no-byte-compile: t; -*-
;;; packages.el

;; Add your custom packages here
" port)))))

(define (setup-doom-config import-repo-url)
  "Set up Doom config directory, either importing from repo or creating defaults."
  (let ((doom-config-dir (string-append (getenv "HOME") "/.config/doom"))
        (doom-emacs-bin (string-append (getenv "HOME") "/.config/emacs/bin/doom")))

    ;; Create doom config directory
    (unless (file-exists? doom-config-dir)
      (mkdir doom-config-dir))

    ;; Import config or create defaults
    (cond
     ;; User wants to import from repo
     ((and import-repo-url (not (string=? import-repo-url "")))
      (info "Importing your Doom config from repository...")
      (if (git-clone-config import-repo-url doom-config-dir)
          (begin
            (success "Config imported successfully!")
            (info "Running doom sync to install packages...")
            (run-command (format #f "~a sync" doom-emacs-bin)))
          (begin
            (warn "Failed to clone repository, creating default config instead")
            (setup-default-doom-config doom-config-dir))))

     ;; No import, create defaults if files don't exist
     ((not (file-exists? (string-append doom-config-dir "/init.el")))
      (setup-default-doom-config doom-config-dir)))))

(define (setup-default-doom-config doom-config-dir)
  "Create default Doom configuration files."
  (info "Creating basic Doom configuration...")
  (create-doom-init-file doom-config-dir)
  (unless (file-exists? (string-append doom-config-dir "/config.el"))
    (create-doom-config-file doom-config-dir))
  (unless (file-exists? (string-append doom-config-dir "/packages.el"))
    (create-doom-packages-file doom-config-dir)))

(define (show-completion-message import-repo-url)
  "Display completion message with next steps."
  (success "Doom Emacs installation complete!")
  (newline)
  (info "Next steps:")
  (info "  1. Apply config: sudo guix system reconfigure /etc/config.scm")
  (if (not import-repo-url)
      (begin
        (info "  2. Run: ~/.config/emacs/bin/doom sync")
        (info "  3. Launch Emacs to complete setup"))
      (info "  2. Launch Emacs (packages already synced)"))
  (newline)
  (info "Doom Emacs quick start:")
  (info "  SPC f f - Find file")
  (info "  SPC p f - Find file in project")
  (info "  SPC b b - Switch buffer")
  (info "  SPC w v - Split window vertically")
  (info "  SPC w s - Split window horizontally")
  (info "  SPC q q - Quit")
  (newline)
  (when (not import-repo-url)
    (info "To import your config later, see: ~/guix-customize/EMACS_IMPORT_GUIDE.md"))
  (info "Documentation: https://docs.doomemacs.org")
  (newline))

;;; Main entry point

(define (add-doom-emacs)
  "Main procedure to install and configure Doom Emacs."
  (newline)
  (display "=== Installing Doom Emacs ===\n")
  (newline)

  ;; Check if Emacs is already in config
  (if (not (file-contains? config-file "emacs"))
      (begin
        (info "Adding Emacs package to config.scm...")
        (add-emacs-to-packages)
        (info "Emacs and dependencies added to configuration"))
      (info "Emacs already in configuration"))

  ;; Get import choice
  (let ((import-repo-url (get-import-config-choice)))

    ;; Handle existing configs
    (when (handle-existing-emacs-config)
      ;; Install Doom Emacs
      (when (install-doom-emacs)
        ;; Setup config
        (setup-doom-config import-repo-url)
        ;; Show completion message
        (show-completion-message import-repo-url)))))

;;; Script entry point

;; Run if called directly
(when (batch-mode?)
  (add-doom-emacs))
