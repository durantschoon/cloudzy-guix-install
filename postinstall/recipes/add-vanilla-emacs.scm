#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; add-vanilla-emacs.scm --- Install vanilla Emacs with minimal configuration
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

(define import-emacs-config "")

;;; Color output helpers

(define (info . args)
  (format #t "  ~a~%" (string-join (map (lambda (x) (format #f "~a" x)) args) " ")))

(define (warn . args)
  (format #t "\n\033[1;33m[warn]\033[0m ~a~%"
          (string-join (map (lambda (x) (format #f "~a" x)) args) " ")))

(define (success . args)
  (format #t "\n\033[1;32m[OK]\033[0m ~a~%"
          (string-join (map (lambda (x) (format #f "~a" x)) args) " ")))

;;; File and command utilities

(define (file-contains? filepath pattern)
  "Check if file contains the given pattern string."
  (call-with-input-file filepath
    (lambda (port)
      (let loop ((line (read-line port)))
        (cond
         ((eof-object? line) #f)
         ((string-contains line pattern) #t)
         (else (loop (read-line port))))))))

(define (read-user-input prompt)
  "Read a line of input from the user with the given prompt."
  (display prompt)
  (force-output)
  (read-line (current-input-port)))

(define (yes-or-no? prompt)
  "Ask user a yes/no question. Returns #t for yes, #f for no."
  (let ((response (read-user-input prompt)))
    (and (not (eof-object? response))
         (or (string-ci=? response "y")
             (string-ci=? response "yes")))))

(define (backup-file filepath)
  "Create a timestamped backup of a file or directory."
  (let* ((timestamp (strftime "%Y%m%d-%H%M%S" (localtime (current-time))))
         (backup-path (string-append filepath ".backup." timestamp)))
    (if (file-exists? filepath)
        (begin
          (system* "mv" filepath backup-path)
          (info "Backed up to" backup-path)
          #t)
        #f)))

(define (read-file-content filepath)
  "Read entire file content as a string."
  (call-with-input-file filepath
    (lambda (port)
      (read-string port))))

(define (write-file-content filepath content)
  "Write string content to file."
  (call-with-output-file filepath
    (lambda (port)
      (display content port))))

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

;;; Emacs configuration creation

(define minimal-emacs-init
  ";;; init.el --- Minimal Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; A minimal Emacs configuration with sensible defaults

;;; Code:

;; Performance tweaks
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Package management
(require 'package)
(setq package-archives '((\"melpa\" . \"https://melpa.org/packages/\")
                          (\"gnu\" . \"https://elpa.gnu.org/packages/\")))
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
(setq backup-directory-alist '((\"\\\\.\" . \"~/.emacs.d/backups\")))
(setq auto-save-file-name-transforms '((\".*\" \"~/.emacs.d/auto-save-list/\" t)))

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
(global-set-key (kbd \"C-x C-b\") 'ibuffer)
(global-set-key (kbd \"M-/\") 'hippie-expand)

;; Theme (built-in)
(load-theme 'wombat t)

;; Custom file
(setq custom-file (expand-file-name \"custom.el\" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
")

(define (create-minimal-emacs-config)
  "Create minimal Emacs configuration in ~/.emacs.d/init.el."
  (let ((emacs-dir (string-append (getenv "HOME") "/.emacs.d"))
        (init-file (string-append (getenv "HOME") "/.emacs.d/init.el")))
    ;; Create directory if needed
    (unless (file-exists? emacs-dir)
      (mkdir emacs-dir))
    ;; Write init.el
    (write-file-content init-file minimal-emacs-init)
    (info "Created minimal Emacs configuration")))

(define (import-emacs-config-from-git repo-url)
  "Clone Emacs configuration from Git repository."
  (let ((emacs-dir (string-append (getenv "HOME") "/.emacs.d")))
    (info "Importing your Emacs configuration...")
    (let ((status (system* "git" "clone" repo-url emacs-dir)))
      (if (zero? status)
          (begin
            (success "Emacs config imported successfully!")
            #t)
          (begin
            (warn "Failed to clone repository, will create default config instead")
            #f)))))

(define (handle-existing-emacs-config action-description)
  "Handle existing Emacs configuration with user confirmation."
  (let ((emacs-dir (string-append (getenv "HOME") "/.emacs.d"))
        (emacs-file (string-append (getenv "HOME") "/.emacs")))
    (if (or (file-exists? emacs-dir) (file-exists? emacs-file))
        (begin
          (warn "Existing Emacs configuration found")
          (if (yes-or-no? (string-append "Backup and " action-description "? [y/N] "))
              (begin
                (when (file-exists? emacs-dir)
                  (backup-file emacs-dir))
                (when (file-exists? emacs-file)
                  (backup-file emacs-file))
                #t)
              (begin
                (info "Keeping existing configuration")
                #f)))
        #t)))

;;; Main installation logic

(define (add-vanilla-emacs)
  "Install vanilla Emacs with minimal configuration."
  (display "\n=== Installing Vanilla Emacs ===\n\n")

  ;; Check if Emacs is already in config
  (if (not (file-contains? config-file "emacs"))
      (begin
        (info "Adding Emacs package to config.scm...")
        (add-emacs-to-packages)
        (info "Emacs added to configuration"))
      (info "Emacs already in configuration"))

  ;; Ask about importing existing config
  (newline)
  (let ((import-choice (yes-or-no? "Do you have an existing vanilla Emacs config to import? [y/N] ")))
    (when import-choice
      (newline)
      (info "Import options:")
      (info "  1. Git repository URL (for .emacs.d or init.el)")
      (info "  2. Skip import (install fresh, import manually later)")
      (newline)
      (let ((choice (read-user-input "Enter choice [1-2]: ")))
        (cond
         ((or (string=? choice "1") (string=? choice ""))
          (let ((repo-url (read-user-input "Enter Git repository URL (e.g., https://github.com/user/emacs-config): ")))
            (if (and (not (eof-object? repo-url))
                     (not (string=? repo-url "")))
                (begin
                  (info "Will import config from:" repo-url)
                  (set! import-emacs-config repo-url))
                (warn "No URL provided, will create default config"))))
         ((string=? choice "2")
          (info "Skipping import - you can manually import later")
          (info "See: ~/guix-customize/EMACS_IMPORT_GUIDE.md"))
         (else
          (warn "Invalid choice, will create default config"))))))

  ;; Import or create configuration
  (if (not (string=? import-emacs-config ""))
      (when (handle-existing-emacs-config "replace with imported config")
        (unless (import-emacs-config-from-git import-emacs-config)
          ;; Import failed, create minimal config
          (set! import-emacs-config "")
          (when (handle-existing-emacs-config "create minimal config")
            (create-minimal-emacs-config))))
      ;; Create minimal config
      (begin
        (info "Creating minimal Emacs configuration...")
        (when (handle-existing-emacs-config "create minimal config")
          (create-minimal-emacs-config))))

  ;; Show completion message
  (success "Vanilla Emacs configuration created!")
  (newline)
  (info "Next steps:")
  (info "  1. Apply config: sudo guix system reconfigure /etc/config.scm")
  (info "  2. Launch Emacs to start using it")
  (when (string=? import-emacs-config "")
    (info "  3. Customize ~/.emacs.d/init.el as needed"))
  (newline)
  (info "Basic Emacs commands:")
  (info "  C-x C-f - Find file")
  (info "  C-x C-s - Save file")
  (info "  C-x b   - Switch buffer")
  (info "  C-x 2   - Split window horizontally")
  (info "  C-x 3   - Split window vertically")
  (info "  C-x 1   - Close other windows")
  (info "  C-x C-c - Quit Emacs")
  (newline)
  (when (string=? import-emacs-config "")
    (info "To add packages:")
    (info "  M-x package-list-packages")
    (info "  i (mark), x (install), d (mark delete)")
    (newline)
    (info "To import your config later, see: ~/guix-customize/EMACS_IMPORT_GUIDE.md"))
  (newline))

;;; Entry point

;; Run if called directly
(when (batch-mode?)
  (add-vanilla-emacs))
