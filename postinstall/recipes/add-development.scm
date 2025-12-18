#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Shared recipe: Install common development tools
;;; Can be called from any platform's customize tool

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 match)
             (ice-9 pretty-print)
             (srfi srfi-1))

;;; Configuration

(define config-file
  (or (getenv "CONFIG_FILE")
      "/etc/config.scm"))

;;; ANSI Colors for output

(define (info . args)
  (format #t "  ~a~%" (string-join (map (lambda (x) (format #f "~a" x)) args) " ")))

(define (success . args)
  (format #t "\n\033[1;32m[✓]\033[0m ~a\n" 
          (string-join (map (lambda (x) (format #f "~a" x)) args) " ")))

;;; Development packages list

(define dev-packages
  '("git"
    "vim"
    "emacs"
    "make"
    "gcc-toolchain"
    "python"
    "node"
    "go"
    "curl"
    "wget"
    "ripgrep"
    "fd"
    "tmux"
    "htop"))

;;; Helper functions

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

(define (package-already-present? exprs pkg)
  "Check if a package is already in the configuration."
  (define (check-expr expr)
    (match expr
      (('specification->package str)
       (string=? str pkg))
      ((? list? lst)
       (any check-expr lst))
      (_ #f)))
  (any check-expr exprs))

(define (create-package-list packages)
  "Create a list S-expression of specification->package calls."
  (cons 'list
        (map (lambda (pkg)
               (list 'specification->package pkg))
             packages)))

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
      ;; Find packages form with append
      (('packages ('append ('list existing-pkgs ...) rest ...))
       (let* ((existing-pkg-names 
               (filter-map (lambda (pkg)
                             (match pkg
                               (('specification->package name) name)
                               (_ #f)))
                           existing-pkgs))
              (new-pkgs (filter (lambda (pkg)
                                  (not (member pkg existing-pkg-names)))
                                packages))
              (new-pkg-specs (map (lambda (pkg)
                                    (list 'specification->package pkg))
                                  new-pkgs))
              (all-pkgs (append existing-pkgs new-pkg-specs)))
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

;;; Main logic

(define (add-development)
  "Add common development tools to Guix system configuration."
  (display "\n")
  (display "=== Installing Development Tools ===\n")
  (display "\n")
  
  (info "Adding development packages to config.scm...")
  
  ;; Read current configuration
  (let ((exprs (read-all-exprs config-file)))
    
    ;; Transform configuration based on current structure
    (let ((new-exprs
           (if (has-minimal-packages? exprs)
               ;; Minimal config - create packages list
               (begin
                 (info "Detected minimal configuration, creating packages list...")
                 (add-packages-to-minimal exprs dev-packages))
               ;; Existing packages - add to them
               (begin
                 (info "Adding to existing packages...")
                 (add-packages-to-existing exprs dev-packages)))))
      
      ;; Write updated configuration
      (write-all-exprs config-file new-exprs)
      
      (success "Development tools added to configuration!")
      (display "\n")
      (info "Packages added:")
      (for-each (lambda (pkg)
                  (info (format #f "  - ~a" pkg)))
                dev-packages)
      (display "\n")
      (info "Apply changes with: sudo guix system reconfigure /etc/config.scm"))))

;;; Entry point

;; Run if called directly
(when (batch-mode?)
  (add-development))
