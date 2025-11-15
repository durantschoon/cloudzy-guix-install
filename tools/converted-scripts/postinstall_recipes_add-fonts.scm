# Converted Guile Script

```scheme
#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; Shared recipe: Install common fonts for programming and general use
;;; Can be called from any platform's customize tool

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 textual-ports)
             (srfi srfi-1))

;;; Configuration

(define config-file
  (or (getenv "CONFIG_FILE") "/etc/config.scm"))

;;; Helper Functions

(define (info . args)
  (format #t "  ~a~%" (string-join (map (lambda (x) (format #f "~a" x)) args) " ")))

(define (success . args)
  (format #t "~%\x1b[1;32m[✓]\x1b[0m ~a~%"
          (string-join (map (lambda (x) (format #f "~a" x)) args) " ")))

(define (file-contains? filepath pattern)
  "Check if file contains the given pattern"
  (if (file-exists? filepath)
      (let ((content (call-with-input-file filepath get-string-all)))
        (string-contains content pattern))
      #f))

(define (read-file filepath)
  "Read entire file as string"
  (call-with-input-file filepath get-string-all))

(define (write-file filepath content)
  "Write content to file"
  (call-with-output-file filepath
    (lambda (port)
      (put-string port content))))

;;; Font Package Definitions

(define font-packages
  '("font-fira-code"           ; Popular programming font with ligatures
    "font-jetbrains-mono"      ; JetBrains programming font
    "font-dejavu"              ; DejaVu fonts (good coverage)
    "font-liberation"          ; Liberation fonts (metric-compatible with Arial, etc.)
    "font-gnu-freefont"        ; GNU FreeFont
    "font-awesome"             ; Icon font
    "font-google-noto"))       ; Google Noto (comprehensive Unicode)

;;; Main Logic

(define (build-package-list-string packages)
  "Build package list string for insertion into config"
  (string-join
   (map (lambda (pkg)
          (format #f "                (specification->package \"~a\")" pkg))
        packages)
   "\n"))

(define (add-fonts-to-minimal-config content)
  "Add fonts to minimal config with %base-packages"
  (let ((package-list (build-package-list-string font-packages)))
    (regexp-substitute/global
     #f
     "\\(packages %base-packages\\)"
     content
     'pre
     (format #f "(packages\n  (append\n   (list\n~a\n         )\n   %base-packages))"
             package-list)
     'post)))

(define (add-font-to-existing-config content pkg)
  "Add a single font package to existing packages section"
  (if (string-contains content (format #f "\"~a\"" pkg))
      content  ; Already present, don't add
      (regexp-substitute/global
       #f
       "specification->package"
       content
       'pre
       (format #f "specification->package\n                (specification->package \"~a\")" pkg)
       'post
       1)))  ; Only replace first occurrence

(define (add-fonts-to-existing-config content)
  "Add fonts to existing packages configuration"
  (fold add-font-to-existing-config content font-packages))

(define (add-fonts)
  "Main function to add fonts to config.scm"
  (display "\n")
  (display "=== Installing Common Fonts ===\n")
  (display "\n")
  
  (info "Adding font packages to config.scm...")
  
  (let ((content (read-file config-file)))
    (let ((new-content
           (if (string-contains content "(packages %base-packages)")
               ;; Minimal config - need to create packages list
               (add-fonts-to-minimal-config content)
               ;; Already has packages, add fonts
               (add-fonts-to-existing-config content))))
      
      (write-file config-file new-content)))
  
  (success "Font packages added to configuration!")
  (display "\n")
  (info "Fonts added:")
  (info "  - Fira Code (programming, ligatures)")
  (info "  - JetBrains Mono (programming)")
  (info "  - DejaVu (general use)")
  (info "  - Liberation (MS-compatible)")
  (info "  - GNU FreeFont (Unicode coverage)")
  (info "  - Font Awesome (icons)")
  (info "  - Google Noto (comprehensive)")
  (display "\n")
  (info "After reconfiguring, you may need to rebuild font cache:")
  (info "  fc-cache -f -v")
  (display "\n")
  (info "Apply changes with: sudo guix system reconfigure /etc/config.scm"))

;;; Entry Point

;; Run if called directly
(when (batch-mode?)
  (add-fonts))
```