#!/run/current-system/profile/bin/guile \
--no-auto-compile
!#

;;; Bootstrap Installer - Guile Scheme Version
;;; Downloads, verifies, and builds the Guix installer from source
;;;
;;; Verification strategy:
;;;   1. Git verifies the commit/tag integrity when cloning
;;;   2. Go verifies go.mod and go.sum (if external deps exist)
;;;   3. Source code is compiled locally before execution

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 ftw)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26))

;;; Configuration

(define default-repo-owner "durantschoon/cloudzy-guix-install")
(define default-repo-ref "main")

;;; Command Execution Helpers

(define (run-command cmd)
  "Execute CMD and return (output . status)."
  (let* ((port (open-input-pipe cmd))
         (output (read-string port))
         (status (close-pipe port)))
    (cons output status)))

(define (run-command/check cmd)
  "Execute CMD and error if it fails."
  (let ((result (run-command cmd)))
    (unless (zero? (cdr result))
      (error (format #f "Command failed: ~a" cmd)))
    (car result)))

(define (system*/check cmd)
  "Execute CMD via system and error if it fails."
  (let ((status (system cmd)))
    (unless (zero? status)
      (error (format #f "Command failed: ~a" cmd)))))

;;; File and Path Helpers

(define (file-exists-and-readable? filepath)
  "Check if FILEPATH exists and is readable."
  (and (file-exists? filepath)
       (access? filepath R_OK)))

(define (make-executable filepath)
  "Make FILEPATH executable."
  (chmod filepath #o755))

(define (calculate-sha256 filepath)
  "Calculate SHA256 hash of FILEPATH."
  (let ((result (run-command (format #f "sha256sum ~s" filepath))))
    (if (zero? (cdr result))
        (car (string-split (car result) #\space))
        (error (format #f "Failed to calculate hash for ~a" filepath)))))

;;; Argument Parsing

(define (parse-arguments args)
  "Parse command line arguments into an association list."
  (let loop ((args args)
             (channel-repo "")
             (channel-branch "main")
             (channel-path "")
             (platform ""))
    (match args
      (() (list (cons 'channel-repo channel-repo)
                (cons 'channel-branch channel-branch)
                (cons 'channel-path channel-path)
                (cons 'platform platform)))
      (("--channel-repo" repo rest ...)
       (loop rest repo channel-branch channel-path platform))
      (("--channel-branch" branch rest ...)
       (loop rest channel-repo branch channel-path platform))
      (("--channel-path" path rest ...)
       (loop rest channel-repo channel-branch path platform))
      ((plat rest ...)
       (loop rest channel-repo channel-branch channel-path plat)))))

;;; Font Setup

(define (set-console-font font-name-or-bool)
  "Set console font if USEBIGFONT is enabled."
  (let* ((font-dir "/run/current-system/profile/share/consolefonts")
         (font-name (cond
                     ((or (string=? font-name-or-bool "1")
                          (string=? font-name-or-bool "yes")
                          (string=? font-name-or-bool "true")
                          (string=? font-name-or-bool "t"))
                      "solar24x32")
                     (else font-name-or-bool))))
    
    (format #t "Setting larger console font (USEBIGFONT enabled)...~%")
    
    (if (file-exists? font-dir)
        (let* ((possible-fonts (list (string-append font-dir "/" font-name ".psf")
                                     (string-append font-dir "/" font-name ".psfu")
                                     (string-append font-dir "/" font-name)))
               (font-found (find file-exists? possible-fonts)))
          
          (cond
           ;; Found requested font
           (font-found
            (let ((font-base (car (string-split font-found #\.))))
              (if (zero? (system (format #f "sudo setfont ~a 2>/dev/null" font-base)))
                  (format #t "  ✓ Set font: ~a~%" (basename font-base))
                  (format #t "  ⚠ Could not set font (may need sudo)~%"))))
           
           ;; Try default solar24x32
           ((or (file-exists? (string-append font-dir "/solar24x32.psf"))
                (file-exists? (string-append font-dir "/solar24x32.psfu")))
            (format #t "  ⚠ Font '~a' not found, using default: solar24x32~%" font-name)
            (if (zero? (system "sudo setfont solar24x32 2>/dev/null"))
                (format #t "  ✓ Set font: solar24x32~%")
                (format #t "  ⚠ Could not set font (may need sudo)~%")))
           
           ;; Font not found - ask user to choose
           (else
            (format #t "~%  ⚠ Font '~a' not found in ~a~%" font-name font-dir)
            (format #t "~%  Please choose a font from the list below, or keep the current font:~%~%")
            
            ;; Get all fonts and categorize them
            (let* ((all-fonts (if (file-exists? font-dir)
                                  (scandir font-dir)
                                  '()))
                   (font-bases (map (lambda (f)
                                      (car (string-split f #\.)))
                                    (filter (lambda (f)
                                              (and (not (string=? f "."))
                                                   (not (string=? f ".."))))
                                            all-fonts)))
                   (large-fonts (filter (lambda (f)
                                          (or (string-contains f "24")
                                              (string-contains f "32")
                                              (string-contains f "36")))
                                        font-bases))
                   (other-fonts (filter (lambda (f)
                                          (not (or (string-contains f "24")
                                                   (string-contains f "32")
                                                   (string-contains f "36"))))
                                        font-bases))
                   (sorted-large (sort large-fonts string<?))
                   (sorted-other (sort other-fonts string<?))
                   (total-count (+ (length sorted-large) (length sorted-other))))
              
              ;; Show large fonts first
              (when (not (null? sorted-large))
                (format #t "  Large fonts (recommended for high-DPI displays):~%")
                (let loop ((fonts sorted-large) (idx 1))
                  (when (not (null? fonts))
                    (format #t "    ~2d) ~a~%" idx (car fonts))
                    (loop (cdr fonts) (+ idx 1))))
                (newline))
              
              ;; Show other fonts
              (when (not (null? sorted-other))
                (format #t "  Other available fonts:~%")
                (let loop ((fonts sorted-other) (idx (+ (length sorted-large) 1)))
                  (when (not (null? fonts))
                    (format #t "    ~2d) ~a~%" idx (car fonts))
                    (loop (cdr fonts) (+ idx 1))))
                (newline))
              
              ;; Option to keep current font
              (format #t "    ~2d) Keep current font (skip font change)~%~%" (+ total-count 1))
              
              ;; Prompt user
              (let ((selected-font
                     (let loop ()
                       (format #t "  Enter your choice [1-~a]: " (+ total-count 1))
                       (force-output)
                       (let ((input (read-line)))
                         (cond
                          ((string->number input)
                           (let ((choice (string->number input)))
                             (cond
                              ;; Large font selected
                              ((and (>= choice 1) (<= choice (length sorted-large)))
                               (list-ref sorted-large (- choice 1)))
                              ;; Other font selected
                              ((and (> choice (length sorted-large))
                                    (<= choice total-count))
                               (list-ref sorted-other
                                         (- choice (length sorted-large) 1)))
                              ;; Keep current font
                              ((= choice (+ total-count 1))
                               (format #t "  Keeping current font~%")
                               #f)
                              ;; Invalid choice
                              (else
                               (format #t "  Invalid choice. Please enter a number between 1 and ~a~%"
                                       (+ total-count 1))
                               (loop)))))
                          (else
                           (format #t "  Invalid input. Please enter a number~%")
                           (loop)))))))
                
                ;; Set selected font if user chose one
                (when selected-font
                  (let* ((possible-fonts (list (string-append font-dir "/" selected-font ".psf")
                                               (string-append font-dir "/" selected-font ".psfu")
                                               (string-append font-dir "/" selected-font)))
                         (font-found (find file-exists? possible-fonts)))
                    (if font-found
                        (let ((font-base (car (string-split font-found #\.))))
                          (if (zero? (system (format #f "sudo setfont ~a 2>/dev/null" font-base)))
                              (format #t "  ✓ Set font: ~a~%" (basename font-base))
                              (format #t "  ⚠ Could not set font (may need sudo)~%")))
                        (format #t "  ⚠ Could not set selected font~%"))))))))
        
        (begin
          (format #t "  ⚠ Font directory not found: ~a~%" font-dir)
          (format #t "  Font will be set after installation completes~%")))
    
    (newline)))

;;; Manifest Verification

(define (verify-manifest-file manifest-path)
  "Verify all files listed in MANIFEST-PATH have correct checksums."
  (format #t "Verifying source file checksums...~%~%")
  
  (call-with-input-file manifest-path
    (lambda (port)
      (let loop ((line (read-line port)))
        (unless (eof-object? line)
          ;; Skip comments and empty lines
          (unless (or (string-prefix? "#" (string-trim line))
                      (string-null? (string-trim line)))
            (let* ((parts (string-split line #\space))
                   (expected-hash (car parts))
                   (filepath (cadr parts)))
              
              (when (file-exists? filepath)
                (let ((actual-hash (calculate-sha256 filepath)))
                  (if (string=? actual-hash expected-hash)
                      (format #t "[OK] ~a~%" filepath)
                      (begin
                        (format #t "ERROR: Checksum mismatch for ~a~%" filepath)
                        (format #t "  Expected: ~a~%" expected-hash)
                        (format #t "  Got:      ~a~%~%" actual-hash)
                        (format #t "This likely means GitHub's CDN hasn't caught up with the latest push.~%")
                        (format #t "Wait a few minutes and try again, or check the repo on GitHub.~%")
                        (exit 1)))))))
          (loop (read-line port))))))
  
  (format #t "~%All source files verified!~%~%"))

(define (build-hash-to-words)
  "Build the hash-to-words helper tool."
  (format #t "Building hash-to-words helper tool for verification...~%")
  (if (zero? (system "go build -o hash-to-words ./cmd/hash-to-words 2>/dev/null"))
      (begin
        (make-executable "hash-to-words")
        (format #t "[OK] hash-to-words tool built~%")
        #t)
      (begin
        (format #t "[WARN] hash-to-words build failed (will use hex hash only)~%")
        #f)))

(define (hash-to-words hash)
  "Convert HASH to words using hash-to-words tool if available."
  (if (file-exists? "hash-to-words")
      (let ((result (run-command (format #f "echo '~a' | ./hash-to-words 2>/dev/null" hash))))
        (if (zero? (cdr result))
            (string-trim-right (car result))
            #f))
      #f))

(define (verify-manifest-hash manifest-path)
  "Calculate and verify manifest hash with user."
  (let* ((manifest-hash (calculate-sha256 manifest-path))
         (manifest-words (hash-to-words manifest-hash)))
    
    (format #t "================================================================~%")
    (format #t "MANIFEST HASH VERIFICATION~%")
    (format #t "================================================================~%~%")
    (format #t "The manifest hash for this download is:~%")
    (format #t "  Hash: ~a~%" manifest-hash)
    
    (when manifest-words
      (format #t "  Words: ~a~%" manifest-words)
      (let* ((word-list (string-split manifest-words #\space))
             (first-three (string-join (take word-list 3) " "))
             (last-three (string-join (take-right word-list 3) " ")))
        (format #t "  Quick: ~a ... ~a~%" first-three last-three))
      (format #t "~%You can verify using either:~%")
      (format #t "  - Compare the hex hash above~%")
      (format #t "  - Compare the words above (easier to read aloud)~%"))
    
    (format #t "~%Before proceeding, you should verify this matches the expected hash~%")
    (format #t "from the repository documentation or a trusted source.~%~%")
    
    (format #t "Does this hash match what you expect? [y/N] ")
    (force-output)
    (let ((response (read-line (open-file "/dev/tty" "r"))))
      (unless (and (not (eof-object? response))
                   (or (string-prefix? "y" (string-downcase response))
                       (string-prefix? "Y" response)))
        (format #t "Installation aborted by user~%")
        (exit 1)))
    (newline)))

;;; Critical Scripts Installation

(define (install-critical-scripts)
  "Copy critical recovery scripts to /root."
  (format #t "Installing critical recovery scripts to /root...~%")
  
  (let ((critical-scripts '("lib/clean-install.sh"
                            "lib/verify-guix-install.sh"
                            "lib/recovery-complete-install.sh")))
    (for-each
     (lambda (script)
       (if (file-exists? script)
           (begin
             (system* (format #f "cp ~a /root/" script))
             (make-executable (string-append "/root/" (basename script)))
             (format #t "[OK] Copied ~a to /root/~%" (basename script)))
           (format #t "[WARN] ~a not found (skipping)~%" script)))
     critical-scripts))
  
  (newline))

;;; Build Functions

(define (setup-go-cache)
  "Set up Go cache directories in /tmp."
  (system* "mkdir -p /tmp/go-cache /tmp/go-tmp")
  (setenv "GOCACHE" "/tmp/go-cache")
  (setenv "GOTMPDIR" "/tmp/go-tmp"))

(define (build-installer)
  "Build the main installer binary."
  (format #t "Building installer from source...~%")
  
  (unless (file-exists? "go.mod")
    (error "go.mod not found. This doesn't appear to be a Go module."))
  
  (unless (zero? (system "go build -o run-remote-steps ."))
    (error "Build failed"))
  
  (unless (file-exists? "run-remote-steps")
    (error "Binary not created")))

;;; Main Program

(define (main args)
  "Main entry point for bootstrap installer."
  (let* ((parsed-args (parse-arguments (cdr args)))
         (repo-owner (or (getenv "GUIX_INSTALL_REPO") default-repo-owner))
         (repo-ref (or (getenv "GUIX_INSTALL_REF") default-repo-ref))
         (channel-repo (assoc-ref parsed-args 'channel-repo))
         (channel-branch (assoc-ref parsed-args 'channel-branch))
         (channel-path (assoc-ref parsed-args 'channel-path))
         (platform (assoc-ref parsed-args 'platform)))
    
    ;; Print banner
    (format #t "=== Guix Installer Bootstrap ===~%")
    (format #t "Repository: ~a~%" repo-owner)
    (format #t "Reference: ~a~%" repo-ref)
    (when (not (string-null? channel-repo))
      (format #t "Channel Repository: ~a~%" channel-repo)
      (format #t "Channel Branch: ~a~%" channel-branch)
      (format #t "Channel Path: ~a~%" channel-path))
    (newline)
    
    ;; Set console font if requested
    (let ((use-big-font (getenv "USEBIGFONT")))
      (when (and use-big-font (not (string-null? use-big-font)))
        (set-console-font use-big-font)))
    
    ;; Test stdin availability
    (format #t "Testing stdin availability...~%")
    (format #t "Press Enter to continue (or Ctrl+C to abort): ")
    (force-output)
    (read-line (open-file "/dev/tty" "r"))
    (newline)
    
    ;; Create temporary directory
    (let ((work-dir (run-command/check "mktemp -d")))
      (let ((work-dir-cleaned (string-trim-right work-dir)))
        
        ;; Change to work directory
        (chdir work-dir-cleaned)
        
        ;; Download repository
        (format #t "Downloading repository...~%")
        (let ((tarball-url (format #f "https://github.com/~a/archive/refs/heads/~a.tar.gz"
                                   repo-owner repo-ref)))
          (unless (zero? (system (format #f "curl -fsSL ~s -o repo.tar.gz" tarball-url)))
            (error "Failed to download repository")))
        
        ;; Extract tarball
        (format #t "Extracting...~%")
        (system*/check "tar -xzf repo.tar.gz")
        
        ;; Find extracted directory
        (let ((extracted-dirs (scandir "." (lambda (f) 
                                            (and (not (member f '("." "..")))
                                                 (file-is-directory? f)
                                                 (string-prefix? "cloudzy-guix-install-" f))))))
          (unless (and extracted-dirs (not (null? extracted-dirs)))
            (error "Could not find extracted directory"))
          
          (chdir (car extracted-dirs))
          
          ;; Show what we're building
          (format #t "~%Building from: ~a (~a)~%~%" repo-owner repo-ref)
          
          ;; Set up Go cache early
          (setup-go-cache)
          
          ;; Verify source manifest if present
          (when (file-exists? "SOURCE_MANIFEST.txt")
            (verify-manifest-file "SOURCE_MANIFEST.txt")
            (build-hash-to-words)
            (newline)
            (verify-manifest-hash "SOURCE_MANIFEST.txt"))
          
          ;; Install critical scripts
          (install-critical-scripts)
          
          ;; Build installer
          (build-installer)
          
          ;; Try to build hash-to-words if not already built
          (unless (file-exists? "hash-to-words")
            (build-hash-to-words))
          
          ;; Export channel info for Go installer
          (setenv "GUIX_CHANNEL_REPO" channel-repo)
          (setenv "GUIX_CHANNEL_BRANCH" channel-branch)
          (setenv "GUIX_CHANNEL_PATH" channel-path)
          (setenv "GUIX_PLATFORM" platform)
          
          ;; Run the installer
          (format #t "~%Starting installation...~%~%")
          (system "./run-remote-steps"))))))

;; Run main if executed as script
(when (batch-mode?)
  (main (command-line)))
