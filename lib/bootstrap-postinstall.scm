#!/run/current-system/profile/bin/guile \
--no-auto-compile
!#
;;; Bootstrap Postinstall Scripts
;;; Downloads and verifies postinstall customization tools

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 ftw)
             (ice-9 format)
             (srfi srfi-1))

;;; Configuration
(define github-user "durantschoon")
(define github-repo "cloudzy-guix-install")
(define github-branch "main")
(define base-url (format #f "https://raw.githubusercontent.com/~a/~a/~a"
                         github-user github-repo github-branch))

;;; ANSI colors
(define (msg text)
  (format #t "\n\033[1;34m==> ~a\033[0m\n" text))

(define (warn text)
  (format #t "\n\033[1;33m[warn]\033[0m ~a\n" text))

(define (err text)
  (format #t "\n\033[1;31m[err]\033[0m  ~a\n" text))

(define (info text)
  (format #t "  ~a\n" text))

;;; Detect platform based on system characteristics
(define (detect-platform)
  (cond
   ;; Check for dual-boot (GUIX_ROOT filesystem with separate EFI)
   ((and (file-exists? "/proc/mounts")
         (let ((mounts (call-with-input-file "/proc/mounts" read-string)))
           (and (string-contains mounts "GUIX_ROOT")
                (string-contains mounts "/boot/efi")
                (not (string-contains (string-copy mounts
                                                   (string-contains mounts "/boot/efi"))
                                     "GUIX_ROOT")))))
    "framework-dual")

   ;; Check for Raspberry Pi
   ((and (file-exists? "/sys/firmware/devicetree/base/model")
         (call-with-input-file "/sys/firmware/devicetree/base/model"
           (lambda (port)
             (let ((model (read-string port)))
               (string-contains model "Raspberry Pi")))))
    "raspberry-pi")

   ;; Check for Framework laptop via lspci
   ((and (file-exists? "/run/current-system/profile/bin/lspci")
         (let* ((port (open-input-pipe "lspci"))
                (output (read-string port))
                (status (close-pipe port)))
           (string-contains output "Framework")))
    "framework")

   ;; Check for VPS vendors
   ((and (file-exists? "/sys/class/dmi/id/sys_vendor")
         (call-with-input-file "/sys/class/dmi/id/sys_vendor"
           (lambda (port)
             (let ((vendor (string-downcase (read-string port))))
               (or (string-contains vendor "cloudzy")
                   (string-contains vendor "ovh")
                   (string-contains vendor "digitalocean")
                   (string-contains vendor "vultr"))))))
    "cloudzy")

   ;; Default to framework
   (else "framework")))

;;; Execute shell command and return status
(define (system* . args)
  (let ((status (apply system args)))
    (zero? status)))

;;; Download file from GitHub
(define (download-file remote-path local-path)
  (let ((url (format #f "~a/~a" base-url remote-path)))
    (info (format #f "Downloading: ~a" remote-path))

    (if (system* (format #f "curl -fsSL ~s -o ~s" url local-path))
        (begin
          ;; Make executable if it's a script
          (when (or (string-suffix? ".sh" local-path)
                    (string-suffix? ".scm" local-path)
                    (string-suffix? "customize" local-path))
            (chmod local-path #o755))
          #t)
        (begin
          (err (format #f "Failed to download: ~a" url))
          #f))))

;;; Calculate SHA256 checksum of file
(define (file-sha256 filepath)
  (if (file-exists? filepath)
      (let* ((port (open-input-pipe (format #f "sha256sum ~s" filepath)))
             (output (read-line port))
             (status (close-pipe port)))
        (if (and (zero? status) (string? output))
            (car (string-split output #\space))
            #f))
      #f))

;;; Verify file checksum against manifest
(define (verify-checksum filepath manifest-content)
  (if (not (file-exists? filepath))
      (begin
        (warn (format #f "File not found: ~a (skipping verification)" filepath))
        #t)
      (let* ((lines (string-split manifest-content #\newline))
             (matching-line (find (lambda (line)
                                   (string-contains line filepath))
                                 lines)))
        (if matching-line
            (let ((expected-hash (car (string-split matching-line #\space)))
                  (actual-hash (file-sha256 filepath)))
              (if (equal? expected-hash actual-hash)
                  (begin
                    (info (format #f "  ✓ ~a" filepath))
                    #t)
                  (begin
                    (err (format #f "  ✗ ~a (checksum mismatch!)" filepath))
                    (err (format #f "    Expected: ~a" expected-hash))
                    (err (format #f "    Got:      ~a" actual-hash))
                    #f)))
            (begin
              (warn (format #f "File not in manifest: ~a (skipping verification)" filepath))
              #t)))))

;;; Create directory if it doesn't exist
(define (mkdir-p path)
  (unless (file-exists? path)
    (system* (format #f "mkdir -p ~s" path))))

;;; Check if Go command is available and return its path
(define (go-path)
  ;; Use bash to run 'command -v go' since command is a bash builtin
  ;; Also check common Go locations as fallback
  (let* ((port (open-input-pipe "bash -c 'command -v go 2>/dev/null || which go 2>/dev/null || ([ -f /run/current-system/profile/bin/go ] && echo /run/current-system/profile/bin/go) || ([ -f ~/.guix-profile/bin/go ] && echo ~/.guix-profile/bin/go) || echo'"))
         (output (read-line port))
         (status (close-pipe port)))
    (if (and (zero? status) (string? output) (not (string-null? output)) (> (string-length output) 0))
        output
        #f)))

;;; Check if Go is available
(define (go-available?)
  (not (not (go-path))))

;;; Use hash-to-words tool (same as update-manifest.sh)
;;; Downloads and compiles the Go tool if Go is available
;;; base-dir: base directory where files should be downloaded (e.g., ~/guix-customize)
(define (hash-to-words hash-string base-dir)
  ;; Try to use the Go tool if available
  (let ((go-cmd (go-path)))
    (if go-cmd
        ;; Download and compile the tool
        (let* ((tool-dir (string-append base-dir "/tools/hash-to-words"))
               (main-go (string-append tool-dir "/main.go"))
               (words-json (string-append tool-dir "/words.json"))
               (binary (string-append tool-dir "/hash-to-words")))
          ;; Create directory
          (mkdir-p tool-dir)
          
          ;; Download source files from GitHub repo
          (let ((downloaded-main (download-file "cmd/hash-to-words/main.go" main-go))
                (downloaded-words (download-file "cmd/hash-to-words/words.json" words-json)))
            (if (and downloaded-main downloaded-words
                     (file-exists? main-go)
                     (file-exists? words-json))
                ;; Compile the tool
                (if (system* (format #f "cd ~s && ~s build -o ~s ~s"
                                    tool-dir go-cmd binary main-go))
                    ;; Run the tool
                    (let* ((port (open-input-pipe (format #f "echo ~s | ~s"
                                                         hash-string binary)))
                           (output (read-line port))
                           (status (close-pipe port)))
                      (if (and (zero? status) (string? output))
                          output
                          #f))
                    (begin
                      (err "Failed to compile hash-to-words tool")
                      #f))
                (begin
                  (err (format #f "Failed to download hash-to-words source files from repository"))
                  (err (format #f "  Expected main.go at: ~a" main-go))
                  (err (format #f "  Expected words.json at: ~a" words-json))
                  #f))))
        ;; No Go available, return #f to indicate we can't convert
        #f)))

;;; Extract first N and last N words from word string
(define (get-quick-words word-string n)
  (let* ((words (string-split word-string #\space))
         (total (length words)))
    (if (< total (* n 2))
        word-string
        (let ((first-n (take words n))
              (last-n (take-right words n)))
          (format #f "~a ... ~a"
                  (string-join first-n " ")
                  (string-join last-n " "))))))

;;; Main bootstrap process
(define (main args)
  (msg "Guix Postinstall Bootstrap")
  (newline)

  ;; Check if running on Guix
  (unless (file-exists? "/run/current-system/profile/bin/bash")
    (err "This script must run on an installed Guix system")
    (err "Looking for: /run/current-system/profile/bin/bash")
    (exit 1))

  ;; Detect platform
  (let ((platform (detect-platform)))
    (msg (format #f "Detected platform: ~a" platform))
    (newline)

    ;; Create directory structure
    (let ((install-dir (string-append (getenv "HOME") "/guix-customize")))
      (msg (format #f "Creating directory: ~a" install-dir))
      (mkdir-p install-dir)
      (chdir install-dir)

      ;; Download manifest first
      (msg "Downloading source manifest")
      (unless (download-file "SOURCE_MANIFEST.txt" "SOURCE_MANIFEST.txt")
        (err "Failed to download manifest")
        (exit 1))

      ;; Show manifest checksum for manual verification
      (let* ((manifest-hash (file-sha256 "SOURCE_MANIFEST.txt"))
             (hash-words (hash-to-words manifest-hash install-dir))
             (quick-words (if hash-words (get-quick-words hash-words 3) #f)))
        (newline)
        (info (format #f "Manifest Hash: ~a" manifest-hash))
        (if hash-words
            (begin
              (info (format #f "Words: ~a" hash-words))
              (if quick-words
                  (info (format #f "Quick: ~a" quick-words))
                  #t))
            (begin
              (err "FATAL: Cannot convert hash to words - Go is required!")
              (err "")
              (err "The hash-to-words conversion is required for verification.")
              (err "Please install Go before running this script:")
              (err "  Option 1: guix package -i go")
              (err "  Option 2: Install Go via your system package manager")
              (err "")
              (err "After installing Go, run this script again.")
              (newline)
              (exit 1)))
        (newline)
        (info "To verify this hash matches the repository:")
        (info (format #f "  curl -fsSL https://raw.githubusercontent.com/~a/~a/main/SOURCE_MANIFEST.txt | shasum -a 256"
                     github-user github-repo))
        (newline)
        (info "Or check the manifest hash in the repository documentation.")
        (newline))

      ;; Read manifest content for verification
      (let ((manifest-content (call-with-input-file "SOURCE_MANIFEST.txt"
                               read-string)))

        ;; Download Guile library
        (msg "Downloading Guile library scripts")
        (mkdir-p "lib")
        (download-file "lib/guile-config-helper.scm" "lib/guile-config-helper.scm")

        ;; Download platform-specific customize script
        (msg (format #f "Downloading ~a customize script" platform))
        (mkdir-p (string-append platform "/postinstall"))
        (download-file (string-append platform "/postinstall/customize")
                      (string-append platform "/postinstall/customize"))

        ;; Create convenience symlink
        (when (file-exists? "customize")
          (delete-file "customize"))
        (symlink (string-append platform "/postinstall/customize") "customize")

        ;; Download shared recipes
        (msg "Downloading shared recipe scripts")
        (mkdir-p "postinstall/recipes")
        (download-file "postinstall/recipes/add-spacemacs.sh"
                      "postinstall/recipes/add-spacemacs.sh")
        (download-file "postinstall/recipes/add-development.sh"
                      "postinstall/recipes/add-development.sh")
        (download-file "postinstall/recipes/add-fonts.sh"
                      "postinstall/recipes/add-fonts.sh")

        ;; Download postinstall library (for channel management)
        (download-file "lib/postinstall.sh" "lib/postinstall.sh")

        ;; Download shared postinstall library (for customize scripts)
        (download-file "postinstall/lib.sh" "postinstall/lib.sh")

        ;; Verify all downloads
        (msg "Verifying checksums")
        (newline)

        (let ((verify-failed
               (not (and (verify-checksum "lib/guile-config-helper.scm" manifest-content)
                        (verify-checksum (string-append platform "/postinstall/customize")
                                        manifest-content)
                        (verify-checksum "lib/postinstall.sh" manifest-content)
                        (verify-checksum "postinstall/lib.sh" manifest-content)))))

          (newline)

          (if verify-failed
              (begin
                (err "Checksum verification failed!")
                (err "Some files do not match the manifest")
                (err "This could indicate:")
                (err "  - Network corruption during download")
                (err "  - GitHub CDN not yet updated")
                (err "  - Manifest out of sync")
                (newline)
                (err (format #f "To proceed anyway: cd ~a && ./customize" install-dir))
                (exit 1))
              (begin
                (msg "Bootstrap complete!")
                (newline)
                (info (format #f "Postinstall scripts installed to: ~a" install-dir))
                (info (format #f "Platform: ~a" platform))
                (newline)
                (info "To customize your system:")
                (info (format #f "  cd ~a" install-dir))
                (info "  ./customize")
                (newline)
                (info "The customize tool provides:")
                (info "  - Desktop environment installation (GNOME, Xfce, MATE, LXQt)")
                (info "  - Service configuration (NetworkManager, SSH)")
                (info "  - Package installation (development tools, fonts, etc.)")
                (info "  - Hardware support (firmware, drivers)")
                (newline)))))))

;; Run main with command-line arguments
(main (command-line))
