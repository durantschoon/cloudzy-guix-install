#!/run/current-system/profile/bin/guile --no-auto-compile -s
!#

;;; verify-postinstall.scm
;;; Verify postinstall scripts against SOURCE_MANIFEST.txt
;;; Run this after downloading postinstall scripts to ensure integrity

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 match)
             (srfi srfi-1))

;;; Configuration

(define manifest-url
  "https://raw.githubusercontent.com/durantschoon/cloudzy-guix-install/main/SOURCE_MANIFEST.txt")

(define platforms
  '("cloudzy" "framework" "framework-dual" "raspberry-pi"))

;;; Helper functions

(define (download-manifest url)
  "Download manifest from URL, return as string or #f on failure."
  (let* ((port (open-input-pipe (format #f "curl -fsSL ~s" url)))
         (content (read-string port))
         (status (close-pipe port)))
    (if (zero? status)
        content
        #f)))

(define (extract-expected-hash manifest file section)
  "Extract expected hash for FILE from MANIFEST under SECTION.
   Returns hash string or #f if not found."
  (let* ((lines (string-split manifest #\newline))
         ;; Find section header
         (section-header (string-append "## " section))
         (in-section? #f)
         (result #f))
    (let loop ((remaining lines))
      (if (null? remaining)
          result
          (let ((line (car remaining)))
            (cond
             ;; Found our section
             ((string-contains line section-header)
              (set! in-section? #t)
              (loop (cdr remaining)))
             ;; Found next section (stop searching)
             ((and in-section? (string-prefix? "## " line))
              result)
             ;; In our section, look for file
             ((and in-section? (string-contains line file))
              (match (string-split line #\space)
                ((hash filename . rest)
                 (if (string-contains filename file)
                     hash
                     (loop (cdr remaining))))
                (_ (loop (cdr remaining)))))
             (else
              (loop (cdr remaining)))))))))

(define (calculate-file-hash filepath)
  "Calculate SHA256 hash of file at FILEPATH.
   Returns hash string or #f on failure."
  (if (not (file-exists? filepath))
      #f
      (let* ((port (open-input-pipe 
                    (format #f "sha256sum ~s" filepath)))
             (output (read-line port))
             (status (close-pipe port)))
        (if (and (zero? status) (not (eof-object? output)))
            (car (string-split output #\space))
            #f))))

(define (verify-file filepath section manifest)
  "Verify a single file against manifest.
   Returns: 'success, 'not-found, 'not-in-manifest, or 'mismatch
   Also prints verification status."
  (cond
   ;; File doesn't exist
   ((not (file-exists? filepath))
    (format #t "  [SKIP] ~a (not found, skipping)~%" filepath)
    'not-found)
   
   ;; File not in manifest
   ((not (extract-expected-hash manifest filepath section))
    (format #t "  [SKIP] ~a (not in manifest, skipping)~%" filepath)
    'not-in-manifest)
   
   ;; Verify hash
   (else
    (let ((expected-hash (extract-expected-hash manifest filepath section))
          (actual-hash (calculate-file-hash filepath)))
      (if (and actual-hash (string=? actual-hash expected-hash))
          (begin
            (format #t "  [OK] ~a~%" filepath)
            'success)
          (begin
            (format #t "  [FAIL] ~a (checksum mismatch!)~%" filepath)
            (format #t "    Expected: ~a~%" expected-hash)
            (format #t "    Got:      ~a~%" (or actual-hash "ERROR"))
            'mismatch))))))

;;; Verification sections

(define (verify-guile-library-scripts manifest)
  "Verify Guile library scripts section."
  (display "Verifying Guile library scripts...\n")
  (let ((result (verify-file "lib/guile-config-helper.scm"
                             "Guile Library Scripts"
                             manifest)))
    (newline)
    result))

(define (verify-postinstall-customize-scripts manifest)
  "Verify postinstall customize scripts for all platforms."
  (display "Verifying postinstall customize scripts...\n")
  (let ((results
         (filter-map
          (lambda (platform)
            (let ((filepath (format #f "~a/postinstall/customize" platform)))
              (if (file-exists? filepath)
                  (verify-file filepath
                              "Post-Install Customization Scripts"
                              manifest)
                  #f)))
          platforms)))
    (newline)
    ;; Return 'success if all verified or skipped, 'mismatch if any failed
    (if (any (lambda (r) (eq? r 'mismatch)) results)
        'mismatch
        'success)))

(define (verify-library-scripts manifest)
  "Verify other library scripts."
  (display "Verifying library scripts...\n")
  (let ((result (verify-file "lib/postinstall.sh"
                             "Critical Shell Scripts"
                             manifest)))
    (newline)
    result))

;;; Main logic

(define (main args)
  "Main entry point for verification script."
  (display "=== Postinstall Script Verification ===\n")
  (newline)
  
  ;; Download manifest
  (display "Downloading manifest...\n")
  (let ((manifest (download-manifest manifest-url)))
    (if (not manifest)
        (begin
          (display "Error: Failed to download manifest from GitHub\n")
          (exit 1))
        (begin
          (display "Manifest downloaded successfully\n")
          (newline)
          
          ;; Run all verification sections
          (let ((results
                 (list
                  (verify-guile-library-scripts manifest)
                  (verify-postinstall-customize-scripts manifest)
                  (verify-library-scripts manifest))))
            
            ;; Print summary
            (display "=== Verification Complete ===\n")
            (newline)
            
            ;; Check if any mismatches occurred
            (if (any (lambda (r) (eq? r 'mismatch)) results)
                (begin
                  (display "Some files show [FAIL] - re-download those files or check for updates.\n")
                  (exit 1))
                (begin
                  (display "All files show [OK] - your postinstall scripts are verified.\n")
                  (exit 0))))))))

;; Run main if executed as script
(when (string=? (car (command-line)) 
                (string-append (getcwd) "/lib/verify-postinstall.scm"))
  (main (command-line)))
