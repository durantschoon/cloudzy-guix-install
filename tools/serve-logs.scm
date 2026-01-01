#!/usr/bin/env guile
!#

;;; serve-logs.scm -- Gather logs and serve them over HTTP
;;; Usage: guile serve-logs.scm

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 ftw)
             (srfi srfi-1)
             (srfi srfi-26))

(define %log-dir "/root/logs")
(define %source-logs
  '("/tmp/guix-install.log"
    "/mnt/etc/config.scm"
    "/var/log/guix-daemon.log"))

(define (run-command cmd)
  (let* ((pipe (open-pipe* OPEN_READ "sh" "-c" cmd))
         (output (read-string pipe)))
    (close-pipe pipe)
    output))

(define (log-info fmt . args)
  (apply format #t (string-append "[INFO] " fmt "~%") args))

(define (log-error fmt . args)
  (apply format #t (string-append "[ERROR] " fmt "~%") args))

(define (ensure-log-dir)
  (unless (file-exists? %log-dir)
    (log-info "Creating log directory: ~a" %log-dir)
    (mkdir %log-dir)))

(define (copy-file-safe src dest)
  (if (file-exists? src)
      (begin
        (log-info "Copying ~a to ~a" src dest)
        (copy-file src dest))
      (log-info "Skipping missing file: ~a" src)))

(define (dump-dmesg)
  (log-info "Dumping dmesg to ~a/dmesg.txt" %log-dir)
  (system* "sh" "-c" (format #f "dmesg > ~a/dmesg.txt" %log-dir)))

(define (get-ip-address)
  ;; Quick and dirty way to get the primary IP
  (let ((output (run-command "ip route get 1.1.1.1 | awk '{print $7}' | head -n 1")))
    (string-trim-right output)))

(define (check-python)
  (zero? (system* "which" "python3")))

(define (install-python)
  (log-info "Python3 not found. Attempting to install...")
  (log-info "WARNING: This requires downloading data. If disk is full, this may fail.")
  (system* "guix" "package" "-i" "python"))

(define (serve-logs ip)
  (log-info "Starting HTTP server...")
  (format #t "~%~%=================================================~%")
  (format #t "   LOG SERVER RUNNING~%")
  (format #t "   Access logs at: http://~a:8000/~%" ip)
  (format #t "   (Press Ctrl+C to stop)~%")
  (format #t "=================================================~%~%")
  ;; Change to log dir and run python server
  (chdir %log-dir)
  (system* "python3" "-m" "http.server" "8000"))

(define (main args)
  (log-info "Initializing log server...")
  
  ;; 1. Prepare Directory
  (ensure-log-dir)
  
  ;; 2. Gather Logs
  (for-each (lambda (f) 
              (copy-file-safe f (string-append %log-dir "/" (basename f))))
            %source-logs)
  (dump-dmesg)
  
  ;; 3. Check Python
  (unless (check-python)
    (install-python)
    (unless (check-python)
      (log-error "Failed to install Python. Cannot start web server.")
      (exit 1)))
      
  ;; 4. Get IP and Serve
  (let ((ip (get-ip-address)))
    (if (string-null? ip)
        (log-error "Could not detect IP address.")
        (serve-logs ip))))

;; Only run main if not in testing mode
(unless (defined? '%testing-mode)
  (main (command-line)))
