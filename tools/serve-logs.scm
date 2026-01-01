#!/usr/bin/env guile
!#

;;; serve-logs.scm -- Gather logs and serve them over HTTP (Zero Dependency)
;;; Usage: guile serve-logs.scm

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 format)
             (ice-9 ftw)
             (ice-9 binary-ports)
             (srfi srfi-1)
             (srfi srfi-26)
             (web server)
             (web request)
             (web response)
             (web uri))

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

;;; Web Server Logic

(define (list-files-html dir)
  (let ((files (scandir dir (lambda (name) (not (member name '("." "..")))))))
    (string-append
     "<html><head><title>Installation Logs</title></head>"
     "<body><h1>Installation Logs</h1><ul>"
     (string-join
      (map (lambda (f) (format #f "<li><a href=\"/~a\">~a</a></li>" f f))
           files)
      "\n")
     "</ul></body></html>")))

(define (request-handler request body)
  (let* ((path (uri-path (request-uri request)))
         (clean-path (if (string-prefix? "/" path) (substring path 1) path))
         (file-path (string-append %log-dir "/" clean-path)))
    
    (cond
     ;; Serve index
     ((string-null? clean-path)
      (values '((content-type . (text/html)))
              (list-files-html %log-dir)))
     
     ;; Serve file if exists
     ((and (not (string-contains clean-path "..")) ; Basic security
           (file-exists? file-path)
           (not (file-is-directory? file-path)))
      (let* ((size (stat:size (stat file-path)))
             (content (call-with-input-file file-path get-bytevector-all)))
        (values `((content-type . (text/plain)))
                content)))
     
     ;; 404
     (else
      (values '((content-type . (text/plain)))
              "404 Not Found")))))

(define (serve-logs ip)
  (log-info "Starting HTTP server...")
  (format #t "~%~%=================================================~%")
  (format #t "   LOG SERVER RUNNING (Zero Dependency)~%")
  (format #t "   Access logs at: http://~a:8000/~%" ip)
  (format #t "   (Press Ctrl+C to stop)~%")
  (format #t "=================================================~%~%")
  
  (run-server request-handler 'http '(#:port 8000)))

(define (main args)
  (log-info "Initializing log server...")
  
  ;; 1. Prepare Directory
  (ensure-log-dir)
  
  ;; 2. Gather Logs
  (for-each (lambda (f) 
              (copy-file-safe f (string-append %log-dir "/" (basename f))))
            %source-logs)
  (dump-dmesg)
  
  ;; 3. Get IP and Serve
  (let ((ip (get-ip-address)))
    (if (string-null? ip)
        (log-error "Could not detect IP address.")
        (serve-logs ip))))

;; Only run main if not in testing mode
(unless (defined? '%testing-mode)
  (main (command-line)))
