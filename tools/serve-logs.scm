#!/usr/bin/env guile
!#

;;; serve-logs.scm -- Gather logs and serve them over HTTP (Zero Dependency, Zero Copy)
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

(define %source-logs
  '(("guix-install.log" . "/tmp/guix-install.log")
    ("config.scm" . "/mnt/etc/config.scm")
    ("guix-daemon.log" . "/var/log/guix-daemon.log")))

(define (run-command cmd)
  (let* ((pipe (open-pipe* OPEN_READ "sh" "-c" cmd))
         (output (read-string pipe)))
    (close-pipe pipe)
    output))

(define (log-info fmt . args)
  (apply format #t (string-append "[INFO] " fmt "~%") args))

(define (log-error fmt . args)
  (apply format #t (string-append "[ERROR] " fmt "~%") args))

(define (get-ip-address)
  ;; Quick and dirty way to get the primary IP
  (let ((output (run-command "ip route get 1.1.1.1 | awk '{print $7}' | head -n 1")))
    (string-trim-right output)))

(define (get-dmesg)
  (run-command "dmesg"))

;;; Web Server Logic

(define (list-files-html)
  (let ((links (map (lambda (pair) 
                      (format #f "<li><a href=\"/~a\">~a</a> (~a)</li>" 
                              (car pair) 
                              (car pair)
                              (if (file-exists? (cdr pair)) "Available" "Missing")))
                    %source-logs)))
    (string-append
     "<html><head><title>Installation Logs</title></head>"
     "<body><h1>Installation Logs</h1>"
     "<ul>"
     (string-join links "\n")
     "<li><a href=\"/dmesg\">dmesg</a> (Generated dynamically)</li>"
     "</ul></body></html>")))

(define (request-handler request body)
  (let* ((path (uri-path (request-uri request)))
         (clean-path (if (string-prefix? "/" path) (substring path 1) path)))
    
    (cond
     ;; Serve index
     ((string-null? clean-path)
      (values '((content-type . (text/html)))
              (list-files-html)))
     
     ;; Serve dmesg dynamically
     ((string=? clean-path "dmesg")
      (values '((content-type . (text/plain)))
              (get-dmesg)))
     
     ;; Serve logged files directly from source (Zero Copy)
     ((assoc clean-path %source-logs)
      (let ((file-path (cdr (assoc clean-path %source-logs))))
        (if (file-exists? file-path)
            (let ((content (call-with-input-file file-path get-bytevector-all)))
              (values `((content-type . (text/plain)))
                      content))
            (values '((content-type . (text/plain)))
                    (format #f "File not found at source: ~a" file-path)))))
     
     ;; 404
     (else
      (values '((content-type . (text/plain)))
              "404 Not Found")))))

(define (serve-logs ip)
  (log-info "Starting HTTP server...")
  (format #t "~%~%=================================================~%")
  (format #t "   LOG SERVER RUNNING (Zero Copy)~%")
  (format #t "   Access logs at: http://~a:8000/~%" ip)
  (format #t "   (Press Ctrl+C to stop)~%")
  (format #t "=================================================~%~%")
  
  (run-server request-handler 'http '(#:port 8000)))

(define (main args)
  (log-info "Initializing log server...")
  
  ;; No copying needed!
  
  ;; Get IP and Serve
  (let ((ip (get-ip-address)))
    (if (string-null? ip)
        (log-error "Could not detect IP address.")
        (serve-logs ip))))

;; Only run main if not in testing mode
(unless (defined? '%testing-mode)
  (main (command-line)))
