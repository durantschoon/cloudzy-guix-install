#!/usr/bin/env -S guile --no-auto-compile -s
!#
;;; Guile helper for safely modifying Guix config.scm files
;;; This script properly parses and manipulates S-expressions
;;;
;;; Note: Shebang uses env -S for portability between Guix and other systems
;;; On Guix, you can also run with: guile --no-auto-compile -s guile-config-helper.scm

(use-modules (ice-9 pretty-print)
             (ice-9 match)
             (ice-9 rdelim)
             (srfi srfi-1))

;;; Read all S-expressions from config file
(define (read-config config-file)
  (call-with-input-file config-file
    (lambda (port)
      (let loop ((exprs '()))
        (let ((expr (read port)))
          (if (eof-object? expr)
              (reverse exprs)
              (loop (cons expr exprs))))))))

;;; Write all S-expressions back to file with pretty printing
(define (write-config exprs config-file)
  (call-with-output-file config-file
    (lambda (port)
      (for-each (lambda (expr)
                  (pretty-print expr port))
                exprs))))

;;; Check if a module is in use-modules
(define (has-module? use-modules-expr module)
  (match use-modules-expr
    (('use-modules modules ...)
     (member module modules))
    (_ #f)))

;;; Add a module to use-modules if not present
(define (add-module-to-use-modules use-modules-expr module)
  (match use-modules-expr
    (('use-modules modules ...)
     (if (member module modules)
         use-modules-expr
         `(use-modules ,@modules ,module)))
    (_ use-modules-expr)))

;;; Check if a service is in the services list
(define (has-service? services-expr service-expr)
  (match services-expr
    (('append ('list services ...) rest ...)
     (member service-expr services))
    (('list services ...)
     (member service-expr services))
    (_ #f)))

;;; Add a service to the services field
(define (add-service-to-services services-expr service-expr)
  (match services-expr
    ;; Already using append with a list
    (('append ('list services ...) base-services ...)
     (if (member service-expr services)
         services-expr
         `(append (list ,@services ,service-expr) ,@base-services)))

    ;; Just %base-services - wrap in append
    ('%base-services
     `(append (list ,service-expr) %base-services))

    ;; Simple list - add to it
    (('list services ...)
     (if (member service-expr services)
         services-expr
         `(list ,@services ,service-expr)))

    ;; Unknown structure - return as-is
    (_ services-expr)))

;;; Modify the services field in operating-system
(define (modify-os-services os-expr service-expr)
  (match os-expr
    (('operating-system fields ...)
     (let loop ((fields fields)
                (result '()))
       (match fields
         (() `(operating-system ,@(reverse result)))
         ((('services services-expr) rest ...)
          (loop rest
                (cons `(services ,(add-service-to-services services-expr service-expr))
                      result)))
         ((field rest ...)
          (loop rest (cons field result))))))
    (_ os-expr)))

;;; Process all expressions, adding module and service
(define (process-exprs exprs module service-expr)
  (let loop ((exprs exprs)
             (result '())
             (module-added? #f))
    (match exprs
      (() (reverse result))

      ;; Handle use-modules - add our module if needed
      (((and use-mod ('use-modules mods ...)) rest ...)
       (loop rest
             (cons (add-module-to-use-modules use-mod module) result)
             #t))

      ;; Handle operating-system - add our service
      (((and os-expr ('operating-system fields ...)) rest ...)
       (loop rest
             (cons (modify-os-services os-expr service-expr) result)
             module-added?))

      ;; Other expressions - keep as-is
      ((expr rest ...)
       (loop rest (cons expr result) module-added?)))))

;;; Main command handlers
(define (cmd-add-service config-file module-name service-expr-str)
  (let* ((exprs (read-config config-file))
         (service-expr (call-with-input-string service-expr-str read))
         (module (call-with-input-string module-name read))
         (modified-exprs (process-exprs exprs module service-expr)))
    (write-config modified-exprs config-file)
    (display "Service added successfully\n")))

(define (cmd-check-service config-file service-type)
  (let ((exprs (read-config config-file))
        (target-service `(service ,(string->symbol service-type))))
    (let loop ((exprs exprs))
      (match exprs
        (() (display "no\n") (exit 1))

        ;; Found operating-system with services
        ((('operating-system fields ...) rest ...)
         (let ((services-field (assoc 'services
                                      (filter-map (lambda (f)
                                                    (match f
                                                      (('services val) (cons 'services val))
                                                      (_ #f)))
                                                  fields))))
           (if (and services-field
                    (has-service? (cdr services-field) target-service))
               (begin
                 (display "yes\n")
                 (exit 0))
               (loop rest))))

        ;; Not an operating-system, keep looking
        ((_ rest ...)
         (loop rest))))))

;;; Main entry point
(define (main args)
  (match args
    ((_ "add-service" config-file module-name service-expr)
     (cmd-add-service config-file module-name service-expr))

    ((_ "check-service" config-file service-type)
     (cmd-check-service config-file service-type))

    (_
     (display "Usage:\n")
     (display "  guile-config-helper.scm add-service CONFIG-FILE MODULE SERVICE-EXPR\n")
     (display "  guile-config-helper.scm check-service CONFIG-FILE SERVICE-TYPE\n")
     (exit 1))))

(main (command-line))
