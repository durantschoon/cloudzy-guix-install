;;; oci-common.scm --- shared helpers for the Oracle Cloud scripts.
;;;
;;; Loaded (not imported as a module) by the numbered scripts in this
;;; directory, so it must stay free of side effects at load time.
;;;
;;; Design constraints these helpers encode:
;;;   - All user prompts read from /dev/tty, never stdin (repo pattern:
;;;     stdin may be redirected by the caller).
;;;   - The oci CLI is always invoked with --query/--raw-output so no
;;;     JSON parser is needed in Guile (guile-json is not in core and
;;;     this script must run on a bare `guix install python`-level box).
;;;   - Output is plain ASCII: [OK] / [ERROR], no Unicode.

(use-modules (ice-9 popen)
             (ice-9 rdelim)
             (ice-9 textual-ports)
             (srfi srfi-1))

;;; ---------------------------------------------------------------------
;;; Process helpers

(define (run-command cmd)
  "Run shell command CMD, return its stdout as a trimmed string.
Stderr passes through to the terminal."
  (let* ((port (open-input-pipe cmd))
         (output (get-string-all port))
         (status (close-pipe port)))
    (string-trim-both output)))

(define (run-command/status cmd)
  "Run shell command CMD, return two values: trimmed stdout and exit status."
  (let* ((port (open-input-pipe cmd))
         (output (get-string-all port))
         (status (close-pipe port)))
    (values (string-trim-both output)
            (status:exit-val status))))

(define (command-succeeds? cmd)
  "Return #t if CMD exits 0.  Both stdout and stderr are discarded."
  (call-with-values
      (lambda () (run-command/status (string-append cmd " 2>/dev/null")))
    (lambda (output status) (zero? status))))

(define (sh-quote str)
  "Single-quote STR for safe interpolation into a shell command."
  (string-append "'" (string-join (string-split str #\') "'\\''") "'"))

;;; ---------------------------------------------------------------------
;;; Terminal helpers

(define (say . parts)
  "Print PARTS followed by a newline."
  (for-each display parts)
  (newline))

(define (prompt-tty question)
  "Ask QUESTION and read one line from /dev/tty (never stdin)."
  (display question)
  (display " ")
  (force-output)
  (call-with-input-file "/dev/tty" read-line))

(define (prompt-yes? question)
  "Ask a yes/no QUESTION on /dev/tty; empty answer means yes."
  (let ((answer (prompt-tty (string-append question " [Y/n]"))))
    (or (string-null? answer)
        (memv (string-ref answer 0) '(#\y #\Y)))))

(define (die . parts)
  "Print PARTS as an [ERROR] line and exit 1."
  (display "[ERROR] ")
  (for-each display parts)
  (newline)
  (exit 1))

;;; ---------------------------------------------------------------------
;;; Paths and configuration

(define (home-path . parts)
  "Join PARTS onto $HOME."
  (string-join (cons (getenv "HOME") parts) "/"))

(define %oci-cli (home-path ".venvs" "oci-cli" "bin" "oci"))
(define %oci-venv-python (home-path ".venvs" "oci-cli" "bin" "python3"))
(define %oci-config (home-path ".oci" "config"))

(define (oci-config-value key)
  "Read KEY from the [DEFAULT] section of ~/.oci/config, or #f."
  (and (file-exists? %oci-config)
       (let ((match (run-command
                     (string-append "command grep -m1 '^" key "=' "
                                    (sh-quote %oci-config)
                                    " 2>/dev/null | command cut -d= -f2-"))))
         (and (not (string-null? match)) match))))

(define (oci cmd)
  "Run an oci CLI subcommand string CMD, return trimmed stdout.
SUPPRESS_LABEL_WARNING silences the key-label advice on every call."
  (run-command
   (string-append "SUPPRESS_LABEL_WARNING=True " %oci-cli " " cmd)))

(define (oci/status cmd)
  "Like `oci' but returns (values stdout exit-status)."
  (run-command/status
   (string-append "SUPPRESS_LABEL_WARNING=True " %oci-cli " " cmd)))

(define (oci-authenticated?)
  "Return #t if the oci CLI can make an authenticated API call."
  (command-succeeds?
   (string-append "SUPPRESS_LABEL_WARNING=True " %oci-cli
                  " iam region-subscription list --output table"
                  " >/dev/null")))

;;; ---------------------------------------------------------------------
;;; Polling

(define (poll-until description thunk interval-seconds max-seconds)
  "Call THUNK every INTERVAL-SECONDS until it returns non-#f or
MAX-SECONDS elapse.  Returns the thunk's value, or #f on timeout.
Prints DESCRIPTION once and a dot per attempt so the user sees life."
  (say "Waiting for " description " (up to " max-seconds "s)...")
  (let loop ((elapsed 0))
    (let ((result (thunk)))
      (cond
       (result
        (newline)
        result)
       ((>= elapsed max-seconds)
        (newline)
        #f)
       (else
        (display ".")
        (force-output)
        (sleep interval-seconds)
        (loop (+ elapsed interval-seconds)))))))
