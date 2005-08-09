;; Why not just require?
;; Well... that doesn't work on certain versions of DrScheme.
(namespace-require/copy '(lib "list.ss"))
(namespace-require/copy '(lib "etc.ss"))
(namespace-require/copy '(lib "process.ss"))
(namespace-require/copy '(prefix mz: mzscheme))

(define ($$trace x) #t)
(define host-system 'mzscheme)

(define version-299? (> (string->number (version)) 299))

;; Initialization

(define (compat:initialize)
  (let ((hostdir (nbuild-parameter 'compatibility)))
    (load (string-append hostdir "logops.ss"))
    (if (eq? 'little (nbuild-parameter 'host-endianness))
        (begin (load (string-append hostdir "bytevec-el.ss"))
               (load (string-append hostdir "misc2bytevector-el.ss")))
        (begin (load (string-append hostdir "bytevec.ss"))
               (load (string-append hostdir "misc2bytevector.ss"))))
    (print-vector-length #f)
    #t))

(define (compat:load filename)
  (define (loadit fn)
    (if (nbuild-parameter 'verbose-load?)
	(begin (display fn)
	       (newline)))
    (load fn))
  (loadit filename))

(define (call-with-error-control thunk1 thunk2) 
  (with-handlers 
   [(values (lambda (exn)

	      ;; delay lookup of print-error-trace as long 
	      ;; as possible, to allow errortrace.ss to be
	      ;; required after this file is loaded.
	      (cond 
	       ((memq 'print-error-trace (mz:namespace-mapped-symbols))
		(let* ((ns-var-val mz:namespace-variable-value)
		       (exn-message (ns-var-val 'exn-message))
		       (print-error-trace (ns-var-val 'print-error-trace)))
		  (display (exn-message exn))
		  (newline)
		  (print-error-trace (current-output-port) exn))))

	      (thunk2)))]
    (thunk1)))


(define (call-with-error-handler handler thunk)
  (with-handlers [(values handler)]
    (thunk)))

(define (call-without-interrupts thunk)
  (thunk))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Larceny-specific objects

(define *undefined-expression* (letrec ((x x)) x))
(define *unspecified-expression* (void))
(define *eof-object* (read (open-input-string "")))

(define (unspecified) *unspecified-expression*)
(define (undefined) *undefined-expression*)
(define (eof-object) *eof-object*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
; Common non-standard operations

(define some? ormap)
(define every? andmap)

(define (find this? list)
  (cond ((null? list) #f)
	((this? (car list)) (car list))
	(else (find this? (cdr list)))))

(define (remove-duplicates list same?)
  (cond ((null? list) list)
	((find (lambda (x)
		 (same? x (car list)))
	       (cdr list))
	 (remove-duplicates (cdr list) same?))
	(else
	 (cons (car list)
	       (remove-duplicates (cdr list) same?)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; A well-defined sorting procedure
(define compat:sort quicksort)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Well-defined character codes.
; Returns the UCS-2 code for a character.

(define compat:char->integer char->integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Error handling.

(define error
  (lambda (msg . irritants)
    (let [(err (open-output-string))]
      (display msg err)
      (for-each (lambda (x) (display " " err) (display x err)) irritants)
      (mz:error 'error (get-output-string err)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Symbol generation
; Chez Scheme's gensym is incompatible with the use of gensym in Twobit.

(define gensym
  (let ((n 0))
    (lambda (x)
      (set! n (+ n 1))
      (string->uninterned-symbol (format "~a~a" x n)))))

(define (symbol-hash sym)
  (string-hash (symbol->string sym)))

(define (string-hash string)
  (define (loop s i h)
    (if (< i 0)
	h
	(loop s
	      (- i 1)
	      (logand 65535 (+ (char->integer (string-ref s i)) h h h)))))
  (let ((n (string-length string)))
    (loop string (- n 1) n)))

(define (an-arbitrary-number)
  (abs (current-milliseconds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input and output

(define (write-lop x p)
  (write x p)
  (newline p)
  (newline p))

; Does not use magic syntax for flonums and compnums, but produces 
; valid data anyway.

(define write-fasl-datum write)

(define (twobit-format port fmt . args)
  (cond ((port? port)
         (display (apply format fmt args) port))
        (port
         (display (apply format fmt args)))
        (else
         (apply format fmt args))))

(define flush-output-port flush-output)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; UNIX interface to support 'make'.

(define file-modification-time
  file-or-directory-modify-seconds)

(define (compat:file-newer? a b)
  (>= (file-modification-time a) (file-modification-time b)))

;; We need delete-file to not raise error if file doesn't exist
(define (delete-file file)
  (if (file-exists? file)
      (mz:delete-file file)))

;; Note that under MzScheme, open-*-file defaults to Binary mode, not Text.

;(define (open-output-file file)
;  (mz:open-output-file file 'truncate/replace))
;
;(define (with-output-to-file file proc)
;  (parameterize [(current-output-port (open-output-file file))]
;    (begin0 (proc)
;            (close-output-port (current-output-port)))))
;(define (call-with-output-file file proc)
;  (let [(out (open-output-file file))]
;    (begin0 (proc out)
;            (close-output-port out))))

(define open-binary-output-file open-output-file)
(define open-binary-input-file open-input-file)

(define call-with-binary-output-file call-with-output-file)
(define call-with-binary-input-file call-with-input-file)
(define with-binary-output-to-file with-output-to-file)
(define with-binary-input-from-file with-input-from-file)

;;;;;;;;;;;;;;;;
; JavaDot is now part of the larceny toplevel, and the compiler
; expects these to be defined.  (Despite that they're useless unless
; Twobit is running on top of Larceny... b/c the reader needs to know
; about these too for them to be useful).

(define (err-on-non-boolean name)
  (lambda (x)
    (if (boolean? x)
        x
        (error name "Passed non boolean"))))


(define recognize-javadot-symbols? 
  (mz:make-parameter #f (err-on-non-boolean 'recognize-javadot-symbols?)))
(define recognize-keywords? 
  (mz:make-parameter #f (err-on-non-boolean 'recognize-keywords?)))
(define case-sensitive? 
  (mz:make-parameter #f (err-on-non-boolean 'case-sensitive?)))
(define javadot-symbol? (lambda (x) #f))
(define (javadot-symbol->symbol! x) x)
(define (symbol->javadot-symbol! x) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Misc


(define (cerror . irritants)
  (error 'error (apply string-append (map (lambda (x) (format "~s" x)) irritants))))

(define (environment-syntax-environment env) env)

(define (vector-copy v)
  (build-vector (vector-length v)
                (lambda (i) (vector-ref v i))))

(define (make-parameter name v)
  (mz:make-parameter v))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= -536870912 x 536870911)))
(define (flonum? x)
  (and (real? x) (inexact? x)))

(define system system/exit-code)


(define *root-directory* (let ((l (current-directory)))
                           (cond
                            ((string? l)  l)
                            (else (path->string l)))))

(define (system-features)
  (define (chop s) (substring s 0 (sub1 (string-length s))))
  (list (cons 'os-name      (case (system-type)
                              ((macosx)  "MacOS X")
                              ((windows) "Win32")
                              ((unix) 
                               (let ((s (open-output-string))) 
                                 (mz:parameterize ((current-output-port s)) 
                                   (system "uname"))
                                 (chop (get-output-string s))))
                              (else 
                               (error 'system-features 
                                      "Add a new case for system-type"))
                              ))))

(define (reset) (error "RESET"))

;; In MzScheme 299, parameterize was changed so that it has to take
;; parameter objects, rather than arbitrary procedures.  This macro
;; reintroduces the old semantics that accepted procedures as well as
;; parameter objects.
(define-syntax parameterize 
  (syntax-rules ()
    ((parameterize ((PARAM EXP) ...) BODY ...)
     (parameterize "help" () ((PARAM EXP) ...) () BODY ...))

    ((parameterize "help" (NAMES ...) ((P E) R ...) (PE ...)       BODY ...)
     (parameterize "help" ((FRESH-1 FRESH-2) NAMES ...) (R ...) (PE ... (P E)) BODY ...))

    ((parameterize "help" ((ORIG RHS) ...) () ((PARAM EXPR) ...) BODY ...)
     (let ((ORIG (PARAM)) ...)
       (let ((RHS EXPR) ...)
         (dynamic-wind 
             (lambda () (PARAM RHS) ...)
             (lambda () BODY   ...)
             (lambda () (PARAM ORIG) ...)))))))

(require (lib "pretty.ss"))
