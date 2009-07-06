;; require-4.x-id : Sexp Symbol -> Any

(define (require-4.x-id require-spec id)
  (let ((ns0 (make-namespace)))
    (eval `(require ,require-spec) ns0)
    (eval id ns0)))

(define (version-greater-or-equal-to? n)
  (cond 
   ((and (number? n) (integer? n) (>= n 200) (< n 400))
    (lambda (version-string)
      ;; Below based on Danny Yoo's version-case PLaneT package
      (cond 
       ;; Old-style version string
       ((regexp-match "^([0-9][0-9][0-9])[.0-9]*$" version-string)
	=> (lambda (full-and-part)
	     (>= (string->number (list-ref full-and-part 1)) n)))
       ;; New-style version string; (x.y.z where x >= 4)
       ;; inherently >= 400
       ((regexp-match "^([.0-9])*$"               version-string)
	#t))))
   ;; Strings *only* denote new-style versions!
   ((string? n)
    (lambda (version-string)
      (cond
       ((regexp-match "^([0-9][0-9][0-9])[.0-9]*$" version-string)
	;; Old-style version can't possibly be >= n.
	#f)
       (else
	;; If we have a new style version, use PLT's library 
	;; to do the comparison.
	(let ((version<=?
	       (require-4.x-id 'version/utils 'version<=?)))
	  (version<=? n version-string))))))))

(define version-299? 
  ((version-greater-or-equal-to? 299) (version)))
(define version-301? 
  ((version-greater-or-equal-to? 301) (version)))
(define version-370? 
  ((version-greater-or-equal-to? 370) (version)))
(define version-4.x? 
  ((version-greater-or-equal-to? "4.0.0") (version)))

(define bytes->list
  (cond (version-4.x?
	 (let ((b->i-l bytes->list)
	       (ilist->mlist 
		(require-4.x-id 'scheme/mpair 'list->mlist)))
	   (lambda args
	     (ilist->mlist (apply b->i-l args)))))
	(else
	 bytes->list)))

;; When *exit-on-error* is set, make our error handler die loudly
(cond (*exit-on-error*
       (cond (version-370?
              (error-escape-handler
               (lambda ()
                 (exit 113))))
             (else
              (current-exception-handler 
               (lambda l 
                 (display l)
                 (newline)
                 (exit 113)))))))

;; import "old" syntax and procedures that handle mutable lists
;; reasonably, but *after* code below so we can convert mlist->list

(define namespace-require/copy namespace-require/copy)
(cond
 (version-4.x?
  (namespace-require/copy 'r5rs)))
(cond 
 (version-4.x?
  (set! namespace-require/copy
	(let ((n-r/c namespace-require/copy)
	      (mlist->list 
	       (require-4.x-id 'scheme/mpair 'mlist->list)))
	  (lambda (form)
	    (n-r/c (mlist->list form)))))))

;; Why not just require?
;; Well... that doesn't work on certain versions of DrScheme.
;; More specifically, namespace-require/copy imports bindings
;; into the top-level in a "destructive" way that will change
;; the behavior of (some/most) previously resolved identifiers.
;(namespace-require/copy '(lib "list.ss"))
(define (ormap f l)
  (cond ((null? l) #f) (else (or (f (car l)) (ormap f (cdr l))))))
(define (andmap f l)
  (cond ((null? l) #t) (else (and (f (car l)) (andmap f (cdr l))))))

(namespace-require/copy '(lib "etc.ss"))
(namespace-require/copy '(lib "process.ss"))
(namespace-require/copy '(prefix mz: mzscheme))

(define namespace-mapped-symbols
  (cond
   (version-4.x?
    (let ((ilist->mlist (require-4.x-id 'scheme/mpair 'list->mlist)))
      (lambda args
	(ilist->mlist (apply mz:namespace-mapped-symbols args)))))
   (else
    mz:namespace-mapped-symbols)))

(define eval eval)
(cond (version-4.x?
       (set! eval 
	     (let ((old-eval eval))
	       (lambda args
		 (if (null? (cdr args))
		     (old-eval (car args) 
			       (interaction-environment))
		     (apply old-eval args)))))))

(define ($$trace x) #t)
(define host-system 'mzscheme)

(define read-byte
  (if version-299?
      read-byte
      (lambda (in) 
        (let ((x (read-char in)))
          (if (eof-object? x)
              x
              (char->integer x))))))

(define write-byte 
  (if version-299?
      (let ((write-byte-orig write-byte))
        (lambda (bytenum . rest)
          (let ((byte (if (< bytenum 0)
                          (+ 256 bytenum)
                          bytenum)))
            (apply write-byte-orig byte rest))))
      (lambda (bytenum . rest)
        (let ((port (if (null? rest)
                        (current-output-port)
                        (car rest)))
              (byte (if (< bytenum 0)
                        (+ 256 bytenum)
                        bytenum)))
          (write-char (integer->char byte) port)))))

(define (rename-file old new)
  (case (nbuild-parameter 'host-os)
    ((win32 cygwin)
     (system (string-append "del " new))
     (system (string-append "rename " old " " new)))
    ((unix macosx macosx-el linux-el) ; we need to come up with hierarchical enum's...
     (system (string-append "mv " old " " new)))
    (else
     (error "RENAME-FILE not defined for this OS."))))

;; Initialization

(define (compat:initialize)
  (let ((hostdir (nbuild-parameter 'compatibility)))
    (load (string-append hostdir "logops.ss"))
    (if (eq? 'little (nbuild-parameter 'host-endianness))
        (begin (compat:load (string-append hostdir "bytevec-el.ss"))
               (compat:load (string-append hostdir "misc2bytevector-el.ss")))
        (begin (compat:load (string-append hostdir "bytevec.ss"))
               (compat:load (string-append hostdir "misc2bytevector.ss"))))
    (compat:load (string-append hostdir "compat2.sch"))
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
   ((values (lambda (exn)
	      (display "Hola from compat.sch call-with-error-control")
	      (display " thunk2 proxy")
	      (display exn)
	      (newline)
	      ;; delay lookup of print-error-trace as long 
	      ;; as possible, to allow errortrace.ss to be
	      ;; required after this file is loaded.
	      (cond 
	       ((memq 'print-error-trace (namespace-mapped-symbols))
		(let* ((ns-var-val mz:namespace-variable-value)
		       (exn-message (ns-var-val 'exn-message))
		       (print-error-trace (ns-var-val 'print-error-trace)))
		  (display (exn-message exn))
		  (newline)
		  (print-error-trace (current-output-port) exn))))

	      (thunk2))))
    (thunk1)))


(define (call-with-error-handler handler thunk)
  (with-handlers ((values handler))
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
(define compat:sort 
  (cond
   (version-4.x?
    (let ((mlist->ilist (require-4.x-id 'scheme/mpair 'mlist->list))
	  (ilist->mlist (require-4.x-id 'scheme/mpair 'list->mlist))
	  (quicksort (require-4.x-id 'mzlib/list 'quicksort)))
      (lambda args 
	(ilist->mlist
	 (apply quicksort
		(mlist->ilist (car args))
		(cdr args))))))
   (else
    (namespace-require/copy '(only (lib "list.ss") quicksort))
    quicksort)))

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
    (begin (display "Hola from compat.sch error") (newline))
    (let ((err (open-output-string)))
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
	      (fxlogand 65535 (+ (char->integer (string-ref s i)) h h h)))))
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

(define open-raw-latin-1-output-file open-output-file)
(define open-raw-latin-1-input-file open-input-file)
(define call-with-raw-latin-1-output-file call-with-output-file)
(define call-with-raw-latin-1-input-file call-with-input-file)

; FIXME:  These names should go away as soon as possible.

(define open-binary-input-file open-raw-latin-1-input-file)
(define open-binary-output-file open-raw-latin-1-output-file)
(define call-with-binary-input-file call-with-raw-latin-1-input-file)
(define call-with-binary-output-file call-with-raw-latin-1-output-file)

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

(define copy-file mz:copy-file)

(require (lib "pretty.ss"))

(define alist->hash-table 'not-yet-defined)
;; for Sassy
(define (compat:load-sassy)
  (parameterize ((current-directory "src/Lib/Sassy/"))
    (load (cond
	   (version-4.x?
	    "inits/mzscheme-4.1.5.scm")
	   (version-301?
	    "inits/mzscheme-301.scm")
	   (else
	    "inits/mzscheme-299.400.scm")))
    (load "sassy.scm")
    (set! sassy-text-bytevector
          (lambda (sassy-output)
            (list->bytevector (sassy-text-list sassy-output))))
	    
    ))

;; Parameter to control reader behavior
(define compat:read-case-sensitive? read-case-sensitive)

(define (append! . args)

  (define (loop rest tail)
    (cond ((null? rest)
           tail)
          ((null? (car rest))
           (loop (cdr rest) tail))
          (else
           (loop (cdr rest)
                 (begin (set-cdr! (last-pair (car rest)) tail)
                        (car rest))))))

  (if (null? args)
      '()
      (let ((a (reverse! args)))
        (loop (cdr a) (car a)))))

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(define (reverse! l)
  (define (loop0 prev curr next)
    (set-cdr! curr prev)
    (if (null? next)
        curr
        (loop1 (cdr next) curr next)))
  (define (loop1 next prev curr)
    (set-cdr! curr prev)
    (if (null? next)
        curr
        (loop2 next (cdr next) curr)))
  (define (loop2 curr next prev)
    (set-cdr! curr prev)
    (if (null? next)
        curr
        (loop0 curr next (cdr next))))
  (if (null? l)
      '()
      (loop0 '() l (cdr l))))
