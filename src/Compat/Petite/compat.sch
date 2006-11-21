; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Compatibility library for the new Twobit under Petite Chez Scheme,
; version 6.0 or later.
;
; 8 May 2001

(optimize-level 1)                      ; Full opt.; no inlining; safe.

(define ($$trace x) #t)

(define host-system 'chez)

(define (write-byte bytenum . rest)
  (let ((port (if (null? rest) 
                  (current-output-port)
                  (car rest))))
    (write-char (integer->char bytenum) port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Initialization

(define chez-compile-file compile-file)
(define *file-list* '())

(define (compat:initialize)
  (let ((hostdir (nbuild-parameter 'compatibility)))
    (if (eq? 'little (nbuild-parameter 'target-endianness))
	(begin
	  (load (string-append hostdir "bytevec-el.ss"))
	  (load (string-append hostdir "misc2bytevector-el.ss")))
	(begin
	  (load (string-append hostdir "misc2bytevector.ss"))
	  (load (string-append hostdir "bytevec.ss"))))
    (load (string-append hostdir "logops.ss"))
    (print-vector-length #f)
    (print-gensym #f)
    #t))

(define (compat:load filename)
  (define (loadit fn)
    (if (nbuild-parameter 'verbose-load?)
	(begin (display fn)
	       (newline)))
    (load fn))
  (set! *file-list* (cons (cons filename (optimize-level)) *file-list*))
  (let ((cfn (chez-new-extension filename "so")))
    (if (and (file-exists? cfn)
	     (compat:file-newer? cfn filename))
	(loadit cfn)
	(loadit filename))))

(define (with-optimization level thunk)
  (parameterize ((optimize-level level))
    (thunk)))

; Calls thunk1, and if thunk1 causes an error to be signalled, calls thunk2.

(define (call-with-error-control thunk1 thunk2)
  (let ((eh (error-handler)))
    (error-handler (lambda args
		     (error-handler eh)
		     (thunk2)
		     (apply eh args)))
    (thunk1)
    (error-handler eh)))

(define (call-without-interrupts thunk)
  (thunk))

(define (chez-new-extension fn ext)
  (let* ((l (string-length fn))
	 (x (let loop ((i (- l 1)))
	      (cond ((< i 0) #f)
		    ((char=? (string-ref fn i) #\.) (+ i 1))
		    (else (loop (- i 1)))))))
    (if (not x)
	(string-append fn "." ext)
	(string-append (substring fn 0 x) ext))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Larceny-specific objects

(define *undefined-expression* ''*undefined*)
(define *unspecified-expression* ''*unspecified*)
(define *eof-object* ''*eof-object*)

(define (unspecified) *unspecified-expression*)
(define (undefined) *undefined-expression*)
(define (eof-object) *eof-object*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Common non-standard operations

(define some? ormap)
(define every? andmap)

(define (filter select? list)
  (cond ((null? list) list)
	((select? (car list))
	 (cons (car list) (filter select? (cdr list))))
	(else
	 (filter select? (cdr list)))))

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

(define (compat:sort list less?)
  (sort less? list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Well-defined character codes.
; Returns the UCS-2 code for a character.

(define compat:char->integer char->integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Error handling.
; Chez Scheme's 'error' is incompatible with the format of the error messages
; used in Twobit.

(define error
  (let ((old-error error))
    (lambda (msg . irritants)
      (display "error: ")
      (display msg)
      (for-each (lambda (x) (display " ") (display x)) irritants)
      (newline)
      (old-error '() ""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Symbol generation
; Chez Scheme's gensym is incompatible with the use of gensym in Twobit.
(define old-gensym-of-chez-scheme 
  (if (top-level-bound? 'old-gensym-of-chez-scheme) ;; idempotency
      old-gensym-of-chez-scheme
      gensym))

(define gensym
  (let ((n 0))
    (lambda (x)
      (set! n (+ n 1))
      (string->uninterned-symbol (format "~a~a" x n)))))

; string->uninterned-symbol was removed in Chez Scheme 6.9c, replaced
; by overloading gensym and dispatching based on argument number.  I'm
; not certain what the old spec was, but I'm guessing this will do the
; trick (with the hope that top-level-bound? is defined in older
; versions of Chez).  It could very well be that in Chez 6.9c, we no
; longer need to redefine gensym to get behavior compatible with
; Larceny.  But I will not speculate further.
(define string->uninterned-symbol
  (if (top-level-bound? 'string->uninterned-symbol)
      string->uninterned-symbol
      (lambda (str) (old-gensym-of-chez-scheme str))))

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

; This needs to be a random number in both a weaker and stronger sense
; than `random': it doesn't need to be a truly random number, so a sequence
; of calls can return a non-random sequence, but if two processes generate
; two sequences, then those sequences should not be the same.
;
; Gross, huh?

(define an-arbitrary-number
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (string-hash (string-append (number->string counter) (date-and-time))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input and output

;; pnkfelix is hoping that Chez does something compatible with what
;; MzScheme does in this cases, because I can't figure out how to open
;; files in "binary mode" from its documentation.
(define open-binary-output-file open-output-file)
(define open-binary-input-file open-input-file)

(define call-with-binary-output-file call-with-output-file)
(define call-with-binary-input-file call-with-input-file)
(define with-binary-output-to-file with-output-to-file)
(define with-binary-input-from-file with-input-from-file)

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

(define call-with-binary-input-file call-with-input-file)
(define call-with-binary-output-file call-with-output-file)
(define with-binary-input-from-file with-input-from-file)
(define with-binary-output-from-file with-input-from-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; UNIX interface to support 'make'.
;
; Defines two procedures:
;
;  (file-exists? string)
;     returns #t if the file exists and #f if not
;
;  (file-modification-time string)
;     returns a timestamp for the file

#;(define file-exists?
  (let ((access
	 ; Using 'access' system call.
	 (foreign-procedure "access" (string integer-32) integer-32)))
    (lambda (f)
      (not (= (access f 0) -1)))))

(define (file-modification-time x)
  ; This can be customized on a platform-by-platform basis.
  ; Portably, always returning 0 shall work.  Code that doesn't
  ; get that right is buggy.
  0)

(define (compat:file-newer? a b)
  (>= (file-modification-time a) (file-modification-time b)))

(define (rename-file old new)
  (case (nbuild-parameter 'host-os)
    ((win32 cygwin)
     (system (string-append "del " new))
     (system (string-append "rename " old " " new)))
    ((unix)
     (system (string-append "mv " old " " new)))
    (else
     (error "RENAME-FILE not defined for this OS."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Compilation

(define (chez-compile-files)
  (for-each (lambda (fn)
	      (with-optimization
	       (cdr fn)
	       (lambda ()
		 (chez-compile-file (car fn)
				    (chez-new-extension (car fn) "so")))))
	    *file-list*))


;;;;;;;;;;;;;;;;
; JavaDot is now part of the larceny toplevel, and the compiler
; expects these to be defined.  (Despite that they're useless unless
; Twobit is running on top of Larceny... b/c the reader needs to know
; about these too for them to be useful).

(define (make-bool-retract proc-name)
  (lambda (x)
    (cond ((not (boolean? x))
           (error proc-name "Non boolean argument")))
    x))

(define recognize-javadot-symbols? 
  (make-parameter #f (make-bool-retract 'recognize-javadot-symbols?)))
(define recognize-keywords? 
  (make-parameter #f (make-bool-retract 'recognize-keywords?)))
(define case-sensitive? 
  (make-parameter #f (make-bool-retract 'case-sensitive?)))
(define javadot-symbol? (lambda (x) #f))
(define (javadot-symbol->symbol! x) x)
(define (symbol->javadot-symbol! x) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Misc

(define (cerror . irritants)
  (display "Error: ")
  (for-each display irritants)
  (newline)
  (reset))

(define (environment-syntax-environment env)
  usual-syntactic-environment)

(define (make-parameter name value)
  (lambda v
    (cond ((null? v) value)
	  ((null? (cdr v))
	   (set! value (car v))
	   value)
	  (else
	   (error "Too many params to make-parameter.")))))

;; Petite at some point decided it has to have a system-features defined
;; so that it can define a compiler directly (see file dumpheap-unix.sch)
;; pnkfelix is hacking it so that this will work when we're using
;; petit-setup.sch; I don't know what to do otherwise...
(define (system-features)
  (define (read-line source)
    ;; Pre-condition:  source designates a file open for reading
    ;; Post-conditions:  returns the next line of the file
    ;;                   or an eof-object if the end of the file is encountered
    (let loop ((ch-list '()) (ch (read-char source)))
      (cond ((eof-object? ch) ch)
            ((char=? ch #\newline) (list->string (reverse ch-list)))
            (else (loop (cons ch ch-list) (read-char source))))))

  (if (not (top-level-bound? '*host:os*))
      (error 'system-features "Need to extend system-features in Petit/compat.sch")
      (list (cons 'os-name (case *host:os*
                             ((macosx) "MacOS X")
                             ((windows) "Win32")
                             ((solaris unix)
                              (let ((l (process "uname")))
                                (read-line (car l)))))))))

;;; let-values

; SRFI 11, "Syntax for receiving multiple values"
; For convenience here is Lars Hansen's sample implementaion.

(define-syntax let-values
  (syntax-rules ()
    ((let-values (?binding ...) ?body0 ?body1 ...)
     (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
    
    ((let-values "bind" () ?tmps ?body)
     (let ?tmps ?body))
    
    ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
     (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
    
    ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
     (call-with-values 
       (lambda () ?e0)
       (lambda ?args
         (let-values "bind" ?bindings ?tmps ?body))))
    
    ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
    
    ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (call-with-values
       (lambda () ?e0)
       (lambda (?arg ... . x)
         (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))


; eof
