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

; This needs to be a random number in both a weaker and stronger sense
; than `random': it doesn't need to be a truly random number, so a sequence
; of calls can return a non-random sequence, but if two processes generate
; two sequences, then those sequences should not be the same.
;
; Gross, huh?

(define (an-arbitrary-number)
  (system "echo \\\"`date`\\\" > a-random-number")
  (let ((x (string-hash (call-with-input-file "a-random-number" read))))
    (delete-file "a-random-number")
    x))

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
    ((win32)
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
; eof
