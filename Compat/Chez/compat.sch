; Chez/compat.sch
; Compatibility library for the new Twobit under Chez Scheme
;
; $Id$

(define host-system 'chez)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Initialization

(define chez-compile-file compile-file)
(define *file-list* '())

; Foreign functions (for OS functionality).

(let* ((hostdir (nbuild-parameter 'compatibility))
       (bitpattern (string-append hostdir "bitpattern.o"))
       (mtime      (string-append hostdir "mtime.o"))
       (libc       "/lib/libc.so"))
  (case (machine-type)
    ((sun4)  ; SunOS 4
     (load-foreign bitpattern)
     (load-foreign mtime))
    ((sps2)  ; SunOS 5
     (load-shared-object bitpattern)
     (load-shared-object mtime)
     (load-shared-object libc))
    (else ???)))

(define (compat:initialize)
  (let ((hostdir (nbuild-parameter 'compatibility)))
    (load (string-append hostdir "bytevec.ss"))
    (load (string-append hostdir "misc2bytevector.ss"))
    (load (string-append hostdir "logops.ss"))
    (if (not (bound? 'values))
	(load (string-append hostdir "values.ss")))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; A well-defined sorting procedure

(define (compat:sort list less?)
  (sort less? list))


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

(define twobit-format format)

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

(define file-exists?
  (let ((access
	 ; Using 'access' system call.
	 (foreign-procedure "access" (string integer-32) integer-32)))
    (lambda (f)
      (not (= (access f 0) -1)))))

(define file-modification-time
  ; Using 'mtime' procedure defined in mtime.c.
  (foreign-procedure "mtime" (string) unsigned-32))


(define (compat:file-newer? a b)
  (>= (file-modification-time a) (file-modification-time b)))


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

; eof
