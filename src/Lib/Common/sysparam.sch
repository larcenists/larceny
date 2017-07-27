; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- system parameter abstraction.

($$trace "sysparam")

; System parameters are defined in the Library using this procedure.
;
; Larceny's parameters pre-date SRFI 39, which is incompatible with
; Larceny's parameters.  The following definition has been modified
; to be fully compatible with SRFI 39.
;
; With one argument, it's SRFI-style.
; With two arguments, and the second is a procedure, it's SRFI-style.
; With two arguments, and the second is not a procedure, it's not SRFI-style.
; With three arguments, it's not SRFI-style.
;
; Watch out for legacy code that passes a string or symbol as the
; first argument and a procedure as the second argument.  In the
; old days, Larceny's old semantics would be used instead of SRFI 39.
;
; The R7RS says the conversion procedure should not be applied when
; the value is restored coming out of a parameterize form.
; To avoid calling the conversion procedure, SRFI-style and R7RS-style
; parameters created by make-parameter now accept a second argument
; whose only legal value is the symbol no-conversion.

(define (make-parameter arg1 . rest)
  (let* ((srfi39-style? (or (null? rest)
                            (and (null? (cdr rest))
                                 (procedure? (car rest)))))
         (converter (if (and srfi39-style? (pair? rest))
                        (car rest)
                        values))
         (ok? (if (or (null? rest) (null? (cdr rest)))
                  (lambda (x) #t) 
                  (cadr rest)))
         (name (if srfi39-style? #f arg1))
         (value (if srfi39-style? (converter arg1) (car rest))))
    (define (complain-argcount)
      (assertion-violation name "too many arguments" (cons arg1 rest))
      #t)
    (define (complain-bad-value x)
      (assertion-violation name "invalid value for parameter" x)
      #t)
    (if srfi39-style?
        (lambda args
          (if (pair? args)
              (cond ((null? (cdr args))
                     (let ((new-value (converter (car args))))
                       (set! value new-value)
                       value))
                    ((eq? (cadr args) 'no-conversion)
                     (let ((new-value (car args)))
                       (set! value new-value)
                       value))
                    (else
                     (complain-argcount)))
              value))
        (lambda args
          (if (pair? args)
              (if (or (null? (cdr args))
                      (eq? (cadr args) 'no-conversion))
                  (let ((new-value (car args)))
                    (if (ok? new-value)
                        (begin (set! value new-value)
                               value)
                        (complain-bad-value (car args))))
                  (complain-argcount))
              value)))))

; The parameterize form has to be able to recognize R7RS-style
; parameters so it can know whether to pass the symbol no-conversion
; as a second argument.

(define larceny:r7rs-parameter-prototype (make-parameter 0))
(define larceny:old-style-parameter-prototype (make-parameter "p" 0))

(define (parameter? x)
  (and (procedure? x)
       (let ((code (procedure-ref x 0))
             (code1 (procedure-ref larceny:r7rs-parameter-prototype 0))
             (code2 (procedure-ref larceny:old-style-parameter-prototype 0)))
         (or (eq? code code1)
             (eq? code code2)))))

; Returns an assoc list of system information.

(define (system-features)
  (let* ((wordsize
	  (if (fixnum? (expt 2 32)) 64 32))
	 (char-bits
          (let ((c21 (integer->char #x100000))
                (c16 (integer->char #xffff)))
            (cond ((char=? c21 (string-ref (make-string 1 c21) 0))
                   32)
                  ((char=? c16 (string-ref (make-string 1 c16) 0))
                   16)
                  (else 8))))
	 (char-repr
	  (case char-bits
	    ((8)  'iso-latin-1)		; iso 8859/1
	    ((16) 'ucs2)		; 2-byte unicode (not supported)
	    ((32) 'unicode)		; all Unicode characters
	    (else 'unknown)))
         (string-repr
          (let ((s (make-string 1 #\space)))
            (cond ((bytevector-like? s)
                   (case (bytevector-like-length s)
                    ((1) 'flat1)
                    ((4) 'flat4)
                    (else 'unknown)))
                  ((vector-like? s)
                   'unknown)
                  (else 'unknown))))
	 (gc-info
	  (sys$system-feature 'gc-tech)))
    (list (cons 'larceny-major-version  (sys$system-feature 'larceny-major))
	  (cons 'larceny-minor-version  (sys$system-feature 'larceny-minor))
	  (cons 'arch-name              (sys$system-feature 'arch-name))
	  (cons 'arch-endianness        (sys$system-feature 'endian))
	  (cons 'arch-word-size         wordsize)
	  (cons 'os-name                (sys$system-feature 'os-name))
	  (cons 'os-major-version       (sys$system-feature 'os-major))
	  (cons 'os-minor-version       (sys$system-feature 'os-minor))
	  (cons 'fixnum-bits            (- wordsize 2))
	  (cons 'fixnum-representation  'twos-complement)
	  (cons 'codevector-representation (sys$system-feature 'codevec))
	  (cons 'char-bits              char-bits)
	  (cons 'char-representation    char-repr)
          (cons 'string-representation  string-repr)
	  (cons 'flonum-bits            64)
	  (cons 'flonum-representation  'ieee)
          (cons 'case-sensitivity       (not (sys$system-feature 'foldcase)))
          (cons 'transcoder             (sys$system-feature 'transcoder))
          (cons 'safety                 (sys$system-feature 'safety))
          (cons 'execution-mode         (sys$system-feature 'execmode))
          (cons 'ignore-first-line      (sys$system-feature 'ignore1))
          (cons 'pedantic               (sys$system-feature 'pedantic))
          (cons 'r7features             (sys$system-feature 'r7features))
          (cons 'library-path           (sys$system-feature 'r6path))
          (cons 'library-path2          (sys$system-feature 'r6path2))
          (cons 'top-level-program      (sys$system-feature 'r6program))
	  (cons 'gc-technology          (car gc-info))
	  (cons 'heap-area-info         (cdr gc-info)))))

; eof
