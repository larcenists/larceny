; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- system parameter abstraction.

($$trace "sysparam")

; System parameters are defined in the Library using this procedure.

(define (make-parameter name value . rest)
  (let ((ok? (if (null? rest) 
                 (lambda (x) #t) 
                 (car rest))))
    (lambda args
;; No need to protect this!
;      (call-without-interrupts
;        (lambda ()
          (if (pair? args)
              (if (null? (cdr args))
                  (let ((new-value (car args)))
                    (if (ok? new-value)
                        (begin (set! value new-value)
                               value)
                        (begin (error name ": Invalid value " (car args))
                               #t)))
                  (begin (error name ": too many arguments.")
                         #t))
              value)
;             ))
          )))

; Returns an assoc list of system information.

(define (system-features)
  (let* ((wordsize
	  (if (fixnum? (expt 2 32)) 64 32))
	 (char-bits
	  (* (bytevector-like-length (string #\a #\b #\c #\d)) 2))
	 (char-repr
	  (case char-bits
	    ((8)  'iso-latin-1)		; iso 8859/1
	    ((16) 'ucs2)		; 2-byte unicode
	    ((32) 'ucs4)		; 4-byte unicode
	    (else 'unknown)))
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
	  (cons 'flonum-bits            64)
	  (cons 'flonum-representation  'ieee)
	  (cons 'gc-technology          (car gc-info))
	  (cons 'heap-area-info         (cdr gc-info)))))

; eof
