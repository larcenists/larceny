; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny library -- system parameter abstraction.

($$trace "sysparam")

; System parameters are defined in the Library using this procedure.

(define (system-parameter name value . rest)
  (let ((ok? (if (null? rest) (lambda (x) #t) (car rest))))
    (lambda rest
      (cond ((null? rest)
	     value)
	    ((null? (cdr rest))
	     (if (ok? (car rest))
		 (begin (set! value (car rest))
			value)
		 (begin (error name ": Invalid value " (car rest))
			#t)))
	    (else
	     (error name ": too many arguments.")
	     #t)))))

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
	    ((32) 'usc4)		; 4-byte unicode
	    (else 'unknown)))
	 (gc-info
	  (sys$system-feature 'gc-tech)))
    (list (cons 'larceny-major-version  (sys$system-feature 'larceny-major))
	  (cons 'larceny-minor-version  (sys$system-feature 'larceny-minor))
	  (cons 'architecture-name      (sys$system-feature 'arch-name))
	  (cons 'architecture-endian    (sys$system-feature 'endian))
	  (cons 'operating-system-name  (sys$system-feature 'os-name))
	  (cons 'os-major-version       (sys$system-feature 'os-major))
	  (cons 'os-minor-version       (sys$system-feature 'os-minor))
	  (cons 'architecture-word-size wordsize)
	  (cons 'fixnum-bits            (- wordsize 2))
	  (cons 'fixnum-representation  'twos-complement)
	  (cons 'char-bits              char-bits)
	  (cons 'char-representation    char-repr)
	  (cons 'flonum-bits            64)
	  (cons 'flonum-representation  'IEEE)
	  (cons 'gc-technology          (car gc-info))
	  (cons 'heap-area-info         (cdr gc-info)))))

; eof
