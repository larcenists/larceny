; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Miscellaneous functions and constants.

;;; System information procedures.

; Returns an identifying string for the implementation and its version.
; Name proposed by Marc Feeley.

(define (scheme-system) 
  (let ((inf (system-features)))
    (string-append "Larceny Version "
		   (number->string (cdr (assq 'larceny-major-version inf)))
		   "."
		   (number->string (cdr (assq 'larceny-minor-version inf))))))

; Compatible with Chez Scheme.

(define most-positive-fixnum
  (let ((mpf (- (expt 2 29) 1)))
    (lambda ()
      mpf)))

; Compatible with Chez Scheme.

(define most-negative-fixnum
  (let ((mnf (expt -2 29)))
    (lambda ()
      mnf)))

;;; Constants

(define *pi* 3.14159265358979323846)

; eof
