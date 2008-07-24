; Copyright 2007 William D Clinger
;
; $Id$
;
; Larceny -- R6RS procedures from (rnrs arithmetic flonums).
; See also Lib/Arch/*/primops.sch and Compiler/common.imp.sch.

($$trace "fl")

; Argument checking.

(define (fl:check! name x)
  (if (not (flonum? x))
      (assertion-violation name "argument not a flonum" x)))

(define (fl:check-args! name args)
  (cond ((null? args) #t)
        ((flonum? (car args))
         (fl:check-args! name (cdr args)))
        (else
         (fl:check! name (car args)))))

(define (flonum-restricted proc name)
  (lambda args
    (fl:check-args! name args)
    (apply proc args)))

(define (flonum-doubly-restricted proc name)
  (lambda args
    (fl:check-args! name args)
    (let ((result (apply proc args)))
      (if (flonum? result)
          result
          +nan.0))))

; flonum? is a primop; see Lib/Arch/*/primops.sch

(define (real->flonum x)
  (assert (real? x))
  (+ x 0.0))

; These can all be slow because important cases will (eventually)
; be handled by Compiler/common.imp.sch

(define fl=? (flonum-restricted = 'fl=?))
(define fl<? (flonum-restricted < 'fl<?))
(define fl>? (flonum-restricted > 'fl>?))
(define fl<=? (flonum-restricted <= 'fl<=?))
(define fl>=? (flonum-restricted >= 'fl>=?))

(define flinteger? (flonum-restricted integer? 'flinteger?))
(define flzero? (flonum-restricted zero? 'flzero?))
(define flpositive? (flonum-restricted positive? 'flpositive?))
(define flnegative? (flonum-restricted negative? 'flnegative?))
(define flodd? (flonum-restricted odd? 'flodd?))
(define fleven? (flonum-restricted even? 'fleven?))
(define flfinite? (flonum-restricted finite? 'flfinite?))
(define flinfinite? (flonum-restricted infinite? 'flinfinite?))
(define flnan? (flonum-restricted nan? 'flnan?))

(define flmax (flonum-restricted max 'flmax))
(define flmin (flonum-restricted min 'flmin))

(define fl+ (flonum-restricted + 'fl+))
(define fl* (flonum-restricted * 'fl*))
(define fl- (flonum-restricted - 'fl-))
(define fl/ (flonum-restricted / 'fl/))

(define flabs (flonum-restricted abs 'flabs))

(define fldiv-and-mod (flonum-restricted div-and-mod 'fldiv-and-mod))
(define fldiv (flonum-restricted div 'fldiv))
(define flmod (flonum-restricted mod 'flmod))
(define fldiv0-and-mod0 (flonum-restricted div0-and-mod0 'fldiv0-and-mod0))
(define fldiv0 (flonum-restricted div0 'fldiv0))
(define flmod0 (flonum-restricted mod0 'flmod0))

; FIXME: The numerator and denominator procedures are
; defined in Lib/Common/ratnums.sch, which isn't loaded
; until later.

(define flnumerator
  (flonum-restricted (lambda (x) (numerator x)) 'flnumerator))

(define fldenominator
  (flonum-restricted (lambda (x) (denominator x)) 'fldenominator))

(define flfloor (flonum-restricted floor 'flfloor))
(define flceiling (flonum-restricted ceiling 'flceiling))
(define fltruncate (flonum-restricted truncate 'fltruncate))
(define flround (flonum-restricted round 'flround))

(define flexp (flonum-restricted exp 'flexp))
(define fllog (flonum-doubly-restricted log 'fllog))
(define flsin (flonum-restricted sin 'flsin))
(define flcos (flonum-restricted cos 'flcos))
(define fltan (flonum-restricted tan 'fltan))
(define flasin (flonum-restricted asin 'flasin))
(define flacos (flonum-restricted acos 'flacos))
(define flatan (flonum-restricted atan 'flatan))

(define flsqrt (flonum-doubly-restricted sqrt 'flsqrt))

(define (flexpt x y)
  (fl:check! 'flexpt x)
  (fl:check! 'flexpt y)
  (cond ((>= x 0.0)
         (expt x y))
        ((not (integer? y))
         +nan.0)
        (else
         (expt x (exact y)))))

; Heaven help us.
; Implementations of the R6RS must implement these conditions,
; even though these conditions can never occur in systems that
; use IEEE arithmetic.
;
; FIXME: not implemented yet

; &no-infinities
; make-no-infinities-violation
; no-infinities-violation
; &no-nans
; make-no-nans-violation
; no-nans-violation

(define (fixnum->flonum n)
  (fx:check! 'fixnum->flonum n)
  (+ n 0.0))
