;;; If (rnrs arithmetic flonums) is unavailable, these definitions are used.

;;; Private.

(define (flop0-or-more name op)
  (lambda args
    (for-each (lambda (x) (check-flonum! name x)) args)
    (flonum (apply op args))))

(define (flop1-or-more name op)
  (lambda (x . args)
    (for-each (lambda (x) (check-flonum! name x)) (cons x args))
    (flonum (apply op x args))))

(define (flop2-or-more name op)
  (lambda (x y . args)
    (for-each (lambda (x) (check-flonum! name x)) (cons x (cons y args)))
    (flonum (apply op x y args))))

(define (flpred1 name op)
  (lambda (x)
    (check-flonum! name x)
    (op x)))

(define (flpred2-or-more name op)
  (lambda (x y . args)
    (for-each (lambda (x) (check-flonum! name x)) (cons x (cons y args)))
    (apply op x y args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exported.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; R6RS 11.7.4.1 says
;;;
;;;     If z is a complex number object, then (real? z) is true if
;;;     and only if (zero? (imag-part z)) and (exact? (imag-part z))
;;;     are both true.
;;;
;;; As explained in R6RS Rationale 11.6.6, some such rule is needed
;;; so the flonum and compnum representation types will be closed
;;; under operations that would be expected to return a flonum or
;;; compnum, respectively.  See especially the last two paragraphs
;;; of 11.6.6.2.
;;;
;;; FIXME: Unfortunately, some implementations of the R7RS have
;;; defined (imag-part x) to be inexact whenever x is an inexact
;;; real.

(define (flonum? x)
  (and (number? x)
       (real? x)        ; implies (exact? (imag-part x))
       (inexact? x)
#;     (exact? (imag-part x))))

(define fl=?  (flpred2-or-more 'fl=? =))
(define fl<?  (flpred2-or-more 'fl<? <))
(define fl>?  (flpred2-or-more 'fl>? >))
(define fl<=? (flpred2-or-more 'fl<=? <=))
(define fl>=? (flpred2-or-more 'fl>=? >=))

(define flinteger?  (flpred1 'flinteger?  integer?))
(define flzero?     (flpred1 'flzero?     zero?))
(define flpositive? (flpred1 'flpositive? positive?))
(define flnegative? (flpred1 'flnegative? negative?))
(define flodd?      (flpred1 'flodd?      odd?))
(define fleven?     (flpred1 'fleven?     even?))
(define flfinite?   (flpred1 'flfinite?   finite?))
(define flinfinite? (flpred1 'flinfinite? infinite?))
(define flnan?      (flpred1 'flnan?      nan?))

(define fl+        (flop0-or-more 'fl+ +))
(define fl*        (flop0-or-more 'fl* *))
(define fl-        (flop1-or-more 'fl- -))
(define fl/        (flop1-or-more 'fl/ /))

(define flabs      (flop1 'flabs      abs))

(define flfloor    (flop1 'flfloor    floor))
(define flceiling  (flop1 'flceiling  ceiling))
(define flround    (flop1 'flround    round))
(define fltruncate (flop1 'fltruncate truncate))

(define r6rs:flnumerator   numerator)
(define r6rs:fldenominator denominator)

(define flexp      (flop1 'flexp  exp))
(define flsqrt     (flop1 'flsqrt sqrt))
(define flexpt     (flop2 'flexpt expt))
(define fllog      (flop1 'fllog  log))
(define flsin      (flop1 'flsin  sin))
(define flcos      (flop1 'flcos  cos))
(define fltan      (flop1 'fltan tan))
(define flasin     (flop1 'flasin asin))
(define flacos     (flop1 'flacos acos))
(define flatan     (flop1-or-more 'flatan atan)) ; FIXME 1 or 2 arguments
