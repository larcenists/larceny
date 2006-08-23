(define (run-benchmark name count run ok?)
  (if (not (ok? (run-bench name count run ok?)))
    (begin
      (display "*** wrong result ***")
      (newline))))

(define (run-bench name count run ok?)
  (let loop ((i 0) (result (list 'undefined)))
    (if (< i count)
      (loop (+ i 1) (run))
      result)))

(define (fatal-error . args)
  (for-each display args)
  (newline)
  (exit 1))

;FLOATvector-const
(define FLOATvector? vector?)
(define FLOATvector vector)
(define FLOATmake-vector make-vector)
(define FLOATvector-ref vector-ref)
(define FLOATvector-set! vector-set!)
(define FLOATvector-length vector-length)
;nuc-const

(define FLOAT+ +)
(define FLOAT- -)
(define FLOAT* *)
(define FLOAT/ /)

(define FLOAT= =)
(define FLOAT< <)
(define FLOAT<= <=)
(define FLOAT> >)
(define FLOAT>= >=)

(define FLOATnegative? negative?)
(define FLOATpositive? positive?)
(define FLOATzero? zero?)

(define FLOATabs abs)
(define FLOATsin sin)
(define FLOATcos cos)
(define FLOATatan atan)
(define FLOATsqrt sqrt)
(define FLOATmin min)
(define FLOATmax max)
(define FLOATround round)
(define FLOATinexact->exact inexact->exact)

(define bitwise-or logior)
(define bitwise-and logand)
(define bitwise-not lognot)

;;; For scheme.scm
(define (DENOMINATOR . args)
  (fatal-error "IMAG-PART is not implemented"))
(define (NUMERATOR . args)
  (fatal-error "NUMERATOR is not implemented"))
(define (RATIONALIZE . args)
  (fatal-error "RATIONALIZE is not implemented"))
