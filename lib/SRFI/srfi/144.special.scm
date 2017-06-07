
;;; FIXME: probably inaccurate for large negative values

(define (flgamma x)
  (cond ((fl>=? x 200.0)    ; FIXME: appropriate for double precision
         +inf.0)
        ((fl>=? x 2.0)
         (let ((x (fl- x 1.0)))
           (fl* x (flgamma x))))
        ((fl=? x 1.0)
         1.0)
        ((and (fl<=? x 0.0)
              (flinteger? x))    ; pole error
         +nan.0)
        (else
         (fl/ (polynomial-at x gamma-coefs)))))

;;; Series expansion for 1/Gamma(x), from Abramowitz and Stegun 6.1.34

(define gamma-coefs
  '(0.0
    1.0
    +0.5772156649015329
    -0.6558780715202538
    -0.0420026350340952
    +0.1665386113822915 ; x^5
    -0.0421977345555443
    -0.0096219715278770
    +0.0072189432466630
    -0.0011651675918591
    -0.0002152416741149 ; x^10
    +0.0001280502823882
    -0.0000201348547807
    -0.0000012504934821
    +0.0000011330272320
    -0.0000002056338417 ; x^15
    +0.0000000061160950
    +0.0000000050020075
    -0.0000000011812746
    +0.0000000001043427
    +0.0000000000077823 ; x^20
    -0.0000000000036968
    +0.0000000000005100
    -0.0000000000000206
    -0.0000000000000054
    +0.0000000000000014 ; x^25
    +0.0000000000000001
    ))


(define flloggamma FIXME)
(define flfirst-bessel FIXME)
(define flsecond-bessel FIXME)
(define flerf FIXME)
(define flerfc FIXME)

