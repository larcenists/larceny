; Useful procedures for manipulating numbers.
; 2000-06-26 / lth

(require 'string)

; (if (negative? x) (sign x)) => -1
; (if (positive? x) (sign x)) => 1
; (if (zero? x) (sign x)) => 0

(define (sign x)                        ; Note, unspecified for NaN, complex
  (cond ((< x 0) -1)
        ((= x 0) 0)
        ((> x 0) 1)))

; This is crude, but it does the job for now.

(define (flonum->string f decimals)
  (cond ((not (rational? f))
         (error "flonum->string: not rational: " f))
        ((not (and (exact? decimals) 
                   (integer? decimals) 
                   (<= 0 decimals 20)))
         (error "flonum->string: invalid number of decimal places: " 
                decimals))
        ((not (flonum? f))
         (flonum->string (exact->inexact f) decimals))
        ((zero? decimals)
         (string-append (number->string (inexact->exact (round f))) "."))
        ((or (= f +inf.0) (= f -inf.0) (not (= f f)))
         (number->string f))
        (else
         (let* ((k (expt 10 decimals))
                (x (number->string (/ (round (* f k)) k))))
           (let ((dot (- (string-length x) 1 (string-index x #\.))))
             (if (negative? (- decimals dot))
                 x
                 (string-append x (make-string (- decimals dot) #\0))))))))

; eof
