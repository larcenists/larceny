; Control facilities.
; 2001-12-11 / lth

; (identity x) => x

(define identity 
  (lambda (x) x))

; (call-with-accumulator
;   (lambda (acc)
;     ... (acc 1) ... (acc 2) ... (acc 3) ...))  => (1 2 3)

(define (call-with-accumulator p)
  (let ((l '()))
    (p (lambda (x)
         (set! l (cons x l))))
    (reverse l)))

; (compose f1 f2 ... fn) =>
;   (lambda (x)
;     (f1 (f2 (... (fn x))))))
;
; ((compose inexact->exact string->number) "77.5") => 155/2

(define (compose . fs)
  (if (null? fs)
      identity
      (let ((f (car fs))
            (r (apply compose (cdr fs))))
        (lambda (x)
          (f (r x))))))

; eof
