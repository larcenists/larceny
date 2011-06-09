;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Queue-like object lifetimes.
;
; (pueue-benchmark m n k p) allocates m lists of length n,
; storing them in a circular buffer that holds k of the lists,
; with p popular objects.
;
; The lists are build out of two-element vectors, which
; Larceny represents with a header word and one word of
; padding.  That representation allows fairer comparisons
; with Java.

(define default-m 1000)
(define default-n 1000000)
(define default-k 10)
(define default-p 1)

(define (pueue-benchmark . rest)
  (let ((m (if (null? rest) default-m (car rest)))
        (n (if (or (null? rest) (null? (cdr rest)))
               default-n
               (cadr rest)))
        (k (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
               default-k
               (caddr rest)))
        (p (if (or (null? rest)
                   (null? (cdr rest))
                   (null? (cddr rest))
                   (null? (cdddr rest)))
               default-p
               (cadddr rest))))
    (run-benchmark (string-append "pueue"
                                  (number->string m)
                                  ":"
                                  (number->string n)
                                  ":"
                                  (number->string k)
                                  ":"
                                  (number->string p))
                   1
                   (lambda () (pueue-test m n k p))
                   (lambda (x) #t))))

;;; Note that this version fixes a bug in pueue3.sch:
;;; This version really does create p popular objects,
;;; whereas pueue3 created only 1 extremely popular object.

(define (pueue-test m n k p)
  (let ((q (make-vector k #f))
        (xs (make-vector p #f)))
    (define (loop m i)
      (cond ((fx=? m 0)
             (mylength (vector-ref q 0)))
            ((fx=? i k)
             (loop m 0))
            (else
             (let* ((x (vector-ref xs (remainder m p))))
	       (vector-set! q i (make-list n x)))
	     (loop (fx- m 1) (fx+ i 1)))))
    (define (make-list n x)
      (do ((n n (fx- n 1))
           (r #f (vector x r)))
          ((fx=? n 0) r)))
    (define (mylength v)
      (do ((v v (vector-ref v 1))
           (r 0 (fx+ r 1)))
          ((not (vector? v))
           r)))
    (do ((i 0 (+ i 1)))
        ((= i p))
      (vector-set! xs i (list i)))
    (loop m 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
