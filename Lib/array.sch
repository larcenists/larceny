(load "import.sch")
 
(define make-array
  (lambda (n . rest)
    (let ((v (make-vector n)))
      (if (null? rest)
          v
          (do ((i (1- n) (1- i)))
              ((<? i 0) v)
              (vector-set! v i (apply make-array rest)))))))
 
(define array-ref
  (lambda (v i . subscripts)
    (if (null? subscripts)
        (vector-ref v i)
        (apply array-ref (cons (vector-ref v i) subscripts)))))

(define-inline array-ref
  (lambda (l f)
    (cond ((f 'vector-ref) l)
          ((= (length l) 3)
           (cons 'vector-ref (cdr l)))
          ((> (length l) 3)
           (cons 'array-ref
                 (cons (list 'vector-ref (cadr l) (caddr l))
                       (cdddr l))))
          (else l))))
 
(define array-set!
  (lambda (v i x . rest)
    (if (null? rest)
        (vector-set! v i x)
        (apply array-set! (cons (vector-ref v i) (cons x rest))))))

(define-inline array-set!
  (lambda (l f)
    (cond ((some? f '(vector-ref vector-set!)) l)
          ((= (length l) 4)
           (cons 'vector-set! (cdr l)))
          ((> (length l) 4)
           (cons 'array-set!
                 (cons (list 'vector-ref (cadr l) (caddr l))
                       (cdddr l))))
          (else l))))
 
(import '(make-array array-ref array-set!))
