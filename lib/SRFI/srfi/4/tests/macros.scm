;;;; Macros for SRFI-4 and extended SRFI-4 implementation
;;; Hvector = homogeneous vector

;; The guts of a make-Hvector without a fill.
;; Requires an Hvector type tag and a multiplier for the size
;; that says how many bytes are needed to represent an element.

(define-syntax make
  (syntax-rules ()
    ((make tag mult size)
     (make-multivector tag (make-bytevector (* mult size))))))

;; The guts of a routine to fill a vector with a single value.
;; Requires Hvector-set! and Hvector-length

(define-syntax fill!
  (syntax-rules ()
    ((fill! vlen vset! vec value)
     (let loop ((i 0))
       (unless (= i (vlen vec))
         (vset! vec i value)
         (loop (+ i 1)))))))

;; Combined make and fill! macro.
;; Requires everything that each submacro requires

(define-syntax make+fill!
  (syntax-rules ()
    ((make+fill! tag mult vlen vset size maybe-value)
     (let ((vec (make tag mult size)))
       (if (not (null? maybe-value))
         (fill! vlen vset vec (car maybe-value)))
       vec))))

;; The guts of an Hvector->list converter.
;; Requires Hvector-length and Hvector-ref

(define-syntax ->list
  (syntax-rules ()
    ((->list vref vlength vec)
     (let loop ((i (- (vlength vec) 1)) (r '()))
       (if (< i 0)
         r
         (loop (- i 1) (cons (vref vec i) r)))))))

;; The guts of a list->Hvector converter.
;; Requires make-Hvector and Hvector-set!

(define-syntax list->
  (syntax-rules ()
    ((list-> vmake vset! list)
     (let* ((len (length list))
            (vec (vmake len)))
       (let loop ((i 0) (list list))
         (unless (= i len)
           (vset! vec i (car list))
           (loop (+ i 1) (cdr list)))
         vec)))))

