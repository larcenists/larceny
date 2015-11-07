;;;; SRFI 114 comparators

#;
(define-syntax define-predefined-comparator
  ;; Define a comparator through a constructor function that caches its result.
  ;; It is to be used to retrieve the predefined comparators.
  ;;
  (syntax-rules ()
    ((_ ?who ?build-form)
     (begin
       (define-syntax ?who
         (identifier-syntax (builder)))
       (define builder
         (let ((C #f))
           (lambda ()
             (or C (receive-and-return (rv)
                       ?build-form
                     (set! C rv))))))
       #| end of BEGIN |# ))
    ))

(define-syntax define-predefined-comparator
  (syntax-rules ()
   ((_ ?who ?build-form)
    (define ?who ?build-form))))

;;; --------------------------------------------------------------------

(define (make-ipair-comparison car-K cdr-K)
  (let ((car-compare (comparator-comparison-procedure car-K))
        (cdr-compare (comparator-comparison-procedure cdr-K)))
    (lambda (a b)
      (let ((result (car-compare (icar a) (icar b))))
        (if (zero? result)
            (cdr-compare (icdr a) (icdr b))
          result)))))

;;; --------------------------------------------------------------------

(define (make-ipair-comparator car-K cdr-K)
  (define car-test-proc
    (comparator-type-test-procedure car-K))
  (define cdr-test-proc
    (comparator-type-test-procedure cdr-K))
  (define (test-proc obj)
    (and (ipair? obj)
         (car-test-proc (icar obj))
         (cdr-test-proc (icdr obj))))
  (make-comparator test-proc
                   #t
                   (make-ipair-comparison car-K cdr-K)
                   (make-ipair-hash       car-K cdr-K)))

(define (make-ipair-hash car-K cdr-K)
  (let ((car-hash (comparator-hash-function car-K))
        (cdr-hash (comparator-hash-function cdr-K)))
    (lambda (obj)
      (+ (car-hash (icar obj))
         (cdr-hash (icdr obj))))))

(define ipair-comparator
  (make-ipair-comparator default-comparator default-comparator))

;;; --------------------------------------------------------------------

(define (make-ilist-comparator K)
  (define element-test-proc
    (comparator-type-test-procedure K))
  (define (test-proc obj)
    (if (ipair? obj)
        (and (element-test-proc (icar obj))
             (test-proc (icdr obj)))
      (null? obj)))
  (make-listwise-comparator test-proc K null? icar icdr))

(define ilist-comparator
  (make-ilist-comparator default-comparator))

;;; --------------------------------------------------------------------

(define (make-icar-comparator K)
  (define icar-test-proc
    (comparator-type-test-procedure K))
  (define (test-proc obj)
    (and (ipair? obj)
         (icar-test-proc (icar obj))))
  (make-comparator test-proc
                   #t
                   (let ((compare (comparator-comparison-procedure K)))
                     (lambda (a b)
                       (compare (icar a) (icar b))))
                   (let ((hash (comparator-hash-function K)))
                     (lambda (obj)
                       (hash (icar obj))))))

(define (make-icdr-comparator K)
  (define icdr-test-proc
    (comparator-type-test-procedure K))
  (define (test-proc obj)
    (and (ipair? obj)
         (icdr-test-proc (icdr obj))))
  (make-comparator test-proc
                   #t
                   (let ((compare (comparator-comparison-procedure K)))
                     (lambda (a b)
                       (compare (icdr a) (icdr b))))
                   (let ((hash (comparator-hash-function K)))
                     (lambda (obj)
                       (hash (icdr obj))))))

;;; --------------------------------------------------------------------

(define (make-improper-ilist-comparator K)
  (make-comparator #t
                   #t
                   (make-improper-ilist-comparison K)
                   (make-improper-ilist-hash       K)))

(define (make-improper-ilist-comparison K)
  (let ((pair-compare (make-ipair-comparison K K))
        (item-compare (comparator-comparison-procedure K)))
    (lambda (a b)
      ;;A.TYPE and B.TYPE are the indexes of the object types.
      (let* ((a.type (improper-list-type a))
             (b.type (improper-list-type b))
             (result (real-comparison a.type b.type)))
        (if (zero? result)
            ;;A and B have the same type index; they are: both pairs, both nulls,
            ;;both some other object.
            (cond ((ipair? a)
                   (pair-compare a b))
                  ((null? a)
                   0)
                  (else
                   (item-compare a b)))
          result)))))

(define (improper-list-type obj)
  ;;Compute type index for inexact list comparisons.
  ;;
  (cond ((null? obj)        0)
        ((ipair? obj)        1)
        (else                2)))

(define (real-comparison a b)
  ;;Comparison procedure for real numbers only.
  ;;
  (cond ((< a b)        -1)
        ((> a b)        +1)
        (else        0)))

(define (make-improper-ilist-hash K)
  (let ((hash (comparator-hash-function K)))
    (lambda (obj)
      (cond ((ipair? obj)
             (+ (hash (icar obj))
                (hash (icdr obj))))
            ((null? obj)
             0)
            (else
             (hash obj))))))

; eof
