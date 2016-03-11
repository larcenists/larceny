;;; Olin's test harness tests some procedures that aren't part of SRFI 132,
;;; so (srfi 132 plus) is here just to support Olin's tests.

(define-library (srfi 132 plus)

  (export list-sorted? vector-sorted?
          list-merge-sort  vector-merge-sort               ; not part of SRFI 132
          list-merge-sort! vector-merge-sort!              ; not part of SRFI 132
          vector-insert-sort vector-insert-sort!           ; not part of SRFI 132
          vector-heap-sort   vector-heap-sort!             ; not part of SRFI 132
          vector-quick-sort  vector-quick-sort!            ; not part of SRFI 132
;         vector-binary-search vector-binary-search3       ; not part of SRFI 132
          vector-quick-sort3 vector-quick-sort3!           ; not part of SRFI 132
          list-merge vector-merge
          list-sort vector-sort
          list-stable-sort vector-stable-sort
          list-merge! vector-merge!
          list-sort! vector-sort!
          list-stable-sort! vector-stable-sort!
          list-delete-neighbor-dups vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!
          vector-find-median vector-find-median!           ; not part of ref impl
          vector-select!                                   ; not part of ref impl
          )

  (import (except (scheme base) vector-copy vector-copy!)
          (rename (only (scheme base) vector-copy vector-copy!)
                  (vector-copy  r7rs-vector-copy)
                  (vector-copy! r7rs-vector-copy!))
          (scheme cxr)
          (scheme write) ; FIXME
          (only (rnrs base) assert)
          (only (srfi 27) random-integer))

  (include "132/delndups.scm")
  (include "132/lmsort.scm")
  (include "132/sortp.scm")
  (include "132/vector-util.scm")
  (include "132/vhsort.scm")
  (include "132/visort.scm")
  (include "132/vmsort.scm")
  (include "132/vqsort2.scm")
  (include "132/vqsort3.scm")
  (include "132/sort.scm") ; must be last

  ;; Added for Larceny.

  (include "132/select.scm")

  ;; These procedures are missing from the reference implementation.

  (begin

   ;; Although the median can be found in linear time, SRFI 132
   ;; is written as though the median must be found by sorting.
   ;; Well, fie on that.

   (define (vector-find-median < v knil . rest)
     (let* ((mean (if (null? rest)
                      (lambda (a b) (/ (+ a b) 2))
                      (car rest)))
            (n (vector-length v)))
       (cond ((zero? n)
              knil)
             ((odd? n)
              (%vector-select < v (quotient n 2) 0 n))
             (else
              (call-with-values
               (lambda () (%vector-select2 < v (- (quotient n 2) 1) 0 n))
               (lambda (a b)
                 (mean a b)))))))

   ;; For this procedure, however, the SRFI 132 specification
   ;; demands the vector be sorted (by side effect).

   (define (vector-find-median! < v knil . rest)
     (let* ((mean (if (null? rest)
                      (lambda (a b) (/ (+ a b) 2))
                      (car rest)))
            (n (vector-length v)))
       (vector-sort! < v)
       (cond ((zero? n)
              knil)
             ((odd? n)
              (vector-ref v (quotient n 2)))
             (else
              (mean (vector-ref v (- (quotient n 2) 1))
                    (vector-ref v (quotient n 2)))))))

   ;; This could be made slightly more efficient, but who cares?

   (define (vector-select! < v k . rest)
     (let* ((start (if (null? rest)
                       0
                       (car rest)))
            (end (if (and (pair? rest)
                          (pair? (cdr rest)))
                     (car (cdr rest))
                     (vector-length v))))
       (vector-sort! < v start end)))

   )

  )

(define-library (srfi 132)

  (export list-sorted? vector-sorted?
          list-merge vector-merge
          list-sort vector-sort
          list-stable-sort vector-stable-sort
          list-merge! vector-merge!
          list-sort! vector-sort!
          list-stable-sort! vector-stable-sort!
          list-delete-neighbor-dups vector-delete-neighbor-dups
          list-delete-neighbor-dups! vector-delete-neighbor-dups!
          vector-find-median vector-find-median!
          vector-select!
          )

  (import (srfi 132 plus)))
