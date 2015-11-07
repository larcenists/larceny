;;; Circular objects must be detected/processed by the R7RS macro expander
;;; and by the R7RS write procedure.
;;;
;;; $Id$
;;;

($$trace "circular")

;;; Is the given object circular?
;;;
;;; First see if a depth-first traversal completes in bounded time.
;;; If not, perform a more expensive traversal that keeps track of
;;; all possibly circular objects in scope.
;;;
;;; See implementation of the equal? procedure for a more complicated
;;; example of this technique.

(define circularity:bound-on-recursion 100000)

(define (object-is-circular? x)

  ; Fast traversal with bounded recursion.
  ; Returns an exact integer n.
  ; If n > 0, then x is not circular and the traversal performed
  ; bound - n recursive calls.
  ; If n <= 0, then the bound was exceeded before the traversal
  ; could determine whether x is circular.

  (define (small? x bound)
    (cond ((<= bound 0)
           bound)
          ((pair? x)
           (let ((result (small? (car x) (- bound 1))))
             (if (> result 0)
                 (small? (cdr x) result)
                 result)))
          ((vector? x)
           (let ((nx (vector-length x)))
             (let loop ((i 0)
                        (bound (- bound 1)))
               (if (< i nx)
                   (let ((result (small? (vector-ref x i) bound)))
                     (if (> result 0)
                         (loop (+ i 1) result)
                         result))
                   bound))))
          (else bound)))

  ; Returns #t iff x contains circular structure or contains
  ; any of the objects present within the given hashtable.

  (define (circular? x table)
    (cond ((hashtable-contains? table x)
           #t)
          ((pair? x)
           (hashtable-set! table x #t)
           (cond ((circular? (car x) table)
                  #t)
                 ((circular? (cdr x) table)
                  #t)
                 (else
                  (hashtable-delete! table x)
                  #f)))
          ((vector? x)
           (hashtable-set! table x #t)
           (let ((nx (vector-length x)))
             (let loop ((i 0))
               (if (< i nx)
                   (if (circular? (vector-ref x i) table)
                       #t
                       (loop (+ i 1)))
                   (begin (hashtable-delete! table x)
                          #f)))))
          (else #f)))

  (cond ((< 0 (small? x circularity:bound-on-recursion))
         #f)
        (else
         (circular? x (make-eq-hashtable)))))

;;; Given a unary procedure f and an object x that may be circular
;;; or contained shared substructure, calls f exactly once on every
;;; leaf (non-pair and non-vector) of x.

(define (larceny:object-map f x)

  ;; The hashtable maps pairs and vectors of x to the corresponding
  ;; pair or vector of the result.

  (define table (make-eq-hashtable))

  (define (traverse x)
    (cond ((hashtable-contains? table x)
           (hashtable-ref table x #f))
          ((pair? x)
           (let ((y (cons (unspecified) (unspecified))))
             (hashtable-set! table x y)
             (set-car! y (traverse (car x)))
             (set-cdr! y (traverse (cdr x)))
             y))
          ((vector? x)
           (let* ((nx (vector-length x))
                  (y (make-vector nx (unspecified))))
             (hashtable-set! table x y)
             (let loop ((i 0))
               (if (< i nx)
                   (begin (vector-set! y i (traverse (vector-ref x i)))
                          (loop (+ i 1)))
                   y))))
          (else (f x))))

  (traverse x))
