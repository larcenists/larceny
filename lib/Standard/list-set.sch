; List-as-sets.
; 2000-05-18 / lth
;
; Uses eqv? to test for equality throughout.
;
; FIXME: Implementation is not particularly efficient (started as 
; an exercise in using reduction/fold operators).

; Invariant: a set has no duplicated elements.

(define (adjoin x set)
  (if (memv x set)
      set
      (cons x set)))

(define (union . sets)
  (reduce (lambda (s1 s2)
            (fold-left (lambda (s x)
                         (if (memv x s) s (cons x s)))
                       s1
                       s2))
          '() sets))

(define (intersection . sets)
  (reduce (lambda (s1 s2)
            (fold-left (lambda (s x)
                         (if (memv x s1) (cons x s) s))
                       '()
                       s2))
          '() sets))

(define (difference s1 s2)
  (fold-left (lambda (s x)
               (if (memv x s2) s (cons x s)))
             '()
             s1))

(define (set<=? s t . sets)             ; Improper subset
  (do ((s1  (cons s (cons t sets)) (cdr s1))
       (s2  (cons t sets)          (cdr s2))
       (ok? #t                     (let ((lhs (car s1))
                                         (rhs (car s2)))
                                     (every? (lambda (x) (memv x rhs)) lhs))))
      ((or (not ok?) (null? s2))
       (if ok? #t #f))))

(define (set=? s t . sets)
  (let ((sets (cons s (cons t sets))))
    (null? (difference (apply union sets)
                       (apply intersection sets)))))

; eof
