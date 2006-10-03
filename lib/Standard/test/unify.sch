; Test code for lib/unify.sch
; 2000-05-25 / lth

; Not finished; what's here works.

(require 'unify)
(require 'box)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

; True if every var in vars is named in substitution with an undefined value.

(define (undefined-in? vars substitution)
  (every? (lambda (v) 
            (let ((probe (assoc v substitution)))
              (and probe
                   (pair? (cdr probe))
                   (undef? (cadr probe)))))
          vars))

; True if x and y are named in substitution and are either both undefined
; and unified or both have the same (non-undefined) value.

(define (same-in? x y substitution)
  (let ((px (assoc x substitution))
        (py (assoc y substitution)))
    (and px
         py
         (or (and (pair? (cdr px))
                  (undef? (cadr px))
                  (pair? (cdr py))
                  (undef? (cadr py))
                  (equal? (cddr px) (cddr py)))
             (equal? (cdr px) (cdr py))))))

(define unify)
(define undef?)

; Variables are boxes.  The content defines the box, by EQUAL?.

(let-values (((unifier undef-pred) (make-unifier box? equal? #t)))
  (set! unify unifier)
  (set! undef? undef-pred))

(or (equal? (unify 'a 'a) '())
    (fail 'unify-data:1))
(or (equal? (unify "a" "a") '())
    (fail 'unify-data:2))
(or (equal? (unify '() '()) '())
    (fail 'unify-data:3))
(or (equal? (unify car car) '())
    (fail 'unify-data:4))
(or (equal? (unify 10 10) '())
    (fail 'unify-data:5))
(or (equal? (unify #\a #\a) '())
    (fail 'unify-data:6))
(or (equal? (unify #t #t) '())
    (fail 'unify-data:7))
(or (equal? (unify `(a (b #(c d e)) #f) `(a (b #(c d e)) #f)) '())
    (fail 'unify-data:8))
(or (equal? (unify 'a '(quote a)) '())
    (fail 'unify-data:9))
(or (equal? (unify '(quote a) 'a) '())
    (fail 'unify-data:10))
(or (let ((b (box 37)))                 ; Looks like a var but is quoted
      (equal? (unify `(quote ,b) `(quote ,b)) '()))
    (fail 'unify-data:11))

(or (not (unify 'a 'b))
    (fail 'unify-data-not:1))
(or (not (unify "a" "b"))
    (fail 'unify-data-not:2))
(or (not (unify '() #f))
    (fail 'unify-data-not:3))
(or (not (unify car cdr))
    (fail 'unify-data-not:4))
(or (not (unify 10 11))
    (fail 'unify-data-not:5))
(or (not (unify #\a #\b))
    (fail 'unify-data-not:6))
(or (not (unify #t #f))
    (fail 'unify-data-not:7))
(or (not (unify `(a (b #(c x e)) #f) `(a (b #(c d e)) #f)))
    (fail 'unify-data-not:8))
(or (let ((a (box 36))
          (b (box 37)))                 ; Looks like a var but is quoted
      (not (unify `(quote ,a) `(quote ,b))))
    (fail 'unify-data-not:9))

(or (undefined-in? '(#&a) (unify '#&a '#&a))
    (fail 'unify-var:1))
(or (let ((s (unify '#&a '#&b)))
      (undefined-in? '(#&a #&b) s)
      (same-in? '#&a '#&b s))
    (fail 'unify-var:2))


; Some tests (old, from the program)

(define (test-unify)
  (let-values (((unify undefined?)
                (make-unifier (lambda (x)
                                (and (pair? x)
                                     (eq? (car x) '?)))
                              equal?
                              #t)))
    (display (unify '(+ (? a) (? a)) '(+ 3 3)))
    (newline)
    (display (unify '((? a) (? a) (? c)) '(a (? b) c))) 
    (newline)
    (display (unify '((? a) (? b) (? a) (? b)) '((? e) (? e) (? e) (? f))))
    (newline)
    (display (unify '((? a) (? b) (? c)) '((? c) (? d) (? b))))
    (newline)
    (display (unify '((? a) (? x)) '((a b (? x)) (? a))))
    (newline)
    ))

