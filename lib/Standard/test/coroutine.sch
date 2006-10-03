; Test code for lib/coroutine.sch
; 2000-05-21 / lth

(require 'coroutine)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (let ((result '()))
      (let ((x (call-with-current-continuation
                (lambda (c0)
                  (letrec ((c1 (make-coroutine 
                                (lambda (resume value)
                                  (set! result (cons (list "C1" value) result))
                                  (resume c2 (* value 2)))))
                           (c2 (make-coroutine
                                (lambda (resume value)
                                  (set! result (cons (list "C2" value) result))
                                  (resume c0 (* value 2))))))
                    (c1 37))))))
        (set! result (cons (list "C0" x) result)))
      (equal? (reverse result) '(("C1" 37) ("C2" 74) ("C0" 148))))
    (fail 'coroutine:1))

(or (let ((result '()))
      (letrec ((c0 '*)
               (c1 (make-coroutine 
                    (lambda (resume value)
                      (let loop ((value value))
                        (set! result (cons (list "C1" value) result))
                        (loop (resume c2 (* value 2)))))))
               (c2 (make-coroutine
                    (lambda (resume value)
                      (let loop ((value value) (c0? #t))
                        (set! result (cons (list "C2" value) result))
                        (loop (resume (if c0? c0 c1) (* value 2)) 
                              (not c0?)))))))
        (let ((x (call-with-current-continuation
                  (lambda (k)
                    (set! c0 k)
                    (c1 37)))))
          (set! result (cons (list "C0" x) result)))
        (let ((x (call-with-current-continuation
                  (lambda (k)
                    (set! c0 k)
                    (c2 12)))))
          (set! result (cons (list "C0" x) result))))
      (equal? (reverse result)
              '(("C1" 37) ("C2" 74) ("C0" 148) ("C2" 12) ("C1" 24) ("C2" 48) 
                          ("C0" 96))))
    (fail 'coroutine:2))

(define (samefringe? a b)

  (define (leaf? x) (not (pair? x)))
  (define (value x) x)
  (define (left x) (car x))
  (define (right x) (cadr x))

  (call-with-current-continuation
   (lambda (return)
     (letrec ((done (list 'done))
              (af   (make-coroutine
                     (lambda (resume v)
                       (define (trav t)
                         (if (leaf? t)
                             (resume bf (value t))
                             (begin (trav (left t))
                                    (trav (right t)))))
                       (trav a)
                       (resume bf done))))
              (bf   (make-coroutine
                     (lambda (resume v)
                       (define (trav t)
                         (if (leaf? t)
                             (if (not (equal? (value t) (resume af #t)))
                                 (return #f))
                             (begin (trav (left t))
                                    (trav (right t)))))
                       (trav b)
                       (return (eq? (resume af #t) done))))))
       (bf '*)))))

(or (samefringe? '((a a) a) '(a (a a)))
    (fail 'coroutine:3))

(or (not (samefringe? '((a a) a) '((a (a b)) (a a))))
    (fail 'coroutine:4))

; eof
