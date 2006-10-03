; Test code for lib/fqueue.sch
; 2000-11-19 / lth

(require 'fqueue)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (fqueue? (make-fqueue))
    (fail 'fqueue:1))

(or (not (fqueue? ((record-constructor (make-record-type "test" '(dummy))) 1)))
    (fail 'fqueue:2))

(or (fqueue-empty? (make-fqueue))
    (fail 'fqueue:3))

(or (not (fqueue-empty? (fqueue-put (make-fqueue) 1)))
    (fail 'fqueue:4))

(or (let ((d '(1 2 3 4)))
      (let ((q (do ((d d (cdr d))
                    (q (make-fqueue) (fqueue-put q (car d))))
                   ((null? d)
                    q))))
        (let loop ((q q) (d d))
          (if (null? d)
              (fqueue-empty? q)
              (let-values (((x q) (fqueue-get q)))
                (if (equal? x (car d))
                    (loop q (cdr d))
                    (begin (display (list x (car d)))
                           #f)))))))
    (fail 'fqueue:5))

(or (let ((d '(1 2 3 4)))
      (let loop ((q (make-fqueue)) (d d))
        (if (null? d)
            #t
            (let ((q1 (fqueue-put q (car d))))
              (let-values (((x q2) (fqueue-get q1)))
                (if (equal? (car d) x)
                    (loop q2 (cdr d))
                    #f))))))
    (fail 'fqueue:6))

(or (let ((d '(1 2 3 4)))
      (let ((q (do ((d d (cdr d))
                    (q (make-fqueue) (fqueue-put q (car d))))
                   ((null? d) q))))
        (and (equal? d (fqueue->list q))
             (equal? d (fqueue->list q)))))
    (fail 'fqueue:7))

(or (let ((d '(1 2 3 4)))
      (equal? d (fqueue->list (list->fqueue '(1 2 3 4)))))
    (fail 'fqueue:8))

(or (let ((d '(1 2 3 4)))
      (let ((q1 (list->fqueue d)))
        (let ((q2 (list->fqueue (reverse d) q1)))
          (equal? (append d (reverse d)) (fqueue->list q2)))))
    (fail 'fqueue:9))
      
; eof

