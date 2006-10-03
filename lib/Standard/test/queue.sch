; Tests for lib/queue.sch
; 2000-05-18 / lth

(require 'queue)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (queue? (make-queue)) (fail 'queue:1))

(or (queue-empty? (make-queue)) (fail 'queue:2))

(or (let ((q (make-queue)))
      (queue-put! q 1)
      (not (queue-empty? q)))
    (fail 'queue:3))

(or (let ((q (make-queue))
          (d '(1 2 3 4)))
      (for-each (lambda (x) (queue-put! q x)) d)
      (and (equal? d (map (lambda (ignored) (queue-get! q)) d))
           (queue-empty? q)))
    (fail 'queue:4))

(or (let ((q (make-queue))
          (d '(1 2 3 4)))
      (equal? d (map (lambda (x)
                       (queue-put! q x)
                       (queue-get! q))
                     d)))
    (fail 'queue:5))

(or (let ((q (make-queue)))
      (for-each (lambda (x) (queue-put! q x)) '(1 2 3 4))
      (queue-remove! q 1)
      (and (equal? 2 (queue-get! q))
	   (equal? 3 (queue-get! q))
	   (equal? 4 (queue-get! q))))
    (fail 'queue-remove:1))

(or (let ((q (make-queue)))
      (for-each (lambda (x) (queue-put! q x)) '(1 2 3 4))
      (queue-remove! q 2)
      (and (equal? 1 (queue-get! q))
	   (equal? 3 (queue-get! q))
	   (equal? 4 (queue-get! q))))
    (fail 'queue-remove:2))

(display "Done.")
(newline)

; eof
