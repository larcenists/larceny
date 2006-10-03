; Thread-safe queues.
; 2004-01-15 / lth

(require 'assert)
(require 'record)

(define make-queue)            ; (make-queue)              --> queue
(define queue?)                ; (queue? obj)              --> boolean
(define queue-empty?)          ; (queue-empty? queue)      --> boolean
(define queue-put!)            ; (queue-put! queue obj)    --> unspecified
(define queue-get!)            ; (queue-get! queue)        --> obj
(define queue-remove!)         ; (queue-remove! queue obj) --> unspecified

(let* ((queue     (make-record-type "queue" '(head tail)))
       (make      (record-constructor queue))
       (head      (record-accessor queue 'head))
       (head-set! (record-updater queue 'head))
       (tail      (record-accessor queue 'tail))
       (tail-set! (record-updater queue 'tail)))

  (set! make-queue 
        (lambda () (make '() '())))

  (set! queue? 
        (record-predicate queue))

  (set! queue-empty? 
        (lambda (x)
          (null? (head x))))

  (set! queue-put!
        (lambda (queue obj)
          (call-without-interrupts
            (lambda ()
              (let ((item (cons obj '())))
                (if (null? (head queue))
                    (begin (head-set! queue item)
                           (tail-set! queue item))
                    (begin (set-cdr! (tail queue) item)
                           (tail-set! queue item)))
                (unspecified))))))

  (set! queue-get!
        (lambda (queue)
          (call-without-interrupts 
            (lambda ()
              (let ((item (head queue)))
                (assert (not (null? item)))
                (head-set! queue (cdr item))
                (if (null? (cdr item))
                    (tail-set! queue '()))
                (car item))))))

  (set! queue-remove!
	(lambda (queue obj)
	  (call-without-interrupts
	   (lambda ()
	     (let loop ((p (head queue)) (pp '()))
	       (cond ((null? p))
		     ((eq? obj (car p))
		      (if (null? pp)
			  (head-set! queue (cdr p))
			  (set-cdr! pp (cdr p))))
		     (else
		      (loop (cdr p) p))))))))

  'queue)

; eof


