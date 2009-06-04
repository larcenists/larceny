; If the non-predictive collector doesn't win on this,
; then something is very wrong.
;
; (queue-benchmark m n) creates a queue of length m
; and then enqueues and dequeues n elements.

(define default-m 1000000)
(define default-n 10000000)

(define (queue-benchmark . rest)
  (let ((m (if (null? rest) default-m (car rest)))
        (n (if (or (null? rest) (null? (cdr rest)))
               default-n
               (cadr rest))))
    (run-benchmark (string-append "queue"
                                  (number->string m)
                                  "-"
                                  (number->string n))
                   (lambda () (queue-test m n))
                   1)))

(define (queue-test m n)
  (let ((q (make-queue)))
    (define (phase1 m)
      (if (positive? m)
          (begin (enqueue! q m)
                 (phase1 (- m 1)))))
    (define (phase2 n expected)
      (if (positive? n)
          (if (= (queue-front q) expected)
              (begin (dequeue! q)
                     (enqueue! q 0)
                     (phase2 (- n 1)
                             (if (zero? expected)
                                 0
                                 (- expected 1))))
              (error "Bug in queue" q expected))))
    (define (phase3)
      (if (not (queue-empty? q))
          (begin (dequeue! q)
                 (phase3))))
    (phase1 m)
    (phase2 n m)
    (phase3)))

; Queues.

(define make-queue (lambda () ...))
(define enqueue! (lambda (q x) ...))
(define dequeue! (lambda (q) ...))
(define queue-front (lambda (q) ...))
(define queue-empty? (lambda (q) ...))

; A nonempty queue is represented as a vector of the form
; #((*queue*) elements end)
; where elements is a list of the elements
; and end is the last pair in the list of elements.
; An empty queue is represented as
; #((*queue*) () ())

(let ((qtag (list '*queue*)))
  (define (queue? x)
    (and (vector? x)
         (= (vector-length x) 3)
         (eq? qtag (vector-ref x 0))))
  (define (elements q)
    (vector-ref q 1))
  (define (elements! q elements)
    (vector-set! q 1 elements))
  (define (end q)
    (vector-ref q 2))
  (define (end! q x)
    (vector-set! q 2 x))
  (define (qerror x)
    (error "Bad queue" x))
  (set! make-queue
        (lambda ()
          (vector qtag '() '())))
  (set! enqueue!
        (lambda (q x)
          (if (queue? q)
              (let ((y (end q))
                    (z (list x)))
                (if (null? y)
                    (elements! q z)
                    (set-cdr! y z))
                (end! q z))
              (qerror q))))
  (set! dequeue!
        (lambda (q)
          (if (queue? q)
              (let ((y (elements q)))
                (if (null? y)
                    (error "Can't remove elements from an empty queue")
                    (let ((y (cdr y)))
                      (if (null? y)
                          (end! q y))
                      (elements! q y))))
              (qerror q))))
  (set! queue-front
        (lambda (q)
          (if (queue? q)
              (let ((y (elements q)))
                (if (null? y)
                    (error "Can't fetch an element from an empty queue")
                    (car y)))
              (qerror q))))
  (set! queue-empty?
        (lambda (q)
          (if (queue? q)
              (null? (elements q))
              (qerror q))))
  #t)
