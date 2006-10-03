; Functional queues
; 2000-11-19 / lth

(define make-fqueue)                    ; returns a queue
(define fqueue?)                        ; returns a bool
(define fqueue-empty?)                  ; returns a bool
(define fqueue-get)                     ; returns an object and a queue
(define fqueue-put)                     ; returns a queue
(define fqueue->list)                   ; returns a list
(define list->fqueue)                   ; returns a queue

(let* ((queue    (make-record-type "fqueue" '(in-order out-of-order)))
       (make     (record-constructor queue))
       (in-order (record-accessor queue 'in-order))
       (out-of-order (record-accessor queue 'out-of-order)))

  (set! make-fqueue
        (lambda () (make '() '())))

  (set! fqueue?
        (record-predicate queue))

  (set! fqueue-empty?
        (lambda (x)
          (and (null? (in-order x))
               (null? (out-of-order x)))))

  (set! fqueue-get
        (lambda (x)
          (let ((in-order (in-order x))
                (out-of-order (out-of-order x)))
            (cond ((not (null? in-order))
                   (values (car in-order)
                           (make (cdr in-order) out-of-order)))
                  ((not (null? out-of-order))
                   (let ((r (reverse out-of-order)))
                     (values (car r)
                             (make (cdr r) '()))))
                  (else
                   (error "Queue is empty: " x))))))

  (set! fqueue-put
        (lambda (x v)
          (let ((in-order (in-order x))
                (out-of-order (out-of-order x)))
            (make in-order (cons v out-of-order)))))

  'fqueue)


; Useful abstractions

(define (fqueue->list q)
  (if (fqueue-empty? q)
      '()
      (let-values (((x q) (fqueue-get q)))
        (cons x (fqueue->list q)))))

(define (list->fqueue l . rest)
  (let ((q (if (null? rest)
               (make-fqueue)
               (car rest))))
    (do ((l l (cdr l))
         (q q (fqueue-put q (car l))))
        ((null? l) q))))

; eof
