;;; Card marking for large objects.
;;;
;;; The generational collector scans only the marked cards of a large
;;; remembered object (Rts/Sys/card.h).  This program stores young
;;; objects into random slots of old vectors of several sizes (below
;;; the size that gets a card table, just above it, and large; the
;;; last card of each is partial), overwrites some with fixnums so that
;;; cards become clean, allocates between rounds so that minor
;;; collections run, collects fully now and then, and after each round
;;; checks every slot against a shadow list kept in a young structure.
;;; A card that holds a young pointer but is not scanned leaves that
;;; pointer unforwarded, and the check fails or the program crashes.
;;; Run it with
;;;
;;;     larceny -- card-marking.sch
;;;     larceny -size0 64K -- card-marking.sch
;;;
;;; (and with the other collectors).  It prints "card-marking: ok".

(define sizes '(4000 8193 8250 100003 1000001))  ; words; 8192 = 64KB

(define seed 12345)
(define (random-below n)
  (set! seed (remainder (+ (* seed 1103515245) 12345) 2147483648))
  (remainder (quotient seed 16) n))

(define (churn k)
  (do ((i 0 (+ i 1))
       (l '() (if (= 0 (remainder i 500)) '() (cons i l))))
      ((= i k))))

(define (check v shadow)
  (let loop ((i 0))
    (or (= i (vector-length v))
        (let ((x (vector-ref v i)) (y (vector-ref shadow i)))
          (and (if (pair? y)
                   (and (pair? x) (equal? x y))
                   (eqv? x y))
               (loop (+ i 1)))))))

(define (run n rounds)
  (let ((v (make-vector n 0))
        (shadow (make-vector n 0)))
    (collect)
    (collect)                           ; v and shadow are old
    (let loop ((r 0) (ok #t))
      (if (or (not ok) (= r rounds))
          ok
          (begin
            (do ((k 0 (+ k 1))) ((= k 50))
              (let* ((i (case (random-below 4)
                          ((0) (- n 1))             ; the last, partial card
                          ((1) (random-below 70))   ; the first cards
                          (else (random-below n))))
                     (young? (< (random-below 3) 2))
                     (x (if young? (list r k i) (+ r k))))
                (vector-set! v i x)
                (vector-set! shadow i (if (pair? x) (list r k i) x))))
            (churn 20000)
            (if (= 0 (remainder r 13)) (collect))
            (loop (+ r 1) (check v shadow)))))))

(define result
  (let loop ((ss sizes))
    (cond ((null? ss) 'ok)
          ((run (car ss) 60) (loop (cdr ss)))
          (else (list 'failed (car ss))))))

(display "card-marking: ")
(display result)
(newline)
(exit)
