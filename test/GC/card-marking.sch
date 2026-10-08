;;; Card marking for large objects.
;;;
;;; The collectors scan only some cards of a large remembered object
;;; (Rts/Sys/card.h).  This program stores young objects into random
;;; slots of old vectors of several sizes (below the size that gets a
;;; card table, just above it, and large; the last card of each is
;;; partial), overwrites some with fixnums so that cards become clean,
;;; allocates between rounds so that minor collections run, keeps live
;;; data turning over so that the regional collector collects and
;;; summarizes its regions, and after each round checks every slot
;;; against a shadow made of small vectors (too small for card tables)
;;; that hold the same objects.  It does so three ways: storing in every
;;; round and collecting fully now and then; storing much at first and
;;; little afterwards, so that cards stay unwritten while the objects
;;; they point to move; and storing in every round for longer.  A card
;;; that holds a pointer into a collected area but is not scanned
;;; leaves that pointer unforwarded, and the check fails or the program
;;; crashes.  Run it with
;;;
;;;     larceny -- card-marking.sch
;;;     larceny -size0 64K -- card-marking.sch
;;;     larceny -rrof -size1 1M -- card-marking.sch
;;;     larceny -rrof -size1 512K -size0 64K -- card-marking.sch
;;;
;;; (and with the other collectors).  It prints "card-marking: ok".
;;; Petit Larceny interprets a source file it loads, which makes this
;;; take minutes; compile it first ((compile-file "card-marking.sch"))
;;; and run card-marking.fasl.
;;; A wrong card does not always show here; for the regional collector,
;;; build the runtime with CARD_CHECK_ACROSS (Rts/Sys/card.h) set, which
;;; checks the cards at each scan.

(define sizes '(4000 8193 8250 100003 1000001))  ; words; 8192 = 64KB

(define seed 12345)
(define (random-below n)
  (set! seed (remainder (+ (* seed 1103515245) 12345) 2147483648))
  (remainder (quotient seed 16) n))

(define ring (make-vector 100 '()))     ; live data, about 3MB
(define ring-index 0)

(define (churn k)
  (do ((i 0 (+ i 1))
       (l '() (if (= 0 (remainder i 500)) '() (cons i l))))
      ((= i k)))
  (do ((j 0 (+ j 1))) ((= j 3))
    (vector-set! ring ring-index (vector->list (make-vector 2000 j)))
    (set! ring-index (remainder (+ ring-index 1) 100))))

(define chunk 1000)                     ; 8000 bytes: no card table

(define (make-shadow n)
  (let ((s (make-vector (quotient (+ n chunk -1) chunk))))
    (do ((i 0 (+ i 1))) ((= i (vector-length s)) s)
      (vector-set! s i (make-vector chunk 0)))))

(define (shadow-ref s i)
  (vector-ref (vector-ref s (quotient i chunk)) (remainder i chunk)))

(define (shadow-set! s i x)
  (vector-set! (vector-ref s (quotient i chunk)) (remainder i chunk) x))

(define (check v shadow)
  (let loop ((i 0))
    (or (= i (vector-length v))
        (let ((x (vector-ref v i)) (y (shadow-ref shadow i)))
          (and (eq? x y)
               (or (not (pair? x)) (eqv? (car (cddr x)) i))
               (loop (+ i 1)))))))

(define (run n rounds stores collect-every)
  (let ((v (make-vector n 0))
        (shadow (make-shadow n)))
    (collect)
    (collect)                           ; v and shadow are old
    (let loop ((r 0) (ok #t))
      (if (or (not ok) (= r rounds))
          ok
          (begin
            (do ((k 0 (+ k 1))) ((= k (stores r)))
              (let* ((i (case (random-below 4)
                          ((0) (- n 1))             ; the last, partial card
                          ((1) (random-below 70))   ; the first cards
                          (else (random-below n))))
                     (young? (< (random-below 3) 2))
                     (x (if young? (list r k i) (+ r k))))
                (vector-set! v i x)
                (shadow-set! shadow i x)))
            (churn 20000)
            (if (and collect-every (= 0 (remainder r collect-every)))
                (collect))
            (loop (+ r 1) (check v shadow)))))))

(define ways                            ; rounds, stores per round, collect
  (list (list 60 (lambda (r) 50) 13)
        (list 150 (lambda (r) (if (< r 5) 300 2)) #f)
        (list 300 (lambda (r) 50) #f)))

(define result
  (let loop ((ws ways) (ss sizes))
    (cond ((null? ws) 'ok)
          ((null? ss) (loop (cdr ws) sizes))
          ((apply run (car ss) (car ws)) (loop ws (cdr ss)))
          (else (list 'failed (car ss) (car (car ws)))))))

(display "card-marking: ")
(display result)
(newline)
(exit)
