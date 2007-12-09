; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$
;
; Larceny library -- vector procedures.

($$trace "vector")

(define vector
  (lambda l
    (list->vector l)))

; List->vector is a partial primop in larceny for the purposes of the
; thesis. The theory is that we can initialize the vector much more
; efficiently if we don't have to check for intergenerational pointers
; all the time, and we can do this when we know that the vector is in
; the tenured area.

(define list->vector
  (lambda (l)
;   (sys$partial-list->vector l (length l))
    (let* ((n (length l))
           (v (make-vector n)))
      (do ((i 0 (+ i 1))
           (l l (cdr l)))
          ((= i n) v)
        (vector-set! v i (car l))))))

(define vector->list
  (letrec ((loop
             (lambda (v i l)
               (if (< i 0)
                   l
                   (loop v (- i 1) (cons (vector-ref v i) l))))))
    (lambda (v)
      (loop v (- (vector-length v) 1) '()))))

(define (vector-fill! v o)
  (define (loop k)
    (if (< k 0)
	#t
	(begin (vector-set! v k o)
	       (loop (- k 1)))))
  (loop (- (vector-length v) 1)))

(define (vector-equal? v1 v2)

  (define (v-equal-loop i)
    (cond ((< i 0) #t)
	  ((equal? (vector-ref v1 i) (vector-ref v2 i))
	   (v-equal-loop (- i 1)))
	  (else #f)))

  (if (= (vector-length v1) (vector-length v2))
      (v-equal-loop (- (vector-length v1) 1))
      #f))
 
(define (vector-copy vec)
  (let* ((l (vector-length vec))
         (v (make-vector l #f)))
    (do ((i 0 (+ i 1)))
        ((= i l) v)
      (vector-set! v i (vector-ref vec i)))))

(define (procedure-copy proc)
  (let* ((l (procedure-length proc))
         (p (make-procedure l)))
    (do ((i 0 (+ i 1)))
        ((= i l) p)
      (procedure-set! p i (procedure-ref proc i)))))
  
; Added for R6RS.

(define (vector-map f v . rest)    ; highly bummed

  (define (finish v i x0 x1 x2 x3 x4 x5 x6 x7)
    (.vector-set!:trusted v i x0)
    (.vector-set!:trusted v (.+:idx:idx i 1) x1)
    (.vector-set!:trusted v (.+:idx:idx i 2) x2)
    (.vector-set!:trusted v (.+:idx:idx i 3) x3)
    (.vector-set!:trusted v (.+:idx:idx i 4) x4)
    (.vector-set!:trusted v (.+:idx:idx i 5) x5)
    (.vector-set!:trusted v (.+:idx:idx i 6) x6)
    (.vector-set!:trusted v (.+:idx:idx i 7) x7)
    v)

  (define (map1 i n)
    (cond ((fx< i (.-:idx:idx n 8))
           (let* ((x0 (f (.vector-ref:trusted v i)))
                  (x1 (f (.vector-ref:trusted v (.+:idx:idx i 1))))
                  (x2 (f (.vector-ref:trusted v (.+:idx:idx i 2))))
                  (x3 (f (.vector-ref:trusted v (.+:idx:idx i 3))))
                  (x4 (f (.vector-ref:trusted v (.+:idx:idx i 4))))
                  (x5 (f (.vector-ref:trusted v (.+:idx:idx i 5))))
                  (x6 (f (.vector-ref:trusted v (.+:idx:idx i 6))))
                  (x7 (f (.vector-ref:trusted v (.+:idx:idx i 7))))
                  (v (map1 (.+:idx:idx i 8) n)))
             (finish v i x0 x1 x2 x3 x4 x5 x6 x7)))
          ((fx< i n)
           (let* ((x (f (.vector-ref:trusted v i)))
                  (v (map1 (.+:idx:idx i 1) n)))
             (.vector-set!:trusted v i x)
             v))
          (else
           (make-vector n))))

  (define (map2 v2 i n)
    (cond ((fx< i (.-:idx:idx n 8))
           (let* ((x0 (f (.vector-ref:trusted v i)
                         (.vector-ref:trusted v2 i)))
                  (x1 (f (.vector-ref:trusted v (.+:idx:idx i 1))
                         (.vector-ref:trusted v2 (.+:idx:idx i 1))))
                  (x2 (f (.vector-ref:trusted v (.+:idx:idx i 2))
                         (.vector-ref:trusted v2 (.+:idx:idx i 2))))
                  (x3 (f (.vector-ref:trusted v (.+:idx:idx i 3))
                         (.vector-ref:trusted v2 (.+:idx:idx i 3))))
                  (x4 (f (.vector-ref:trusted v (.+:idx:idx i 4))
                         (.vector-ref:trusted v2 (.+:idx:idx i 4))))
                  (x5 (f (.vector-ref:trusted v (.+:idx:idx i 5))
                         (.vector-ref:trusted v2 (.+:idx:idx i 5))))
                  (x6 (f (.vector-ref:trusted v (.+:idx:idx i 6))
                         (.vector-ref:trusted v2 (.+:idx:idx i 6))))
                  (x7 (f (.vector-ref:trusted v (.+:idx:idx i 7))
                         (.vector-ref:trusted v2 (.+:idx:idx i 7))))
                  (v (map2 v2 (.+:idx:idx i 8) n)))
             (finish v i x0 x1 x2 x3 x4 x5 x6 x7)))
          ((fx< i n)
           (let* ((x (f (.vector-ref:trusted v i) (.vector-ref:trusted v2 i)))
                  (v (map2 v2 (.+:idx:idx i 1) n)))
             (.vector-set!:trusted v i x)
             v))
          (else
           (make-vector n))))

  (define (map-n rvecs i n)
    (if (fx< i n)
        (do ((rev rvecs (cdr rev))
             (things '() (cons (.vector-ref:trusted (car rev) i) things)))
            ((null? rev)
             (let* ((x (apply f things))
                    (v (map-n rvecs (.+:idx:idx i 1) n)))
               (.vector-set!:trusted v i x)
               v)))
        (make-vector n)))

  (let* ((n (vector-length v)))
    (cond ((null? rest)
           (map1 0 n))
          ((and (null? (cdr rest))
                (vector? (car rest))
                (= n (vector-length (car rest))))
           (map2 (car rest) 0 n))
          (else
           (let ((args (cons v rest)))
             (do ((vs rest (cdr vs)))
                 ((null? vs)
                  (map-n (reverse args) 0 n))
               (let ((x (car vs)))
                 (if (or (not (vector? x))
                         (not (= n (vector-length x))))
                     (assertion-violation 'vector-map
                                          "illegal-arguments"
                                          (cons f args))))))))))

(define (vector-for-each f v . rest)

  (define (for-each1 i n)
    (if (< i n)
	(begin (f (vector-ref v i))
	       (for-each1 (+ i 1) n))
	(unspecified)))

  (define (for-each2 v2 i n)
    (if (< i n)
	(begin (f (vector-ref v i) (vector-ref v2 i))
	       (for-each2 v2 (+ i 1) n))
	(unspecified)))

  (define (for-each-n rvecs i n)
    (if (< i n)
        (do ((rev rvecs (cdr rev))
             (things '() (cons (vector-ref (car rev) i) things)))
            ((null? rev)
             (apply f things)
             (for-each-n rvecs (+ i 1) n)))
	(unspecified)))

  (let ((n (vector-length v)))
    (cond ((null? rest)
           (for-each1 0 n))
          ((and (null? (cdr rest))
                (vector? (car rest))
                (= n (vector-length (car rest))))
           (for-each2 (car rest) 0 n))
          (else
           (let ((args (cons v rest)))
             (do ((vs rest (cdr vs)))
                 ((null? vs)
                  (for-each-n (reverse args) 0 n))
               (let ((x (car vs)))
                 (if (or (not (vector? x))
                         (not (= n (vector-length x))))
                     (assertion-violation 'vector-for-each
                                          "illegal-arguments"
                                          (cons f args))))))))))

; eof
