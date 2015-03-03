; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$
;
; Larceny library -- vector procedures.

($$trace "vector")

(define vector
  (lambda l
    (list->vector l)))

(define (concatenate-vectors vector-list)
  (define (concatenate-vectors1 position tail)
    (cond ((pair? tail)
           (let* ((this-vector  (car tail))
                  (length (vector-length this-vector))
                  (result-vector
                   (concatenate-vectors1 (+ position length) (cdr tail))))
             (vector-copy-into-down! this-vector 0 length
                                     result-vector position)
             result-vector))
          ((null? tail) (make-vector position))
          (else (error 'concatenate-vectors
                       (errmsg 'msg:notlist)
                       vector-list))))
  (concatenate-vectors1 0 vector-list))

(define (vector-append . args)
  (concatenate-vectors args))

; The following comment is of historical interest only.
;
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

(define (vector->list v . rest)
  (define (loop v start i xs)
    (if (< i start)
        xs
        (loop v start (- i 1) (cons (vector-ref v i) xs))))
  (let* ((start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest)))
                  (vector-length v)
                  (cadr rest))))
    (loop v start (- end 1) '())))

(define (vector->string v . rest)
  (define (loop v i end s j)
    (if (>= i end)
        s
        (begin (string-set! s j (vector-ref v i))
               (loop v (+ i 1) end s (+ j 1)))))
  (let* ((start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest)))
                  (vector-length v)
                  (cadr rest)))
         (s (make-string (- end start))))
    (loop v start end s 0)))

(define (string->vector s . rest)
  (define (loop s i end v j)
    (if (>= i end)
        v
        (begin (vector-set! v j (string-ref s i))
               (loop s (+ i 1) end v (+ j 1)))))
  (let* ((start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest)))
                  (string-length s)
                  (cadr rest)))
         (v (make-vector (- end start))))
    (loop s start end v 0)))

; Copies src[i..j] into dst starting at k.
; Used only within this file.
; Performs no checking.
; Assumes src and dst are distinct strings, or that k <= i.

(define vector-copy-into-down!
  (lambda (src i j dst k)
    (do ((i i (.+:idx:idx i 1))
         (k k (.+:idx:idx k 1)))
        ((.>=:fix:fix i j))
      (.vector-set!:trusted dst k (.vector-ref:trusted src i)))))

; As above, but assumes k >= i.

(define vector-copy-into-up!
  (lambda (src i j dst k)
    (do ((j (- j 1) (- j 1))
         (k (+ k (- j i) -1) (- k 1)))
        ((> i j))
      (vector-set! dst k (vector-ref src j)))))

(define vector-copy
  (lambda (x . rest)
    (define (complain msgcode culprit)
      (assertion-violation 'vector-copy
                           (errmsg msgcode)
                           culprit))
    (let* ((start (if (null? rest) 0 (car rest)))
           (end  (if (or (null? rest) (null? (cdr rest)))
                     (vector-length x)
                     (cadr rest)))
           (v (make-vector (- end start))))
      (cond ((not (vector? x))
             (complain 'msg:notvector v))
            ((not (fixnum? start))
             (complain 'msg:notfixnum start))
            ((not (fixnum? end))
             (complain 'msg:notfixnum end))
            ((not (<= 0 start end (vector-length x)))
             (complain 'msg:illegalargs (list x start end)))
            (else
             (vector-copy-into-up! x start end v 0)
             v)))))

;;; R7RS 6.7 says "It is an error if at is less than zero or greater than
;;; the length of to.  It is also an error if (- (vector-length to) at)
;;; is less than (- end start)."
;;; The R7RS does not say what the last argument (end) defaults to if
;;; omitted.  If end is not specified, Larceny uses the largest index
;;; that will work.

(define vector-copy!
  (lambda (dst at src . rest)

    (define (complain0 msgcode)
      (assertion-violation 'vector-copy!
                           (errmsg msgcode)
                           (cons dst (cons at (cons src rest)))))

    (define (complain msgcode culprit)
      (assertion-violation 'vector-copy!
                           (errmsg msgcode)
                           culprit))

    (let* ((start (if (null? rest)
                      0
                      (car rest)))
           (end (if (or (null? rest) (null? (cdr rest)))
                    (min (vector-length src)
                         (+ start (- (vector-length dst) at)))
                    (cadr rest))))
      (cond ((not (vector? dst))
             (complain 'msg:notvector dst))
            ((not (vector? src))
             (complain 'msg:notvector src))
            ((not (fixnum? at))
             (complain 'msg:notfixnum at))
            ((not (fixnum? start))
             (complain 'msg:notfixnum start))
            ((not (fixnum? end))
             (complain 'msg:notfixnum end))
            ((not (and (<= 0 at (vector-length dst))
                       (<= 0 start end (vector-length src))
                       (<= (+ at (- end start)) (vector-length dst))))
             (complain0 'msg:rangeerror))
            (else
             ((if (<= at start)
                  vector-copy-into-down!
                  vector-copy-into-up!)
              src start end dst at))))))

(define vector-fill!
  (lambda (v x . rest)
    (let* ((start (if (null? rest) 0 (car rest)))
           (end (if (or (null? rest) (null? (cdr rest)))
                    (vector-length v)
                    (cadr rest))))
      (subvector-fill! v start end x))))

(define subvector-fill!
  (lambda (v start end x)
    (do ((i start (+ i 1)))
        ((>= i end) v)
        (vector-set! v i x))))

(define (vector-equal? v1 v2)

  (define (v-equal-loop i)
    (cond ((< i 0) #t)
	  ((equal? (vector-ref v1 i) (vector-ref v2 i))
	   (v-equal-loop (- i 1)))
	  (else #f)))

  (if (= (vector-length v1) (vector-length v2))
      (v-equal-loop (- (vector-length v1) 1))
      #f))
 
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
