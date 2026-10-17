;;; SPDX-FileCopyrightText: 2018 John Cowan <cowanf64ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

;;; This code is the same for all SRFI 160 vector sizes.
;;; The f64s appearing in the code are expanded to u8, s8, etc.

;; make-f64vector defined in (srfi 160 base)

;; f64vector defined in (srfi 160 base)

(define (f64vector-unfold f len seed)
  (let ((v (make-f64vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (f64vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (f64vector-unfold-right f len seed)
  (let ((v (make-f64vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (f64vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define f64vector-copy
  (case-lambda
    ((vec) (f64vector-copy* vec 0 (f64vector-length vec)))
    ((vec start) (f64vector-copy* vec start (f64vector-length vec)))
    ((vec start end) (f64vector-copy* vec start end))))

(define (f64vector-copy* vec start end)
  (let ((v (make-f64vector (- end start))))
    (f64vector-copy! v 0 vec start end)
    v))

(define f64vector-copy!
  (case-lambda
    ((to at from)
     (f64vector-copy!* to at from 0 (f64vector-length from)))
    ((to at from start)
     (f64vector-copy!* to at from start (f64vector-length from)))
    ((to at from start end) (f64vector-copy!* to at from start end))))

(define (f64vector-copy!* to at from start end)
  (let loop ((at at) (i start))
    (unless (= i end)
      (f64vector-set! to at (f64vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define f64vector-reverse-copy
  (case-lambda
    ((vec) (f64vector-reverse-copy* vec 0 (f64vector-length vec)))
    ((vec start) (f64vector-reverse-copy* vec start (f64vector-length vec)))
    ((vec start end) (f64vector-reverse-copy* vec start end))))

(define (f64vector-reverse-copy* vec start end)
  (let ((v (make-f64vector (- end start))))
    (f64vector-reverse-copy! v 0 vec start end)
    v))

(define f64vector-reverse-copy!
  (case-lambda
    ((to at from)
     (f64vector-reverse-copy!* to at from 0 (f64vector-length from)))
    ((to at from start)
     (f64vector-reverse-copy!* to at from start (f64vector-length from)))
    ((to at from start end) (f64vector-reverse-copy!* to at from start end))))

(define (f64vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (f64vector-set! to at (f64vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (f64vector-append . vecs)
  (f64vector-concatenate vecs))

(define (f64vector-concatenate vecs)
  (let ((v (make-f64vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (f64vector-copy! v at vec 0 (f64vector-length vec))
          (loop (cdr vecs) (+ at (f64vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (f64vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (f64vector-append-subvectors . args)
  (let ((v (make-f64vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (f64vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; f64? defined in (srfi 160 base)

;; f64vector? defined in (srfi 160 base)

(define (f64vector-empty? vec)
  (zero? (f64vector-length vec)))

(define (f64vector= . vecs)
  (f64vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (f64vector=* vec1 vec2 vecs)
  (and (f64dyadic-vecs= vec1 0 (f64vector-length vec1)
                      vec2 0 (f64vector-length vec2))
       (or (null? vecs)
           (f64vector=* vec2 (car vecs) (cdr vecs)))))

(define (f64dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (f64vector-ref vec1 start1))
           (elt2 (f64vector-ref vec2 start2)))
      (= elt1 elt2))
     (f64dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; f64vector-ref defined in (srfi 160 base)

;; f64vector-length defined in (srfi 160 base)

(define (f64vector-take vec n)
  (let ((v (make-f64vector n)))
    (f64vector-copy! v 0 vec 0 n)
    v))

(define (f64vector-take-right vec n)
  (let ((v (make-f64vector n))
        (len (f64vector-length vec)))
    (f64vector-copy! v 0 vec (- len n) len)
    v))

(define (f64vector-drop vec n)
 (let* ((len (f64vector-length vec))
        (vlen (- len n))
        (v (make-f64vector vlen)))
    (f64vector-copy! v 0 vec n len)
    v))

(define (f64vector-drop-right vec n)
  (let* ((len (f64vector-length vec))
         (rlen (- len n))
         (v (make-f64vector rlen)))
    (f64vector-copy! v 0 vec 0 rlen)
    v))

(define (f64vector-segment vec n)
  (unless (and (integer? n) (positive? n))
    (error "length must be a positive integer" n))
  (let loop ((r '()) (i 0) (remain (f64vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (f64vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%f64vectors-ref vecs i)
  (map (lambda (v) (f64vector-ref v i)) vecs))

(define (f64vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (f64vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%f64vectors-ref vecs i))
                (+ i 1)))))))

(define (f64vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((r knil) (i (- (f64vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (f64vector-ref vec i)) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%f64vectors-ref vecs i))
                (- i 1)))))))

(define (f64vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (f64vector-length vec))
           (v (make-f64vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (f64vector-set! v i (f (f64vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs)))
           (v (make-f64vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (f64vector-set! v i (apply f (%f64vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))


(define (f64vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f64vector-set! vec i (f (f64vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (f64vector-set! vec i (apply f (%f64vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (f64vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (f64vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%f64vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (f64vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (f64vector-length vec)) r)
         ((pred (f64vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%f64vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (f64vector-cumulate f knil vec)
  (let* ((len (f64vector-length vec))
         (v (make-f64vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (f64vector-ref vec i))))
          (f64vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (f64vector-foreach f vec)
  (let ((len (f64vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (f64vector-ref vec i))
        (loop (+ i 1))))))

(define (f64vector-take-while pred vec)
  (let* ((len (f64vector-length vec))
         (idx (f64vector-skip pred vec))
         (idx* (if idx idx len)))
    (f64vector-copy vec 0 idx*)))

(define (f64vector-take-while-right pred vec)
  (let* ((len (f64vector-length vec))
         (idx (f64vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (f64vector-copy vec idx* len)))

(define (f64vector-drop-while pred vec)
  (let* ((len (f64vector-length vec))
         (idx (f64vector-skip pred vec))
         (idx* (if idx idx len)))
    (f64vector-copy vec idx* len)))

(define (f64vector-drop-while-right pred vec)
  (let* ((len (f64vector-length vec))
         (idx (f64vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (f64vector-copy vec 0 (+ 1 idx*))))

(define (f64vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (f64vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%f64vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (f64vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (f64vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%f64vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (f64vector-skip pred vec . vecs)
  (if (null? vecs)
    (f64vector-index (lambda (x) (not (pred x))) vec)
    (apply f64vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (f64vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (f64vector-index-right (lambda (x) (not (pred x))) vec)
    (apply f64vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (f64vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (f64vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%f64vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (f64vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f64vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (f64vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f64vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%f64vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (f64vector-partition pred vec)
  (let* ((len (f64vector-length vec))
         (cnt (f64vector-count pred vec))
         (r (make-f64vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) (values r cnt))
        ((pred (f64vector-ref vec i))
         (f64vector-set! r yes (f64vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (f64vector-set! r no (f64vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (f64vector-filter pred vec)
  (let* ((len (f64vector-length vec))
         (cnt (f64vector-count pred vec))
         (r (make-f64vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (f64vector-ref vec i))
         (f64vector-set! r j (f64vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (f64vector-remove pred vec)
  (f64vector-filter (lambda (x) (not (pred x))) vec))

;; f64vector-set! defined in (srfi 160 base)

(define (f64vector-swap! vec i j)
  (let ((ival (f64vector-ref vec i))
        (jval (f64vector-ref vec j)))
    (f64vector-set! vec i jval)
    (f64vector-set! vec j ival)))

(define f64vector-fill!
  (case-lambda
    ((vec fill) (f64vector-fill-some! vec fill 0 (f64vector-length vec)))
    ((vec fill start) (f64vector-fill-some! vec fill start (f64vector-length vec)))
    ((vec fill start end) (f64vector-fill-some! vec fill start end))))

(define (f64vector-fill-some! vec fill start end)
  (unless (= start end)
    (f64vector-set! vec start fill)
    (f64vector-fill-some! vec fill (+ start 1) end)))

(define f64vector-reverse!
  (case-lambda
    ((vec) (f64vector-reverse-some! vec 0 (f64vector-length vec)))
    ((vec start) (f64vector-reverse-some! vec start (f64vector-length vec)))
    ((vec start end) (f64vector-reverse-some! vec start end))))

(define (f64vector-reverse-some! vec start end)
  (let loop ((i start) (j (- end 1)))
    (when (< i j)
      (f64vector-swap! vec i j)
      (loop (+ i 1) (- j 1)))))

(define (f64vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f i seed)))
        (f64vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (f64vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f i seed)))
        (f64vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define reverse-f64vector->list
  (case-lambda
    ((vec) (reverse-f64vector->list* vec 0 (f64vector-length vec)))
    ((vec start) (reverse-f64vector->list* vec start (f64vector-length vec)))
    ((vec start end) (reverse-f64vector->list* vec start end))))

(define (reverse-f64vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (f64vector-ref vec i) r)))))

(define (reverse-list->f64vector list)
  (let* ((len (length list))
         (r (make-f64vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (f64vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define f64vector->vector
  (case-lambda
    ((vec) (f64vector->vector* vec 0 (f64vector-length vec)))
    ((vec start) (f64vector->vector* vec start (f64vector-length vec)))
    ((vec start end) (f64vector->vector* vec start end))))

(define (f64vector->vector* vec start end)
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (f64vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define vector->f64vector
  (case-lambda
    ((vec) (vector->f64vector* vec 0 (vector-length vec)))
    ((vec start) (vector->f64vector* vec start (vector-length vec)))
    ((vec start end) (vector->f64vector* vec start end))))

(define (vector->f64vector* vec start end)
  (let* ((len (- end start))
         (r (make-f64vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (f64vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define make-f64vector-generator
  (case-lambda ((vec) (make-f64vector-generator vec 0 (f64vector-length vec)))
               ((vec start) (make-f64vector-generator vec start (f64vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (f64vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define write-f64vector
  (case-lambda
    ((vec) (write-f64vector* vec (current-output-port)))
    ((vec port) (write-f64vector* vec port))))


(define (write-f64vector* vec port)
  (display "#f64(" port)  ; f64-expansion is blind, so will expand this too
  (let ((last (- (f64vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (f64vector-ref vec i) port)
         (display ")" port))
        (else
          (write (f64vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (f64vector< vec1 vec2)
  (let ((len1 (f64vector-length vec1))
        (len2 (f64vector-length vec2)))
    (cond
      ((< len1 len2)
       #t)
      ((> len1 len2)
       #f)
      (else
       (let loop ((i 0))
         (cond
           ((= i len1)
            #f)
           ((< (f64vector-ref vec1 i) (f64vector-ref vec2 i))
            #t)
           ((> (f64vector-ref vec1 i) (f64vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (f64vector-hash vec)
  (let ((len (min 256 (f64vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (f64vector-ref vec i)))))))

(define f64vector-comparator
  (make-comparator f64vector? f64vector= f64vector< f64vector-hash))
