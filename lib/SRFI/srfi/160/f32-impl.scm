;;; SPDX-FileCopyrightText: 2018 John Cowan <cowanf32ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

;;; This code is the same for all SRFI 160 vector sizes.
;;; The f32s appearing in the code are expanded to u8, s8, etc.

;; make-f32vector defined in (srfi 160 base)

;; f32vector defined in (srfi 160 base)

(define (f32vector-unfold f len seed)
  (let ((v (make-f32vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (f32vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (f32vector-unfold-right f len seed)
  (let ((v (make-f32vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (f32vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define f32vector-copy
  (case-lambda
    ((vec) (f32vector-copy* vec 0 (f32vector-length vec)))
    ((vec start) (f32vector-copy* vec start (f32vector-length vec)))
    ((vec start end) (f32vector-copy* vec start end))))

(define (f32vector-copy* vec start end)
  (let ((v (make-f32vector (- end start))))
    (f32vector-copy! v 0 vec start end)
    v))

(define f32vector-copy!
  (case-lambda
    ((to at from)
     (f32vector-copy!* to at from 0 (f32vector-length from)))
    ((to at from start)
     (f32vector-copy!* to at from start (f32vector-length from)))
    ((to at from start end) (f32vector-copy!* to at from start end))))

(define (f32vector-copy!* to at from start end)
  (let loop ((at at) (i start))
    (unless (= i end)
      (f32vector-set! to at (f32vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define f32vector-reverse-copy
  (case-lambda
    ((vec) (f32vector-reverse-copy* vec 0 (f32vector-length vec)))
    ((vec start) (f32vector-reverse-copy* vec start (f32vector-length vec)))
    ((vec start end) (f32vector-reverse-copy* vec start end))))

(define (f32vector-reverse-copy* vec start end)
  (let ((v (make-f32vector (- end start))))
    (f32vector-reverse-copy! v 0 vec start end)
    v))

(define f32vector-reverse-copy!
  (case-lambda
    ((to at from)
     (f32vector-reverse-copy!* to at from 0 (f32vector-length from)))
    ((to at from start)
     (f32vector-reverse-copy!* to at from start (f32vector-length from)))
    ((to at from start end) (f32vector-reverse-copy!* to at from start end))))

(define (f32vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (f32vector-set! to at (f32vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (f32vector-append . vecs)
  (f32vector-concatenate vecs))

(define (f32vector-concatenate vecs)
  (let ((v (make-f32vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (f32vector-copy! v at vec 0 (f32vector-length vec))
          (loop (cdr vecs) (+ at (f32vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (f32vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (f32vector-append-subvectors . args)
  (let ((v (make-f32vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (f32vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; f32? defined in (srfi 160 base)

;; f32vector? defined in (srfi 160 base)

(define (f32vector-empty? vec)
  (zero? (f32vector-length vec)))

(define (f32vector= . vecs)
  (f32vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (f32vector=* vec1 vec2 vecs)
  (and (f32dyadic-vecs= vec1 0 (f32vector-length vec1)
                      vec2 0 (f32vector-length vec2))
       (or (null? vecs)
           (f32vector=* vec2 (car vecs) (cdr vecs)))))

(define (f32dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (f32vector-ref vec1 start1))
           (elt2 (f32vector-ref vec2 start2)))
      (= elt1 elt2))
     (f32dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; f32vector-ref defined in (srfi 160 base)

;; f32vector-length defined in (srfi 160 base)

(define (f32vector-take vec n)
  (let ((v (make-f32vector n)))
    (f32vector-copy! v 0 vec 0 n)
    v))

(define (f32vector-take-right vec n)
  (let ((v (make-f32vector n))
        (len (f32vector-length vec)))
    (f32vector-copy! v 0 vec (- len n) len)
    v))

(define (f32vector-drop vec n)
 (let* ((len (f32vector-length vec))
        (vlen (- len n))
        (v (make-f32vector vlen)))
    (f32vector-copy! v 0 vec n len)
    v))

(define (f32vector-drop-right vec n)
  (let* ((len (f32vector-length vec))
         (rlen (- len n))
         (v (make-f32vector rlen)))
    (f32vector-copy! v 0 vec 0 rlen)
    v))

(define (f32vector-segment vec n)
  (unless (and (integer? n) (positive? n))
    (error "length must be a positive integer" n))
  (let loop ((r '()) (i 0) (remain (f32vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (f32vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%f32vectors-ref vecs i)
  (map (lambda (v) (f32vector-ref v i)) vecs))

(define (f32vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (f32vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%f32vectors-ref vecs i))
                (+ i 1)))))))

(define (f32vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((r knil) (i (- (f32vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (f32vector-ref vec i)) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%f32vectors-ref vecs i))
                (- i 1)))))))

(define (f32vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (f32vector-length vec))
           (v (make-f32vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (f32vector-set! v i (f (f32vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs)))
           (v (make-f32vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (f32vector-set! v i (apply f (%f32vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))


(define (f32vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f32vector-set! vec i (f (f32vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (f32vector-set! vec i (apply f (%f32vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (f32vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (f32vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%f32vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (f32vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (f32vector-length vec)) r)
         ((pred (f32vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%f32vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (f32vector-cumulate f knil vec)
  (let* ((len (f32vector-length vec))
         (v (make-f32vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (f32vector-ref vec i))))
          (f32vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (f32vector-foreach f vec)
  (let ((len (f32vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (f32vector-ref vec i))
        (loop (+ i 1))))))

(define (f32vector-take-while pred vec)
  (let* ((len (f32vector-length vec))
         (idx (f32vector-skip pred vec))
         (idx* (if idx idx len)))
    (f32vector-copy vec 0 idx*)))

(define (f32vector-take-while-right pred vec)
  (let* ((len (f32vector-length vec))
         (idx (f32vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (f32vector-copy vec idx* len)))

(define (f32vector-drop-while pred vec)
  (let* ((len (f32vector-length vec))
         (idx (f32vector-skip pred vec))
         (idx* (if idx idx len)))
    (f32vector-copy vec idx* len)))

(define (f32vector-drop-while-right pred vec)
  (let* ((len (f32vector-length vec))
         (idx (f32vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (f32vector-copy vec 0 (+ 1 idx*))))

(define (f32vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (f32vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%f32vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (f32vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (f32vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%f32vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (f32vector-skip pred vec . vecs)
  (if (null? vecs)
    (f32vector-index (lambda (x) (not (pred x))) vec)
    (apply f32vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (f32vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (f32vector-index-right (lambda (x) (not (pred x))) vec)
    (apply f32vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (f32vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (f32vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%f32vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (f32vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (f32vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (f32vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map f32vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%f32vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (f32vector-partition pred vec)
  (let* ((len (f32vector-length vec))
         (cnt (f32vector-count pred vec))
         (r (make-f32vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) (values r cnt))
        ((pred (f32vector-ref vec i))
         (f32vector-set! r yes (f32vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (f32vector-set! r no (f32vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (f32vector-filter pred vec)
  (let* ((len (f32vector-length vec))
         (cnt (f32vector-count pred vec))
         (r (make-f32vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (f32vector-ref vec i))
         (f32vector-set! r j (f32vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (f32vector-remove pred vec)
  (f32vector-filter (lambda (x) (not (pred x))) vec))

;; f32vector-set! defined in (srfi 160 base)

(define (f32vector-swap! vec i j)
  (let ((ival (f32vector-ref vec i))
        (jval (f32vector-ref vec j)))
    (f32vector-set! vec i jval)
    (f32vector-set! vec j ival)))

(define f32vector-fill!
  (case-lambda
    ((vec fill) (f32vector-fill-some! vec fill 0 (f32vector-length vec)))
    ((vec fill start) (f32vector-fill-some! vec fill start (f32vector-length vec)))
    ((vec fill start end) (f32vector-fill-some! vec fill start end))))

(define (f32vector-fill-some! vec fill start end)
  (unless (= start end)
    (f32vector-set! vec start fill)
    (f32vector-fill-some! vec fill (+ start 1) end)))

(define f32vector-reverse!
  (case-lambda
    ((vec) (f32vector-reverse-some! vec 0 (f32vector-length vec)))
    ((vec start) (f32vector-reverse-some! vec start (f32vector-length vec)))
    ((vec start end) (f32vector-reverse-some! vec start end))))

(define (f32vector-reverse-some! vec start end)
  (let loop ((i start) (j (- end 1)))
    (when (< i j)
      (f32vector-swap! vec i j)
      (loop (+ i 1) (- j 1)))))

(define (f32vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f i seed)))
        (f32vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (f32vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f i seed)))
        (f32vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define reverse-f32vector->list
  (case-lambda
    ((vec) (reverse-f32vector->list* vec 0 (f32vector-length vec)))
    ((vec start) (reverse-f32vector->list* vec start (f32vector-length vec)))
    ((vec start end) (reverse-f32vector->list* vec start end))))

(define (reverse-f32vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (f32vector-ref vec i) r)))))

(define (reverse-list->f32vector list)
  (let* ((len (length list))
         (r (make-f32vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (f32vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define f32vector->vector
  (case-lambda
    ((vec) (f32vector->vector* vec 0 (f32vector-length vec)))
    ((vec start) (f32vector->vector* vec start (f32vector-length vec)))
    ((vec start end) (f32vector->vector* vec start end))))

(define (f32vector->vector* vec start end)
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (f32vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define vector->f32vector
  (case-lambda
    ((vec) (vector->f32vector* vec 0 (vector-length vec)))
    ((vec start) (vector->f32vector* vec start (vector-length vec)))
    ((vec start end) (vector->f32vector* vec start end))))

(define (vector->f32vector* vec start end)
  (let* ((len (- end start))
         (r (make-f32vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (f32vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define make-f32vector-generator
  (case-lambda ((vec) (make-f32vector-generator vec 0 (f32vector-length vec)))
               ((vec start) (make-f32vector-generator vec start (f32vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (f32vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define write-f32vector
  (case-lambda
    ((vec) (write-f32vector* vec (current-output-port)))
    ((vec port) (write-f32vector* vec port))))


(define (write-f32vector* vec port)
  (display "#f32(" port)  ; f32-expansion is blind, so will expand this too
  (let ((last (- (f32vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (f32vector-ref vec i) port)
         (display ")" port))
        (else
          (write (f32vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (f32vector< vec1 vec2)
  (let ((len1 (f32vector-length vec1))
        (len2 (f32vector-length vec2)))
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
           ((< (f32vector-ref vec1 i) (f32vector-ref vec2 i))
            #t)
           ((> (f32vector-ref vec1 i) (f32vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (f32vector-hash vec)
  (let ((len (min 256 (f32vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (f32vector-ref vec i)))))))

(define f32vector-comparator
  (make-comparator f32vector? f32vector= f32vector< f32vector-hash))
