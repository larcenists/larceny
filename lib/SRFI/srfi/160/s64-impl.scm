;;; SPDX-FileCopyrightText: 2018 John Cowan <cowans64ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

;;; This code is the same for all SRFI 160 vector sizes.
;;; The s64s appearing in the code are expanded to u8, s8, etc.

;; make-s64vector defined in (srfi 160 base)

;; s64vector defined in (srfi 160 base)

(define (s64vector-unfold f len seed)
  (let ((v (make-s64vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (s64vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (s64vector-unfold-right f len seed)
  (let ((v (make-s64vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (s64vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define s64vector-copy
  (case-lambda
    ((vec) (s64vector-copy* vec 0 (s64vector-length vec)))
    ((vec start) (s64vector-copy* vec start (s64vector-length vec)))
    ((vec start end) (s64vector-copy* vec start end))))

(define (s64vector-copy* vec start end)
  (let ((v (make-s64vector (- end start))))
    (s64vector-copy! v 0 vec start end)
    v))

(define s64vector-copy!
  (case-lambda
    ((to at from)
     (s64vector-copy!* to at from 0 (s64vector-length from)))
    ((to at from start)
     (s64vector-copy!* to at from start (s64vector-length from)))
    ((to at from start end) (s64vector-copy!* to at from start end))))

(define (s64vector-copy!* to at from start end)
  (let loop ((at at) (i start))
    (unless (= i end)
      (s64vector-set! to at (s64vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define s64vector-reverse-copy
  (case-lambda
    ((vec) (s64vector-reverse-copy* vec 0 (s64vector-length vec)))
    ((vec start) (s64vector-reverse-copy* vec start (s64vector-length vec)))
    ((vec start end) (s64vector-reverse-copy* vec start end))))

(define (s64vector-reverse-copy* vec start end)
  (let ((v (make-s64vector (- end start))))
    (s64vector-reverse-copy! v 0 vec start end)
    v))

(define s64vector-reverse-copy!
  (case-lambda
    ((to at from)
     (s64vector-reverse-copy!* to at from 0 (s64vector-length from)))
    ((to at from start)
     (s64vector-reverse-copy!* to at from start (s64vector-length from)))
    ((to at from start end) (s64vector-reverse-copy!* to at from start end))))

(define (s64vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (s64vector-set! to at (s64vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (s64vector-append . vecs)
  (s64vector-concatenate vecs))

(define (s64vector-concatenate vecs)
  (let ((v (make-s64vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (s64vector-copy! v at vec 0 (s64vector-length vec))
          (loop (cdr vecs) (+ at (s64vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (s64vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (s64vector-append-subvectors . args)
  (let ((v (make-s64vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (s64vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; s64? defined in (srfi 160 base)

;; s64vector? defined in (srfi 160 base)

(define (s64vector-empty? vec)
  (zero? (s64vector-length vec)))

(define (s64vector= . vecs)
  (s64vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (s64vector=* vec1 vec2 vecs)
  (and (s64dyadic-vecs= vec1 0 (s64vector-length vec1)
                      vec2 0 (s64vector-length vec2))
       (or (null? vecs)
           (s64vector=* vec2 (car vecs) (cdr vecs)))))

(define (s64dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (s64vector-ref vec1 start1))
           (elt2 (s64vector-ref vec2 start2)))
      (= elt1 elt2))
     (s64dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; s64vector-ref defined in (srfi 160 base)

;; s64vector-length defined in (srfi 160 base)

(define (s64vector-take vec n)
  (let ((v (make-s64vector n)))
    (s64vector-copy! v 0 vec 0 n)
    v))

(define (s64vector-take-right vec n)
  (let ((v (make-s64vector n))
        (len (s64vector-length vec)))
    (s64vector-copy! v 0 vec (- len n) len)
    v))

(define (s64vector-drop vec n)
 (let* ((len (s64vector-length vec))
        (vlen (- len n))
        (v (make-s64vector vlen)))
    (s64vector-copy! v 0 vec n len)
    v))

(define (s64vector-drop-right vec n)
  (let* ((len (s64vector-length vec))
         (rlen (- len n))
         (v (make-s64vector rlen)))
    (s64vector-copy! v 0 vec 0 rlen)
    v))

(define (s64vector-segment vec n)
  (unless (and (integer? n) (positive? n))
    (error "length must be a positive integer" n))
  (let loop ((r '()) (i 0) (remain (s64vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (s64vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%s64vectors-ref vecs i)
  (map (lambda (v) (s64vector-ref v i)) vecs))

(define (s64vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s64vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (s64vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s64vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%s64vectors-ref vecs i))
                (+ i 1)))))))

(define (s64vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s64vector-length vec)))
      (let loop ((r knil) (i (- (s64vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (s64vector-ref vec i)) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s64vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%s64vectors-ref vecs i))
                (- i 1)))))))

(define (s64vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (s64vector-length vec))
           (v (make-s64vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (s64vector-set! v i (f (s64vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s64vector-length vecs)))
           (v (make-s64vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (s64vector-set! v i (apply f (%s64vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))


(define (s64vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s64vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (s64vector-set! vec i (f (s64vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s64vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (s64vector-set! vec i (apply f (%s64vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (s64vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s64vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (s64vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s64vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%s64vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (s64vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s64vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (s64vector-length vec)) r)
         ((pred (s64vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s64vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%s64vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (s64vector-cumulate f knil vec)
  (let* ((len (s64vector-length vec))
         (v (make-s64vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (s64vector-ref vec i))))
          (s64vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (s64vector-foreach f vec)
  (let ((len (s64vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (s64vector-ref vec i))
        (loop (+ i 1))))))

(define (s64vector-take-while pred vec)
  (let* ((len (s64vector-length vec))
         (idx (s64vector-skip pred vec))
         (idx* (if idx idx len)))
    (s64vector-copy vec 0 idx*)))

(define (s64vector-take-while-right pred vec)
  (let* ((len (s64vector-length vec))
         (idx (s64vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (s64vector-copy vec idx* len)))

(define (s64vector-drop-while pred vec)
  (let* ((len (s64vector-length vec))
         (idx (s64vector-skip pred vec))
         (idx* (if idx idx len)))
    (s64vector-copy vec idx* len)))

(define (s64vector-drop-while-right pred vec)
  (let* ((len (s64vector-length vec))
         (idx (s64vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (s64vector-copy vec 0 (+ 1 idx*))))

(define (s64vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (s64vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%s64vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (s64vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s64vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (s64vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s64vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%s64vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (s64vector-skip pred vec . vecs)
  (if (null? vecs)
    (s64vector-index (lambda (x) (not (pred x))) vec)
    (apply s64vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (s64vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (s64vector-index-right (lambda (x) (not (pred x))) vec)
    (apply s64vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (s64vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s64vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (s64vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s64vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%s64vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (s64vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s64vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (s64vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s64vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%s64vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (s64vector-partition pred vec)
  (let* ((len (s64vector-length vec))
         (cnt (s64vector-count pred vec))
         (r (make-s64vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) (values r cnt))
        ((pred (s64vector-ref vec i))
         (s64vector-set! r yes (s64vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (s64vector-set! r no (s64vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (s64vector-filter pred vec)
  (let* ((len (s64vector-length vec))
         (cnt (s64vector-count pred vec))
         (r (make-s64vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (s64vector-ref vec i))
         (s64vector-set! r j (s64vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (s64vector-remove pred vec)
  (s64vector-filter (lambda (x) (not (pred x))) vec))

;; s64vector-set! defined in (srfi 160 base)

(define (s64vector-swap! vec i j)
  (let ((ival (s64vector-ref vec i))
        (jval (s64vector-ref vec j)))
    (s64vector-set! vec i jval)
    (s64vector-set! vec j ival)))

(define s64vector-fill!
  (case-lambda
    ((vec fill) (s64vector-fill-some! vec fill 0 (s64vector-length vec)))
    ((vec fill start) (s64vector-fill-some! vec fill start (s64vector-length vec)))
    ((vec fill start end) (s64vector-fill-some! vec fill start end))))

(define (s64vector-fill-some! vec fill start end)
  (unless (= start end)
    (s64vector-set! vec start fill)
    (s64vector-fill-some! vec fill (+ start 1) end)))

(define s64vector-reverse!
  (case-lambda
    ((vec) (s64vector-reverse-some! vec 0 (s64vector-length vec)))
    ((vec start) (s64vector-reverse-some! vec start (s64vector-length vec)))
    ((vec start end) (s64vector-reverse-some! vec start end))))

(define (s64vector-reverse-some! vec start end)
  (let loop ((i start) (j (- end 1)))
    (when (< i j)
      (s64vector-swap! vec i j)
      (loop (+ i 1) (- j 1)))))

(define (s64vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f i seed)))
        (s64vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (s64vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f i seed)))
        (s64vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define reverse-s64vector->list
  (case-lambda
    ((vec) (reverse-s64vector->list* vec 0 (s64vector-length vec)))
    ((vec start) (reverse-s64vector->list* vec start (s64vector-length vec)))
    ((vec start end) (reverse-s64vector->list* vec start end))))

(define (reverse-s64vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (s64vector-ref vec i) r)))))

(define (reverse-list->s64vector list)
  (let* ((len (length list))
         (r (make-s64vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (s64vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define s64vector->vector
  (case-lambda
    ((vec) (s64vector->vector* vec 0 (s64vector-length vec)))
    ((vec start) (s64vector->vector* vec start (s64vector-length vec)))
    ((vec start end) (s64vector->vector* vec start end))))

(define (s64vector->vector* vec start end)
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (s64vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define vector->s64vector
  (case-lambda
    ((vec) (vector->s64vector* vec 0 (vector-length vec)))
    ((vec start) (vector->s64vector* vec start (vector-length vec)))
    ((vec start end) (vector->s64vector* vec start end))))

(define (vector->s64vector* vec start end)
  (let* ((len (- end start))
         (r (make-s64vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (s64vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define make-s64vector-generator
  (case-lambda ((vec) (make-s64vector-generator vec 0 (s64vector-length vec)))
               ((vec start) (make-s64vector-generator vec start (s64vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (s64vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define write-s64vector
  (case-lambda
    ((vec) (write-s64vector* vec (current-output-port)))
    ((vec port) (write-s64vector* vec port))))


(define (write-s64vector* vec port)
  (display "#s64(" port)  ; s64-expansion is blind, so will expand this too
  (let ((last (- (s64vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (s64vector-ref vec i) port)
         (display ")" port))
        (else
          (write (s64vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (s64vector< vec1 vec2)
  (let ((len1 (s64vector-length vec1))
        (len2 (s64vector-length vec2)))
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
           ((< (s64vector-ref vec1 i) (s64vector-ref vec2 i))
            #t)
           ((> (s64vector-ref vec1 i) (s64vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (s64vector-hash vec)
  (let ((len (min 256 (s64vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (s64vector-ref vec i)))))))

(define s64vector-comparator
  (make-comparator s64vector? s64vector= s64vector< s64vector-hash))
