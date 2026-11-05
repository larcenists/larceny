;;; SPDX-FileCopyrightText: 2018 John Cowan <cowanu8ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

;;; This code is the same for all SRFI 160 vector sizes.
;;; The u8s appearing in the code are expanded to u8, s8, etc.

;; make-u8vector defined in (srfi 160 base)

;; u8vector defined in (srfi 160 base)

(define (u8vector-unfold f len seed)
  (let ((v (make-u8vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (u8vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (u8vector-unfold-right f len seed)
  (let ((v (make-u8vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (u8vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define u8vector-copy
  (case-lambda
    ((vec) (u8vector-copy* vec 0 (u8vector-length vec)))
    ((vec start) (u8vector-copy* vec start (u8vector-length vec)))
    ((vec start end) (u8vector-copy* vec start end))))

(define (u8vector-copy* vec start end)
  (let ((v (make-u8vector (- end start))))
    (u8vector-copy! v 0 vec start end)
    v))

(define u8vector-copy!
  (case-lambda
    ((to at from)
     (u8vector-copy!* to at from 0 (u8vector-length from)))
    ((to at from start)
     (u8vector-copy!* to at from start (u8vector-length from)))
    ((to at from start end) (u8vector-copy!* to at from start end))))

(define (u8vector-copy!* to at from start end)
  (let loop ((at at) (i start))
    (unless (= i end)
      (u8vector-set! to at (u8vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define u8vector-reverse-copy
  (case-lambda
    ((vec) (u8vector-reverse-copy* vec 0 (u8vector-length vec)))
    ((vec start) (u8vector-reverse-copy* vec start (u8vector-length vec)))
    ((vec start end) (u8vector-reverse-copy* vec start end))))

(define (u8vector-reverse-copy* vec start end)
  (let ((v (make-u8vector (- end start))))
    (u8vector-reverse-copy! v 0 vec start end)
    v))

(define u8vector-reverse-copy!
  (case-lambda
    ((to at from)
     (u8vector-reverse-copy!* to at from 0 (u8vector-length from)))
    ((to at from start)
     (u8vector-reverse-copy!* to at from start (u8vector-length from)))
    ((to at from start end) (u8vector-reverse-copy!* to at from start end))))

(define (u8vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (u8vector-set! to at (u8vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (u8vector-append . vecs)
  (u8vector-concatenate vecs))

(define (u8vector-concatenate vecs)
  (let ((v (make-u8vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (u8vector-copy! v at vec 0 (u8vector-length vec))
          (loop (cdr vecs) (+ at (u8vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (u8vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (u8vector-append-subvectors . args)
  (let ((v (make-u8vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (u8vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; u8? defined in (srfi 160 base)

;; u8vector? defined in (srfi 160 base)

(define (u8vector-empty? vec)
  (zero? (u8vector-length vec)))

(define (u8vector= . vecs)
  (u8vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (u8vector=* vec1 vec2 vecs)
  (and (u8dyadic-vecs= vec1 0 (u8vector-length vec1)
                      vec2 0 (u8vector-length vec2))
       (or (null? vecs)
           (u8vector=* vec2 (car vecs) (cdr vecs)))))

(define (u8dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (u8vector-ref vec1 start1))
           (elt2 (u8vector-ref vec2 start2)))
      (= elt1 elt2))
     (u8dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; u8vector-ref defined in (srfi 160 base)

;; u8vector-length defined in (srfi 160 base)

(define (u8vector-take vec n)
  (let ((v (make-u8vector n)))
    (u8vector-copy! v 0 vec 0 n)
    v))

(define (u8vector-take-right vec n)
  (let ((v (make-u8vector n))
        (len (u8vector-length vec)))
    (u8vector-copy! v 0 vec (- len n) len)
    v))

(define (u8vector-drop vec n)
 (let* ((len (u8vector-length vec))
        (vlen (- len n))
        (v (make-u8vector vlen)))
    (u8vector-copy! v 0 vec n len)
    v))

(define (u8vector-drop-right vec n)
  (let* ((len (u8vector-length vec))
         (rlen (- len n))
         (v (make-u8vector rlen)))
    (u8vector-copy! v 0 vec 0 rlen)
    v))

(define (u8vector-segment vec n)
  (unless (and (integer? n) (positive? n))
    (error "length must be a positive integer" n))
  (let loop ((r '()) (i 0) (remain (u8vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (u8vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%u8vectors-ref vecs i)
  (map (lambda (v) (u8vector-ref v i)) vecs))

(define (u8vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u8vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (u8vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u8vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%u8vectors-ref vecs i))
                (+ i 1)))))))

(define (u8vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u8vector-length vec)))
      (let loop ((r knil) (i (- (u8vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (u8vector-ref vec i)) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u8vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%u8vectors-ref vecs i))
                (- i 1)))))))

(define (u8vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (u8vector-length vec))
           (v (make-u8vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (u8vector-set! v i (f (u8vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u8vector-length vecs)))
           (v (make-u8vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (u8vector-set! v i (apply f (%u8vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))


(define (u8vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u8vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (u8vector-set! vec i (f (u8vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u8vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (u8vector-set! vec i (apply f (%u8vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (u8vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u8vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (u8vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u8vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%u8vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (u8vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u8vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (u8vector-length vec)) r)
         ((pred (u8vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u8vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%u8vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (u8vector-cumulate f knil vec)
  (let* ((len (u8vector-length vec))
         (v (make-u8vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (u8vector-ref vec i))))
          (u8vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (u8vector-foreach f vec)
  (let ((len (u8vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (u8vector-ref vec i))
        (loop (+ i 1))))))

(define (u8vector-take-while pred vec)
  (let* ((len (u8vector-length vec))
         (idx (u8vector-skip pred vec))
         (idx* (if idx idx len)))
    (u8vector-copy vec 0 idx*)))

(define (u8vector-take-while-right pred vec)
  (let* ((len (u8vector-length vec))
         (idx (u8vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (u8vector-copy vec idx* len)))

(define (u8vector-drop-while pred vec)
  (let* ((len (u8vector-length vec))
         (idx (u8vector-skip pred vec))
         (idx* (if idx idx len)))
    (u8vector-copy vec idx* len)))

(define (u8vector-drop-while-right pred vec)
  (let* ((len (u8vector-length vec))
         (idx (u8vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (u8vector-copy vec 0 (+ 1 idx*))))

(define (u8vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u8vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (u8vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u8vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%u8vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (u8vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u8vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (u8vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u8vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%u8vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (u8vector-skip pred vec . vecs)
  (if (null? vecs)
    (u8vector-index (lambda (x) (not (pred x))) vec)
    (apply u8vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (u8vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (u8vector-index-right (lambda (x) (not (pred x))) vec)
    (apply u8vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (u8vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u8vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (u8vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u8vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%u8vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (u8vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (u8vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (u8vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map u8vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%u8vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (u8vector-partition pred vec)
  (let* ((len (u8vector-length vec))
         (cnt (u8vector-count pred vec))
         (r (make-u8vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) (values r cnt))
        ((pred (u8vector-ref vec i))
         (u8vector-set! r yes (u8vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (u8vector-set! r no (u8vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (u8vector-filter pred vec)
  (let* ((len (u8vector-length vec))
         (cnt (u8vector-count pred vec))
         (r (make-u8vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (u8vector-ref vec i))
         (u8vector-set! r j (u8vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (u8vector-remove pred vec)
  (u8vector-filter (lambda (x) (not (pred x))) vec))

;; u8vector-set! defined in (srfi 160 base)

(define (u8vector-swap! vec i j)
  (let ((ival (u8vector-ref vec i))
        (jval (u8vector-ref vec j)))
    (u8vector-set! vec i jval)
    (u8vector-set! vec j ival)))

(define u8vector-fill!
  (case-lambda
    ((vec fill) (u8vector-fill-some! vec fill 0 (u8vector-length vec)))
    ((vec fill start) (u8vector-fill-some! vec fill start (u8vector-length vec)))
    ((vec fill start end) (u8vector-fill-some! vec fill start end))))

(define (u8vector-fill-some! vec fill start end)
  (unless (= start end)
    (u8vector-set! vec start fill)
    (u8vector-fill-some! vec fill (+ start 1) end)))

(define u8vector-reverse!
  (case-lambda
    ((vec) (u8vector-reverse-some! vec 0 (u8vector-length vec)))
    ((vec start) (u8vector-reverse-some! vec start (u8vector-length vec)))
    ((vec start end) (u8vector-reverse-some! vec start end))))

(define (u8vector-reverse-some! vec start end)
  (let loop ((i start) (j (- end 1)))
    (when (< i j)
      (u8vector-swap! vec i j)
      (loop (+ i 1) (- j 1)))))

(define (u8vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f i seed)))
        (u8vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (u8vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f i seed)))
        (u8vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define reverse-u8vector->list
  (case-lambda
    ((vec) (reverse-u8vector->list* vec 0 (u8vector-length vec)))
    ((vec start) (reverse-u8vector->list* vec start (u8vector-length vec)))
    ((vec start end) (reverse-u8vector->list* vec start end))))

(define (reverse-u8vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (u8vector-ref vec i) r)))))

(define (reverse-list->u8vector list)
  (let* ((len (length list))
         (r (make-u8vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (u8vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define u8vector->vector
  (case-lambda
    ((vec) (u8vector->vector* vec 0 (u8vector-length vec)))
    ((vec start) (u8vector->vector* vec start (u8vector-length vec)))
    ((vec start end) (u8vector->vector* vec start end))))

(define (u8vector->vector* vec start end)
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (u8vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define vector->u8vector
  (case-lambda
    ((vec) (vector->u8vector* vec 0 (vector-length vec)))
    ((vec start) (vector->u8vector* vec start (vector-length vec)))
    ((vec start end) (vector->u8vector* vec start end))))

(define (vector->u8vector* vec start end)
  (let* ((len (- end start))
         (r (make-u8vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (u8vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define make-u8vector-generator
  (case-lambda ((vec) (make-u8vector-generator vec 0 (u8vector-length vec)))
               ((vec start) (make-u8vector-generator vec start (u8vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (u8vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define write-u8vector
  (case-lambda
    ((vec) (write-u8vector* vec (current-output-port)))
    ((vec port) (write-u8vector* vec port))))


(define (write-u8vector* vec port)
  (display "#u8(" port)  ; u8-expansion is blind, so will expand this too
  (let ((last (- (u8vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (u8vector-ref vec i) port)
         (display ")" port))
        (else
          (write (u8vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (u8vector< vec1 vec2)
  (let ((len1 (u8vector-length vec1))
        (len2 (u8vector-length vec2)))
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
           ((< (u8vector-ref vec1 i) (u8vector-ref vec2 i))
            #t)
           ((> (u8vector-ref vec1 i) (u8vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (u8vector-hash vec)
  (let ((len (min 256 (u8vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (u8vector-ref vec i)))))))

(define u8vector-comparator
  (make-comparator u8vector? u8vector= u8vector< u8vector-hash))
