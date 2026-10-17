;;; SPDX-FileCopyrightText: 2018 John Cowan <cowans32ccil.org>
;;;
;;; SPDX-License-Identifier: MIT

;;; This code is the same for all SRFI 160 vector sizes.
;;; The s32s appearing in the code are expanded to u8, s8, etc.

;; make-s32vector defined in (srfi 160 base)

;; s32vector defined in (srfi 160 base)

(define (s32vector-unfold f len seed)
  (let ((v (make-s32vector len)))
    (let loop ((i 0) (state seed))
      (unless (= i len)
        (let-values (((value newstate) (f i state)))
          (s32vector-set! v i value)
          (loop (+ i 1) newstate))))
    v))

(define (s32vector-unfold-right f len seed)
  (let ((v (make-s32vector len)))
    (let loop ((i (- len 1)) (state seed))
      (unless (= i -1)
        (let-values (((value newstate) (f i state)))
          (s32vector-set! v i value)
          (loop (- i 1) newstate))))
    v))

(define s32vector-copy
  (case-lambda
    ((vec) (s32vector-copy* vec 0 (s32vector-length vec)))
    ((vec start) (s32vector-copy* vec start (s32vector-length vec)))
    ((vec start end) (s32vector-copy* vec start end))))

(define (s32vector-copy* vec start end)
  (let ((v (make-s32vector (- end start))))
    (s32vector-copy! v 0 vec start end)
    v))

(define s32vector-copy!
  (case-lambda
    ((to at from)
     (s32vector-copy!* to at from 0 (s32vector-length from)))
    ((to at from start)
     (s32vector-copy!* to at from start (s32vector-length from)))
    ((to at from start end) (s32vector-copy!* to at from start end))))

(define (s32vector-copy!* to at from start end)
  (let loop ((at at) (i start))
    (unless (= i end)
      (s32vector-set! to at (s32vector-ref from i))
      (loop (+ at 1) (+ i 1)))))

(define s32vector-reverse-copy
  (case-lambda
    ((vec) (s32vector-reverse-copy* vec 0 (s32vector-length vec)))
    ((vec start) (s32vector-reverse-copy* vec start (s32vector-length vec)))
    ((vec start end) (s32vector-reverse-copy* vec start end))))

(define (s32vector-reverse-copy* vec start end)
  (let ((v (make-s32vector (- end start))))
    (s32vector-reverse-copy! v 0 vec start end)
    v))

(define s32vector-reverse-copy!
  (case-lambda
    ((to at from)
     (s32vector-reverse-copy!* to at from 0 (s32vector-length from)))
    ((to at from start)
     (s32vector-reverse-copy!* to at from start (s32vector-length from)))
    ((to at from start end) (s32vector-reverse-copy!* to at from start end))))

(define (s32vector-reverse-copy!* to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (s32vector-set! to at (s32vector-ref from i))
      (loop (+ at 1) (- i 1)))))

(define (s32vector-append . vecs)
  (s32vector-concatenate vecs))

(define (s32vector-concatenate vecs)
  (let ((v (make-s32vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (s32vector-copy! v at vec 0 (s32vector-length vec))
          (loop (cdr vecs) (+ at (s32vector-length vec)))))
    v)))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (s32vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (s32vector-append-subvectors . args)
  (let ((v (make-s32vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (s32vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum vecs)
  (if (null? vecs)
    0
    (+ (- (caddr vecs) (cadr vecs))
       (len-subsum (cdddr vecs)))))

;; s32? defined in (srfi 160 base)

;; s32vector? defined in (srfi 160 base)

(define (s32vector-empty? vec)
  (zero? (s32vector-length vec)))

(define (s32vector= . vecs)
  (s32vector=* (car vecs) (cadr vecs) (cddr vecs)))

(define (s32vector=* vec1 vec2 vecs)
  (and (s32dyadic-vecs= vec1 0 (s32vector-length vec1)
                      vec2 0 (s32vector-length vec2))
       (or (null? vecs)
           (s32vector=* vec2 (car vecs) (cdr vecs)))))

(define (s32dyadic-vecs= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((not (< start1 end1)) #t)
    ((let ((elt1 (s32vector-ref vec1 start1))
           (elt2 (s32vector-ref vec2 start2)))
      (= elt1 elt2))
     (s32dyadic-vecs= vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2))
    (else #f)))

;; s32vector-ref defined in (srfi 160 base)

;; s32vector-length defined in (srfi 160 base)

(define (s32vector-take vec n)
  (let ((v (make-s32vector n)))
    (s32vector-copy! v 0 vec 0 n)
    v))

(define (s32vector-take-right vec n)
  (let ((v (make-s32vector n))
        (len (s32vector-length vec)))
    (s32vector-copy! v 0 vec (- len n) len)
    v))

(define (s32vector-drop vec n)
 (let* ((len (s32vector-length vec))
        (vlen (- len n))
        (v (make-s32vector vlen)))
    (s32vector-copy! v 0 vec n len)
    v))

(define (s32vector-drop-right vec n)
  (let* ((len (s32vector-length vec))
         (rlen (- len n))
         (v (make-s32vector rlen)))
    (s32vector-copy! v 0 vec 0 rlen)
    v))

(define (s32vector-segment vec n)
  (unless (and (integer? n) (positive? n))
    (error "length must be a positive integer" n))
  (let loop ((r '()) (i 0) (remain (s32vector-length vec)))
    (if (<= remain 0)
      (reverse r)
      (let ((size (min n remain)))
        (loop
          (cons (s32vector-copy vec i (+ i size)) r)
          (+ i size)
          (- remain size))))))

;; aux. procedure
(define (%s32vectors-ref vecs i)
  (map (lambda (v) (s32vector-ref v i)) vecs))

(define (s32vector-fold kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (kons r (s32vector-ref vec i)) (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((r knil) (i 0))
        (if (= i len)
          r
          (loop (apply kons r (%s32vectors-ref vecs i))
                (+ i 1)))))))

(define (s32vector-fold-right kons knil vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((r knil) (i (- (s32vector-length vec) 1)))
        (if (negative? i)
          r
          (loop (kons r (s32vector-ref vec i)) (- i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((r knil) (i (- len 1)))
        (if (negative? i)
          r
          (loop (apply kons r (%s32vectors-ref vecs i))
                (- i 1)))))))

(define (s32vector-map f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let* ((len (s32vector-length vec))
           (v (make-s32vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (s32vector-set! v i (f (s32vector-ref vec i)))
          (loop (+ i 1))))
      v)
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs)))
           (v (make-s32vector len)))
      (let loop ((i 0))
        (unless (= i len)
          (s32vector-set! v i (apply f (%s32vectors-ref vecs i)))
          (loop (+ i 1))))
      v)))


(define (s32vector-map! f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (s32vector-set! vec i (f (s32vector-ref vec i)))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (s32vector-set! vec i (apply f (%s32vectors-ref vecs i)))
          (loop (+ i 1)))))))

(define (s32vector-for-each f vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0))
        (unless (= i len)
          (f (s32vector-ref vec i))
          (loop (+ i 1)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0))
        (unless (= i len)
          (apply f (%s32vectors-ref vecs i))
          (loop (+ i 1)))))))

(define (s32vector-count pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0) (r 0))
        (cond
         ((= i (s32vector-length vec)) r)
         ((pred (s32vector-ref vec i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0) (r 0))
        (cond
         ((= i len) r)
         ((apply pred (%s32vectors-ref vecs i)) (loop (+ i 1) (+ r 1)))
         (else (loop (+ i 1) r)))))))

(define (s32vector-cumulate f knil vec)
  (let* ((len (s32vector-length vec))
         (v (make-s32vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f r (s32vector-ref vec i))))
          (s32vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (s32vector-foreach f vec)
  (let ((len (s32vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (s32vector-ref vec i))
        (loop (+ i 1))))))

(define (s32vector-take-while pred vec)
  (let* ((len (s32vector-length vec))
         (idx (s32vector-skip pred vec))
         (idx* (if idx idx len)))
    (s32vector-copy vec 0 idx*)))

(define (s32vector-take-while-right pred vec)
  (let* ((len (s32vector-length vec))
         (idx (s32vector-skip-right pred vec))
         (idx* (if idx (+ idx 1) 0)))
    (s32vector-copy vec idx* len)))

(define (s32vector-drop-while pred vec)
  (let* ((len (s32vector-length vec))
         (idx (s32vector-skip pred vec))
         (idx* (if idx idx len)))
    (s32vector-copy vec idx* len)))

(define (s32vector-drop-while-right pred vec)
  (let* ((len (s32vector-length vec))
         (idx (s32vector-skip-right pred vec))
         (idx* (if idx idx -1)))
    (s32vector-copy vec 0 (+ 1 idx*))))

(define (s32vector-index pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (s32vector-ref vec i)) i)
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%s32vectors-ref vecs i)) i)
         (else (loop (+ i 1))))))))

(define (s32vector-index-right pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((pred (s32vector-ref vec i)) i)
         (else (loop (- i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i (- len 1)))
        (cond
         ((negative? i) #f)
         ((apply pred (%s32vectors-ref vecs i)) i)
         (else (loop (- i 1))))))))

(define (s32vector-skip pred vec . vecs)
  (if (null? vecs)
    (s32vector-index (lambda (x) (not (pred x))) vec)
    (apply s32vector-index (lambda xs (not (apply pred xs))) vec vecs)))

(define (s32vector-skip-right pred vec . vecs)
  (if (null? vecs)
    (s32vector-index-right (lambda (x) (not (pred x))) vec)
    (apply s32vector-index-right (lambda xs (not (apply pred xs))) vec vecs)))

(define (s32vector-any pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((pred (s32vector-ref vec i)))  ;returns result of pred
         (else (loop (+ i 1))))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0))
        (cond
         ((= i len) #f)
         ((apply pred (%s32vectors-ref vecs i))) ;returns result of pred
         (else (loop (+ i 1))))))))

(define (s32vector-every pred vec . vecs)
  (if (null? vecs)
    ;; fast path
    (let ((len (s32vector-length vec)))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((pred (s32vector-ref vec i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))
    ;; generic case
    (let* ((vecs (cons vec vecs))
           (len (apply min (map s32vector-length vecs))))
      (let loop ((i 0) (last #t))
        (cond
         ((= i len) last)
         ((apply pred (%s32vectors-ref vecs i)) => (lambda (r) (loop (+ i 1) r)))
         (else #f))))))

(define (s32vector-partition pred vec)
  (let* ((len (s32vector-length vec))
         (cnt (s32vector-count pred vec))
         (r (make-s32vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) (values r cnt))
        ((pred (s32vector-ref vec i))
         (s32vector-set! r yes (s32vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (s32vector-set! r no (s32vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (s32vector-filter pred vec)
  (let* ((len (s32vector-length vec))
         (cnt (s32vector-count pred vec))
         (r (make-s32vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred (s32vector-ref vec i))
         (s32vector-set! r j (s32vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (s32vector-remove pred vec)
  (s32vector-filter (lambda (x) (not (pred x))) vec))

;; s32vector-set! defined in (srfi 160 base)

(define (s32vector-swap! vec i j)
  (let ((ival (s32vector-ref vec i))
        (jval (s32vector-ref vec j)))
    (s32vector-set! vec i jval)
    (s32vector-set! vec j ival)))

(define s32vector-fill!
  (case-lambda
    ((vec fill) (s32vector-fill-some! vec fill 0 (s32vector-length vec)))
    ((vec fill start) (s32vector-fill-some! vec fill start (s32vector-length vec)))
    ((vec fill start end) (s32vector-fill-some! vec fill start end))))

(define (s32vector-fill-some! vec fill start end)
  (unless (= start end)
    (s32vector-set! vec start fill)
    (s32vector-fill-some! vec fill (+ start 1) end)))

(define s32vector-reverse!
  (case-lambda
    ((vec) (s32vector-reverse-some! vec 0 (s32vector-length vec)))
    ((vec start) (s32vector-reverse-some! vec start (s32vector-length vec)))
    ((vec start end) (s32vector-reverse-some! vec start end))))

(define (s32vector-reverse-some! vec start end)
  (let loop ((i start) (j (- end 1)))
    (when (< i j)
      (s32vector-swap! vec i j)
      (loop (+ i 1) (- j 1)))))

(define (s32vector-unfold! f vec start end seed)
  (let loop ((i start) (seed seed))
    (when (< i end)
      (let-values (((elt seed) (f i seed)))
        (s32vector-set! vec i elt)
        (loop (+ i 1) seed)))))

(define (s32vector-unfold-right! f vec start end seed)
  (let loop ((i (- end 1)) (seed seed))
    (when (>= i start)
      (let-values (((elt seed) (f i seed)))
        (s32vector-set! vec i elt)
        (loop (- i 1) seed)))))

(define reverse-s32vector->list
  (case-lambda
    ((vec) (reverse-s32vector->list* vec 0 (s32vector-length vec)))
    ((vec start) (reverse-s32vector->list* vec start (s32vector-length vec)))
    ((vec start end) (reverse-s32vector->list* vec start end))))

(define (reverse-s32vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (s32vector-ref vec i) r)))))

(define (reverse-list->s32vector list)
  (let* ((len (length list))
         (r (make-s32vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (s32vector-set! r (- len i 1) (car list))
          (loop (+ i 1) (cdr list)))))))

(define s32vector->vector
  (case-lambda
    ((vec) (s32vector->vector* vec 0 (s32vector-length vec)))
    ((vec start) (s32vector->vector* vec start (s32vector-length vec)))
    ((vec start end) (s32vector->vector* vec start end))))

(define (s32vector->vector* vec start end)
  (let* ((len (- end start))
         (r (make-vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (vector-set! r o (s32vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define vector->s32vector
  (case-lambda
    ((vec) (vector->s32vector* vec 0 (vector-length vec)))
    ((vec start) (vector->s32vector* vec start (vector-length vec)))
    ((vec start end) (vector->s32vector* vec start end))))

(define (vector->s32vector* vec start end)
  (let* ((len (- end start))
         (r (make-s32vector len)))
    (let loop ((i start) (o 0))
      (cond
        ((= i end) r)
        (else
          (s32vector-set! r o (vector-ref vec i))
          (loop (+ i 1) (+ o 1)))))))

(define make-s32vector-generator
  (case-lambda ((vec) (make-s32vector-generator vec 0 (s32vector-length vec)))
               ((vec start) (make-s32vector-generator vec start (s32vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (s32vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define write-s32vector
  (case-lambda
    ((vec) (write-s32vector* vec (current-output-port)))
    ((vec port) (write-s32vector* vec port))))


(define (write-s32vector* vec port)
  (display "#s32(" port)  ; s32-expansion is blind, so will expand this too
  (let ((last (- (s32vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (s32vector-ref vec i) port)
         (display ")" port))
        (else
          (write (s32vector-ref vec i) port)
          (display " " port)
          (loop (+ i 1)))))))

(define (s32vector< vec1 vec2)
  (let ((len1 (s32vector-length vec1))
        (len2 (s32vector-length vec2)))
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
           ((< (s32vector-ref vec1 i) (s32vector-ref vec2 i))
            #t)
           ((> (s32vector-ref vec1 i) (s32vector-ref vec2 i))
            #f)
           (else
             (loop (+ i 1)))))))))

(define (s32vector-hash vec)
  (let ((len (min 256 (s32vector-length vec))))
    (let loop ((i 0) (r 0))
      (if (= i len)
        (abs (floor (real-part (inexact->exact r))))
        (loop (+ i 1) (+ r (s32vector-ref vec i)))))))

(define s32vector-comparator
  (make-comparator s32vector? s32vector= s32vector< s32vector-hash))
