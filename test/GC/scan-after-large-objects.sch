;;; Scanning to-space after forwarding large objects (generational
;;; collector).
;;;
;;; When a collection forwards a large object (more than a page), which
;;; is not copied, and then another, to-space grows by a chunk that
;;; stays empty.  The scan loop used to stop when the scan pointer
;;; reached the end of one such chunk if the next object would be copied
;;; to the start of another that happened to follow it in memory,
;;; without scanning the chunks in between; their objects kept pointers
;;; to from-space (seen as stack frames returning to garbage, "WEIRD
;;; OBJECT"s, and a failed assertion in los.c).
;;;
;;; This program, from the vecsort benchmark (merge sorts of 1.1 million
;;; characters) run under the profiler, which flushes the stack at timer
;;; interrupts, crashed every time with the default collector.  Run it
;;; with
;;;
;;;     larceny < scan-after-large-objects.sch
;;;
;;; It prints "scan-after-large-objects: ok" (in about a minute).

(require 'profile)

(define (my-merge-sort! v tmp lo hi less?)

  (define (insertion-sort! lo hi)
    (do ((i (+ lo 1) (+ i 1)))
        ((>= i hi))
      (let ((x (vector-ref v i)))
        (let loop ((j i))
          (if (and (> j lo) (less? x (vector-ref v (- j 1))))
              (begin (vector-set! v j (vector-ref v (- j 1)))
                     (loop (- j 1)))
              (vector-set! v j x))))))

  ; Merges v[lo,mid) and v[mid,hi), both sorted, into v[lo,hi).

  (define (merge! lo mid hi)
    (let ((n (- mid lo)))
      (do ((i 0 (+ i 1)))
          ((= i n))
        (vector-set! tmp i (vector-ref v (+ lo i))))
      (let loop ((i 0) (j mid) (k lo))
        (cond ((= i n))                  ; the rest of the right half stays
              ((= j hi)
               (do ((i i (+ i 1))
                    (k k (+ k 1)))
                   ((= i n))
                 (vector-set! v k (vector-ref tmp i))))
              ((less? (vector-ref v j) (vector-ref tmp i))
               (vector-set! v k (vector-ref v j))
               (loop i (+ j 1) (+ k 1)))
              (else
               (vector-set! v k (vector-ref tmp i))
               (loop (+ i 1) j (+ k 1)))))))

  (define (sort! lo hi)
    (if (<= (- hi lo) 1)
        (insertion-sort! lo hi)
        (let ((mid (+ lo (quotient (- hi lo) 2))))
          (sort! lo mid)
          (sort! mid hi)
          (if (less? (vector-ref v mid) (vector-ref v (- mid 1)))
              (merge! lo mid hi)))))

  (sort! lo hi))

; Sorts the elements of v from start to end in place.

(define (my-sort-range! less? v start end)
  (let ((n (- end start)))
    (if (> n 1)
        (my-merge-sort! v (make-vector (quotient (+ n 1) 2)) start end
                                 less?))))



(define (my-vsort less? seq)
  (let ((v (vector-copy seq)))
    (my-sort-range! less? v 0 (vector-length v))
    v))

(define (all-characters)
  (define (loop sv chars)
    (cond ((< sv 0) chars)
          ((<= #xd800 sv #xdfff) (loop #xd7ff chars))
          (else (loop (- sv 1) (cons (integer->char sv) chars)))))
  (list->vector (loop #x10ffff '())))

(define (hash<? c0 c1)
  (define (hash c)
    (let ((sv (char->integer c)))
      (fx- sv (fxxor sv (fxarithmetic-shift-right sv 2)))))
  (fx<? (hash c0) (hash c1)))

(define chars (all-characters))

(define (sorted? v less?)
  (let loop ((i 1))
    (or (>= i (vector-length v))
        (and (not (less? (vector-ref v i) (vector-ref v (- i 1))))
             (loop (+ i 1))))))

(define result
  (profile
   (let loop ((i 0) (r #f))
     (if (= i 6)
         r
         (loop (+ i 1) (my-vsort char<? (my-vsort hash<? chars)))))))

(display (if (and (sorted? result char<?)
                  (equal? result chars))
             "scan-after-large-objects: ok"
             "scan-after-large-objects: FAILED"))
(newline)
(exit)
