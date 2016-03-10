;;; Linear-time (average case) algorithms for:
;;;
;;; Selecting the kth smallest element from an unsorted vector.
;;; Selecting the kth and (k+1)st smallest elements from an unsorted vector.
;;; Selecting the median from an unsorted vector.

;;; Given
;;;     an irreflexive total order <?
;;;     a vector v
;;;     an index k
;;;     an index start
;;;     an index end
;;; with
;;;     0 <= k < (- end start)
;;;     0 <= start < end <= (vector-length v)
;;; returns
;;;     (vector-ref (vector-sort <? (vector-copy v start end)) k)
;;; but is usually faster than that.

(define (%vector-select <? v k start end)
  (assert (and 'vector-select
               (procedure? <?)
               (vector? v)
               (exact-integer? k)
               (exact-integer? start)
               (exact-integer? end)
               (<= 0 k (- end start 1))
               (<= 0 start end (vector-length v))))
  (%%vector-select <? v k start end))

(define (%%vector-select <? v k start end)
  (let ((size (- end start)))
    (cond ((= 1 size)
           (vector-ref v (+ k start)))
          ((= 2 size)
           (cond ((<? (vector-ref v start)
                      (vector-ref v (+ start 1)))
                  (vector-ref v (+ k start)))
                 (else
                  (vector-ref v (+ (- 1 k) start)))))
          (else
           (let* ((ip (random-integer size))
                  (pivot (vector-ref v (+ start ip)))
                  (count (count-smaller <? pivot v start end 0)))
             (if (< k count)
                 (let* ((n count)
                        (v2 (make-vector n)))
                   (copy-smaller! <? pivot v2 0 v start end)
                   (%%vector-select <? v2 k 0 n))
                 (let* ((n (- size count))
                        (v2 (make-vector n)))
                   (copy-bigger! <? pivot v2 0 v start end)
                   (%%vector-select <? v2 (- k count) 0 n))))))))

;;; Counts how many elements within the range are less than the pivot.

(define (count-smaller <? pivot v i end count)
  (cond ((= i end) count)
        ((<? (vector-ref v i) pivot)
         (count-smaller <? pivot v (+ i 1) end (+ count 1)))
        (else
         (count-smaller <? pivot v (+ i 1) end count))))

;;; Like vector-copy! but copies an element only if it is less than the pivot.
;;; The destination vector must be large enough.

(define (copy-smaller! <? pivot dst at src start end)
  (cond ((= start end) dst)
        ((<? (vector-ref src start) pivot)
         (vector-set! dst at (vector-ref src start))
         (copy-smaller! <? pivot dst (+ at 1) src (+ start 1) end))
        (else
         (copy-smaller! <? pivot dst at src (+ start 1) end))))

;;; Like copy-smaller! but copies only elements that are not less than the pivot.

(define (copy-bigger! <? pivot dst at src start end)
  (cond ((= start end) dst)
        ((<? (vector-ref src start) pivot)
         (copy-bigger! <? pivot dst at src (+ start 1) end))
        (else
         (vector-set! dst at (vector-ref src start))
         (copy-bigger! <? pivot dst (+ at 1) src (+ start 1) end))))

