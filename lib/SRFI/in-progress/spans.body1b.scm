;;; The representation-dependent part of an implementation of
;;; character spans that represents character spans by records
;;; encapsulating a string and substring bounds and represents
;;; cursors by exact integers.
;;;
;;; This implementation is suitable for systems in which
;;; string-ref runs in constant time.

;;; A :span represents the characters in (substring s i j).

(define-record-type :span
  (%make-raw-span s i j)
  %span?
  (s %span:string)
  (i %span:start)
  (j %span:end))

;;; As an aid to debugging in Larceny, this makes spans print as #!"...".

(cond-expand
 (larceny
  (begin
   (rtd-printer-set!
    :span
    (lambda (sp out)
      (write-char #\# out)
      (write-char #\! out)
      (write-char #\" out)
      (let* ((s (%span->string sp))
             (n (string-length s)))
        (do ((i 0 (+ i 1)))
            ((= i n))
          (write-char (string-ref s i) out)))
      (write-char #\" out)))))
 (else))

;;; Returns an empty span.

(define (%span:empty)
  (%make-raw-span "" 0 0))

;;; Nothing in the specification of span cursors precludes their
;;; representation as exact integers.  To provide a small degree
;;; of sanity checking, this implementation adds a fixed offset
;;; so span cursors will be less likely to be valid indexes into
;;; a related string.

(define *cursor-offset* -1000)

;;; Using a different offset for string cursors improves sanity
;;; checking.

(define *string-cursor-offset* -54321)

;;; Procedures that aren't exported but may be called by the
;;; representation-independent part of the implementation.

(define (%string->span str)
  (%make-raw-span str 0 (string-length str)))

(define (%span->string sp)
  (%check-span sp '%span->string)
  (substring (%span:string sp) (%span:start sp) (%span:end sp)))

(define (%check-span sp name)
  (if (not (%span? sp))
      (error (string-append (cond ((symbol? name)
                                   (string-append (symbol->string name) ": "))
                                  ((string? name)
                                   (string-append name ": "))
                                  (else ""))
                            "not a span")
             sp)))

;;; Exported procedures.

;;; String cursors.

(define (string-cursor-start str)
  (+ 0 *string-cursor-offset*))

(define (span-cursor-start sp)
  (+ 0 *cursor-offset*))

(define (string-cursor-end str)
  (+ (string-length str) *string-cursor-offset*))

(define (span-cursor-end sp)
  (+ (string-length (%span->string sp)) *cursor-offset*))

(define (string-cursor-ref str curs)
  (string-ref str (string-cursor->index str curs)))

;;; FIXME

(define (span-cursor-ref sp curs)
  (string-ref (%span->string sp) (span-cursor->index sp curs)))

(define (string-cursor-next str curs)
  (min (string-cursor-end str) (+ curs 1)))

(define (span-cursor-next sp curs)
  (min (span-cursor-end sp) (+ curs 1)))

(define (string-cursor-prev str curs)
  (max (+ -1 *string-cursor-offset*) (- curs 1)))

(define (span-cursor-prev sp curs)
  (max (+ -1 *cursor-offset*) (- curs 1)))

(define (string-cursor-forward str curs n)
  (min (string-cursor-end str) (+ curs n)))

(define (span-cursor-forward sp curs n)
  (min (span-cursor-end sp) (+ curs n)))

(define (string-cursor-backward str curs n)
  (max (+ -1 *string-cursor-offset*) (- curs n)))

(define (span-cursor-backward sp curs n)
  (max (+ -1 *cursor-offset*) (- curs n)))

(define (string-cursor-forward-until str curs pred)
  (let ((n (string-length str)))
    (let loop ((i (string-cursor->index str curs)))
      (cond ((>= i n)
             (string-cursor-end str))
            ((and (<= 0 i)
                  (pred (string-ref str i)))
             (string-index->cursor str i))
            (else
             (loop (+ i 1)))))))

(define (span-cursor-forward-until sp curs pred)
  (let* ((str (%span->string sp))
         (n (string-length str)))
    (let loop ((i (span-cursor->index sp curs)))
      (cond ((>= i n)
             (span-cursor-end sp))
            ((and (<= 0 i)
                  (pred (string-ref str i)))
             (span-index->cursor sp i))
            (else
             (loop (+ i 1)))))))

(define (string-cursor-backward-until str curs pred)
  (let ((n (string-length str)))
    (let loop ((i (string-cursor->index str curs)))
      (cond ((< i 0)
             (string-cursor-prev (string-cursor-start str)))
            ((and (< i n)
                  (pred (string-ref str i)))
             (string-index->cursor str i))
            (else
             (loop (- i 1)))))))

(define (span-cursor-backward-until sp curs pred)
  (let* ((str (%span->string sp))
         (n (string-length str)))
    (let loop ((i (span-cursor->index str curs)))
      (cond ((< i 0)
             (span-cursor-prev (span-cursor-start sp)))
            ((and (< i n)
                  (pred (string-ref str i)))
             (span-index->cursor sp i))
            (else
             (loop (- i 1)))))))

(define (string-cursor=? str curs1 curs2)
  (= curs1 curs2))

(define (span-cursor=? sp curs1 curs2)
  (= curs1 curs2))

(define (string-cursor<? str curs1 curs2)
  (< curs1 curs2))

(define (span-cursor<? sp curs1 curs2)
  (< curs1 curs2))

(define (string-cursor>? str curs1 curs2)
  (> curs1 curs2))

(define (span-cursor>? sp curs1 curs2)
  (> curs1 curs2))

(define (string-cursor<=? str curs1 curs2)
  (<= curs1 curs2))

(define (span-cursor<=? sp curs1 curs2)
  (<= curs1 curs2))

(define (string-cursor>=? str curs1 curs2)
  (>= curs1 curs2))

(define (span-cursor>=? sp curs1 curs2)
  (>= curs1 curs2))

(define (string-cursor->index str curs)
  (- curs *string-cursor-offset*))

(define (span-cursor->index sp curs)
  (- curs *cursor-offset*))

(define (string-index->cursor str idx)
  (+ idx *string-cursor-offset*))

(define (span-index->cursor sp idx)
  (+ idx *cursor-offset*))

(define (string-cursor-difference str curs1 curs2)
  (- curs1 curs2))

(define (span-cursor-difference sp curs1 curs2)
  (- curs1 curs2))

;;; Span constructors.

(define (make-whole-span str)
  (make-span str 0 (string-length str)))

(define (make-span str start end)
  (%string->span (substring str start end)))

(define (span . chars)
  (%string->span (list->string chars)))

;;; Predicates.

(define (span? x)
  (%span? x))

;;; Selection.

(define (span-ref sp k)
  (%check-span sp 'span-ref)
  (let ((s (%span:string sp))
        (i (%span:start sp))
        (j (%span:end sp)))
    (if (and (exact-integer? k)
             (<= 0 k)
             (< k (- j i)))
        (string-ref s (- k i))
        (%error 'span-ref
                "index out of range"
                sp
                k))))

(define (subspan sp start end)
  (%check-span sp 'subspan)
  (let ((s (%span:string sp))
        (i (%span:start sp))
        (j (%span:end sp)))
    (if (and (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (- j i)))
        (if (and (= start 0)
                 (= end (- j i)))
            sp
            (%make-raw-span s (+ i start) (+ i end)))
        (%error 'span-ref
                "index out of range"
                sp
                start
                end))))

(define (subspan/cursors sp start end)
  (subspan sp
           (span-cursor->index sp start)
           (span-cursor->index sp end)))

;;; Padding, trimming, and compressing.

(define (%space? x)
  (eqv? x #\space))

(define (%span-trim-predicate rest)
  (if (null? rest)
      %space?
      (car rest)))

(define (span-trim sp . rest)
  (let* ((pred (%span-trim-predicate rest))
         (s (%span:string sp))
         (i (%span:start sp))
         (j (%span:end sp))
         (n (- j i)))
    (let loop ((k 0))
      (cond ((= k n)
             (%span:empty))
            ((pred (string-ref s (+ k i)))
             (loop (+ k 1)))
            (else
             (subspan sp k n))))))

(define (span-trim-right sp . rest)
  (let* ((pred (%span-trim-predicate rest))
         (s (%span:string sp))
         (i (%span:start sp))
         (j (%span:end sp))
         (n (- j i)))
    (let loop ((k (- n 1)))
      (cond ((< k 0)
             (%span:empty))
            ((pred (string-ref s (+ k i)))
             (loop (- k 1)))
            (else
             (subspan sp 0 (+ k 1)))))))

;;; The whole character span or string.

(define (span-length sp)
  (- (%span:end sp) (%span:start sp)))

;;; Conversion.

(define (string->span str)
  (%string->span str))

(define (span->string sp)
  (%span->string sp))

; eof
