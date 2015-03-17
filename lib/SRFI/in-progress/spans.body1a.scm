;;; The representation-dependent part of an implementation of
;;; character spans that represents character spans by strings
;;; and cursors by exact integers.
;;;
;;; This implementation might be suitable for systems that
;;; don't allow non-Ascii characters in strings, don't support
;;; bytevectors, and don't much care about performance.

;;; FIXME:  So far as I can see, nothing in the specification
;;; of character spans precludes their representation as strings.
;;; To provide a small degree of sanity checking, however, this
;;; implementation uses an uncommon Ascii character to tag the
;;; strings that represent character spans.

(define *span-tag*
  (integer->char 23))   ; ^W, End of Transmission Block

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

(define (%span? x)
  (and (string? x)
       (< 0 (string-length x))
       (char=? *span-tag* (string-ref x 0))))

(define (%string->span str)
  (list->string (cons *span-tag* (string->list str))))

(define (%span->string sp)
  (%check-span sp '%span->string)
  (substring sp 1 (string-length sp)))

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
  (%string->span str))

(define (make-span str start end)
  (%string->span (substring str start end)))

(define (span . chars)
  (%string->span (list->string chars)))

;;; Predicates.

(define (span? x)
  (%span? x))

(define (span-null? sp)
  (= 0 (span-length sp)))

;;; Selection.

(define (span-ref sp k)
  (string-ref (%span->string sp) k))

(define (subspan sp start end)
  (%string->span (substring (%span->string sp) start end)))

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
  (let ((pred (%span-trim-predicate rest)))
    (let loop ((chars (string->list (%span->string sp))))
      (cond ((null? chars)
             (%string->span ""))
            ((pred (car chars))
             (loop (cdr chars)))
            (else
             (list->span chars))))))

(define (span-trim-right sp . rest)
  (let ((pred (%span-trim-predicate rest)))
    (let loop ((chars (reverse (string->list (%span->string sp)))))
      (cond ((null? chars)
             (%string->span ""))
            ((pred (car chars))
             (loop (cdr chars)))
            (else
             (list->span (reverse chars)))))))

;;; Searching.

;;; Precondition: needle is non-empty.

(define (%span-contains:naive haystack needle)
  (let ((start2 (span-cursor-start needle))
        (end1 (span-cursor-end haystack))
        (end2 (span-cursor-end needle)))
    (define (at? curs1 curs2)
      (cond ((span-cursor=? needle curs2 end2)
             curs1)
            ((and (span-cursor<? haystack curs1 end1)
                  (char=? (span-cursor-ref haystack curs1)
                          (span-cursor-ref needle curs2)))
             (at? (span-cursor-next haystack curs1)
                  (span-cursor-next needle curs2)))
            (else
             #f)))
    (let loop ((curs (span-cursor-start haystack)))
      (cond ((span-cursor>=? haystack curs end1)
             #f)
            (else
             (if (at? curs start2)
                 curs
                 (loop (span-cursor-next haystack curs))))))))

;;; FIXME: not implemented for this representation

(define (%span-contains:boyer-moore haystack needle)
  (%span-contains:naive haystack needle))

;;; The whole character span or string.

(define (span-length sp)
  (string-length (%span->string sp)))

;;; Conversion.

(define (string->span str)
  (%string->span str))

(define (span->string sp)
  (%span->string sp))

; eof
