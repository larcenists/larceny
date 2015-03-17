;;; The representation-dependent part of an implementation of
;;; character spans that represents character spans by records
;;; encapsulating a string and substring bounds and represents
;;; cursors by exact integers.
;;;
;;; This implementation is suitable for systems in which
;;; string-ref runs in constant time.

(define (%debugging) #f)

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

(define (%span:empty) %the-empty-span)

(define %the-empty-span
  (%make-raw-span "" 0 0))

;;; The main advantage of this representation is that subspans
;;; can share the same base string, which can make the subspan
;;; operation faster and more space-efficient.  On the other
;;; hand, just one tiny subspan can retain the storage of an
;;; enormous base string.  To eliminate that possibility, this
;;; implementation of the subspan operation creates a new base
;;; string whenever:
;;;
;;;     the length of the subspan is less than a threshold
;;;     the length of the subspan is less than a certain fraction
;;;         of the length of the base string

(define %subspan:absolute-threshold 20)
(define %subspan:threshold-multiplier 10)

(define (%subspan sp start end)
  (%check-span sp 'subspan)
  (let ((s (%span:string sp))
        (i (%span:start sp))
        (j (%span:end sp)))
    (if (and (exact-integer? start)
             (exact-integer? end)
             (<= 0 start end (- j i)))
        (cond ((and (= start 0)
                    (= end (- j i)))
               sp)
              ((or (< (- end start) %subspan:absolute-threshold 20)
                   (< (* (- end start) %subspan:threshold-multiplier)
                      (string-length s)))
               (%make-raw-span (substring s (+ i start) (+ i end))
                               0
                               (- end start)))
              (else
               (%make-raw-span s (+ i start) (+ i end))))
        (%error 'span-ref
                "index out of range"
                sp
                start
                end))))

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
  (cond ((string=? str "")
         (%span:empty))
        ((%debugging)
         (%make-raw-span (string-append "***" str "&&&&&")
                         3
                         (+ 3 (string-length str))))
        (else
         (%make-raw-span str 0 (string-length str)))))

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

;;;

(define (string-cursor-ref str curs)
  (string-ref str (string-cursor->index str curs)))

(define (span-cursor-ref sp curs)
  (span-ref sp (span-cursor->index sp curs)))

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
  (%check-span sp 'span-cursor-forward-until)
  (let* ((s  (%span:string sp))
         (i0 (%span:start sp))
         (j0 (%span:end sp))
         (n  (- j0 i0)))
    (let loop ((i (span-cursor->index sp curs)))
      (cond ((>= i n)
             (span-cursor-end sp))
            ((and (<= 0 i)
                  (pred (string-ref s (+ i i0))))
             (span-index->cursor sp i))
            (else
             (loop (+ i 1)))))))

(define (string-cursor-backward-until str curs pred)
  (let ((n (string-length str)))
    (let loop ((i (string-cursor->index str curs)))
      (cond ((< i 0)
             (string-cursor-prev str (string-cursor-start str)))
            ((and (< i n)
                  (pred (string-ref str i)))
             (string-index->cursor str i))
            (else
             (loop (- i 1)))))))

(define (span-cursor-backward-until sp curs pred)
  (%check-span sp 'span-cursor-forward-until)
  (let* ((s  (%span:string sp))
         (i0 (%span:start sp))
         (j0 (%span:end sp))
         (n  (- j0 i0)))
    (let loop ((i (span-cursor->index sp curs)))
      (cond ((< i 0)
             (span-cursor-prev sp (span-cursor-start sp)))
            ((and (< i n)
                  (pred (string-ref s (+ i i0))))
             (span-index->cursor sp i))
            (else
             (loop (- i 1)))))))

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

(define (span-null? sp)
  (= 0 (span-length sp)))

;;; Selection.

(define (span-ref sp k)
  (%check-span sp 'span-ref)
  (let ((s (%span:string sp))
        (i (%span:start sp))
        (j (%span:end sp)))
    (if (and (exact-integer? k)
             (<= 0 k)
             (< k (- j i)))
        (string-ref s (+ k i))
        (%error 'span-ref
                "index out of range"
                sp
                k))))

(define (subspan sp start end)
  (%subspan sp start end))

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

;;; Searching.
;;;
;;; These algorithms are representation-dependent because we might want
;;; its representation of tables to depend upon the alphabet size etc,
;;; and because we might want to search a span's underlying string or
;;; bytevector.

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

;;; Boyer-Moore string search.
;;;
;;; Translated into Scheme by starting with the (buggy) Java code in
;;; http://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm
;;; as of 16 March 2015 and then debugging from first principles.
;;;
;;; (I think the Java code was buggy, but I might have introduced
;;; the bugs myself during the translation.)
;;;
;;; Precondition: needle is non-empty.

(define (%span-contains:boyer-moore haystack needle)
  (let* ((s0 (%span:string haystack))
         (i0 (%span:start haystack))
         (j0 (%span:end haystack))
         (n0 (- j0 i0))
         (s1 (%span:string needle))
         (i1 (%span:start needle))
         (j1 (%span:end needle))
         (n1 (- j1 i1)))

    ;; Returns the bad character table, which maps characters that
    ;; occur within the needle to the distance between the rightmost
    ;; occurrence of that character within the needle and the last
    ;; character of the needle.
    ;; If the rightmost character of the needle does not match a
    ;; character c within the haystack, then the bad character table
    ;; entry for c is the appropriate shift.
    ;; If the rightmost character of the needle does match c, but
    ;; a mismatch occurs somewhere to the left, then the bad character
    ;; table entry for c is a lower bound for the appropriate shift.

    (define (makeCharTable)
      (let ((table (make-hash-table char=? char->integer)))
        (do ((i 0 (+ i 1)))
            ((>= (+ i 1) n1)
             table)
          (hash-table-set! table
                           (string-ref s1 (+ i i1))
                           (- n1 1 i)))))

    ;; Returns the good suffix table, which maps needle positions
    ;; where the first mismatch occurs (to the left of at least one
    ;; matching character) to a safe shift.
    ;;
    ;; That safe shift is defined as follows:
    ;;
    ;;     If the matching suffix occurs as a substring of the needle
    ;;     somewhere to the left, and the character to the left of
    ;;     that substring differs from the character to the left of
    ;;     the matching suffix, then the distance between the
    ;;     rightmost substring with that property and the matching
    ;;     suffix is a safe shift.
    ;;
    ;;     If the matching suffix does not occur as a substring
    ;;     of the needle somewhere to the left with a different
    ;;     character preceding, then find the longest prefix of
    ;;     the needle that matches a suffix of the matching
    ;;     suffix.  If that suffix exists, then its index (which
    ;;     is the length of the needle minus the length of the
    ;;     prefix) is a safe shift.
    ;;
    ;;     Otherwise it's safe to shift by the length of the needle.

    (define (makeOffsetTable)
      (let ((table (make-vector (- n1 1) -1)))

        ;; i is the leftmost index of the matching suffix.
        ;; By increasing i, longest matches will be found first.

        (do ((i 1 (+ i 1)))
            ((>= (+ i 1) n1))
          (if (span-prefix? (subspan needle i n1) ; matching suffix
                            needle)
              (vector-set! table i i)))

        ;; The loop above finds the longest prefix of the needle
        ;; that matches the matching suffix.  The following loop
        ;; fills that in, effectively computing the longest prefix
        ;; that matches a suffix of the matching suffix.

        (let loop ((prev n1)
                   (i (- n1 2)))
          (if (< 0 i)
              (let ((jump (vector-ref table i)))
                (if (< jump 0)
                    (begin (vector-set! table i prev)
                           (loop prev (- i 1)))
                    (loop jump (- i 1))))))

        ;; By increasing i, matches to the right will replace matches
        ;; to the left.
        ;;
        ;; FIXME: this loop is O(n1^2)

        (do ((i 1 (+ i 1)))
            ((>= (+ i 1) n1))
          (let ((slen (span-suffix-length (subspan needle 0 i)
                                          needle)))
            (if (< 0 slen i)
                (vector-set! table
                             (- n1 slen 1) ; index of first mismatch
                             (- n1 i)))))

        table))

    (let ((charTable (makeCharTable))
          (offsetTable (makeOffsetTable)))

      (if (%debugging)
          (begin
           (for-each (lambda (entry)
                       (write-string "#\\")
                       (write-char (car entry))
                       (write-string " ")
                       (write-string (number->string (cdr entry)))
                       (newline))
                     (hash-table->alist charTable))
           (newline)
           (do ((i 0 (+ i 1)))
               ((= i (vector-length offsetTable)))
             (write-string (number->string i))
             (write-string " ")
             (write-string (number->string (vector-ref offsetTable i)))
             (newline))
           (newline)))

      ;; Returns the least i greater than or equal to the given i
      ;; at which a match is found.

      (let loop1 ((i 0))

        ;; i is an index into s0 (haystack), j an index into s1 (needle).
        ;; Returns the j at which a mismatch occurs, or -1 for a match.

        (define (loop2 i j)
          (cond ((< j 0) j)
                ((char=? (string-ref s1 (+ j i1))
                         (string-ref s0 (+ i i0)))
                 (loop2 (- i 1) (- j 1)))
                (else j)))

        (if (> (+ i n1) n0)
            #f
            (let ((j (loop2 (- (+ i n1) 1) (- n1 1)))
                  (jumpA (hash-table-ref/default charTable
                                                 (string-ref s0 (+ i n1 -1 i0))
                                                 n1)))
              (cond ((< j 0)
                     (span-index->cursor haystack i))
                    ((= j (- n1 1))
                     (loop1 (+ i jumpA)))
                    (else
                     (let ((jumpB (vector-ref offsetTable j)))
                       (if (%debugging)
                           (begin
                            (write-string (number->string i))
                            (write-string " ")
                            (write-string (number->string j))
                            (write-string " ")
                            (write-string (number->string jumpA))
                            (write-string " ")
                            (write-string (number->string jumpB))
                            (newline)))
                       (loop1 (+ i (max jumpA jumpB))))))))))))

;;; The whole character span or string.

(define (span-length sp)
  (- (%span:end sp) (%span:start sp)))

;;; Conversion.

(define (string->span str)
  (%string->span str))

(define (span->string sp)
  (%span->string sp))

; eof
