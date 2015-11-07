;;; Copyright (C) William D Clinger 2015. All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; The representation-dependent part of an implementation of
;;; character spans that represents character spans by records
;;; encapsulating a UTF-8 representation (as a bytevector) along
;;; with a vector mapping character indexes to bytevector indexes,
;;; the character length of the span (not the bytevector), and
;;; substring bounds (as bytevector indexes).  Cursors are exact
;;; integers.
;;;
;;; This implementation is suitable for systems that support bytevectors,
;;; Unicode characters, and Unicode strings.  It provides O(1) indexing
;;; of spans even if string-ref does not run in constant time.

(define (%debugging) #f)

;;; A :span represents the characters in (bytevector-copy bv i j).
;;;
;;; bv is the UTF-8 representation of a string.
;;; i and j are indexes into bv on UTF-8 character boundaries.
;;; n is (string-length (utf8->string (bytevector-copy bv i j))).
;;; char-indexes is a non-empty vector that maps selected character
;;; indexes to bytevector indexes.  If 0 <= k < n, then
;;;
;;;     (vector-ref char-indexes
;;;                 (div k %char-indexes:resolution))
;;;
;;; is the bytevector index of the character that would be obtained by
;;;
;;;     (string-ref (utf8->string bv)
;;;                 (* %char-indexes:resolution
;;;                    (div k %char-indexes:resolution)))
;;;
;;; That index allows the kth character of a span to be found by
;;; scanning at most %char-indexes:resolution characters of the
;;; UTF-8 representation.  Since %char-indexes:resolution is a
;;; constant, span indexing runs in O(1) time.
;;;
;;; If %char-indexes:resolution is 32, then the space overhead is
;;; roughly 25% for 64-bit systems and about 12.5% for 32-bit systems.
;;; Subspans share the char-indexes vector as well as the UTF-8
;;; representation, so the relative space overhead of indexing goes
;;; down when subspans are created.

(define %char-indexes:resolution 32)

(define-record-type :span
  (%make-raw-span bv i j n char-indexes)
  %span?
  (bv %span:bytevector)
  (i %span:start)
  (j %span:end)
  (n %span:length)
  (char-indexes %span:char-indexes))

;;; Given a span and a character index into the base bytevector,
;;; returns the corresponding bytevector index.

(define (%char-index->bytevector-index sp idx)
  (let* ((bv (%span:bytevector sp))
         (char-indexes (%span:char-indexes sp))
         (k (div idx %char-indexes:resolution)))
    (let loop ((i (vector-ref char-indexes k))
               (idx2 (* k %char-indexes:resolution)))
      (if (< idx2 idx)
          (loop (%utf8-next-index bv i)
                (+ idx2 1))
          i))))

;;; Given a span and an index into its base bytevector,
;;; returns the corresponding character index into the base bytevector.

(define (%bytevector-index->char-index sp i)
  (let* ((bv (%span:bytevector sp))
         (char-indexes (%span:char-indexes sp)))
    (let loop1 ((k (div i %char-indexes:resolution)))
      (if (or (<= (vector-length char-indexes) k)
              (< i (vector-ref char-indexes k)))
          (loop1 (- k 1))
          (let loop2 ((j (vector-ref char-indexes k))
                      (idx (* k %char-indexes:resolution)))
            (if (< j i)
                (loop2 (%utf8-next-index bv j)
                       (+ idx 1))
                idx))))))

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

;;; Runs in constant time, returning a lower bound for the true length.
;;; (With this representation, the estimate is exact.)

(define (%span-length:estimated span)
  (%span:length span))

;;; Returns an empty span.

(define (%span:empty) %the-empty-span)

(define %the-empty-span
  (%make-raw-span '#u8() 0 0 0 '#(0)))

;;; The main advantage of this representation is that subspans can
;;; share the same UTF-8 representation and indexing vector, which
;;; can make the subspan operation faster and more space-efficient.
;;; On the other hand, just one tiny subspan can retain the storage
;;; of an enormous UTF-8 bytevector.  To eliminate that possibility,
;;; this implementation of the subspan operation creates a new UTF-8
;;; bytevector whenever:
;;;
;;;     the length of the subspan is less than a certain fraction
;;;         of the length of the original base span

(define %subspan:threshold-multiplier 15)

(define (%subspan/cursors sp curs:start curs:end)
  (%check-span sp 'subspan/cursors)
  (let ((bv (%span:bytevector sp))
        (i0 (%span:start sp))
        (j0 (%span:end sp))
        (n  (%span:length sp))
        (i:start (- curs:start *cursor-offset*))
        (j:end (- curs:end *cursor-offset*)))
    (if (and (exact-integer? i:start)
             (exact-integer? j:end)
             (<= 0 i:start j:end (- j0 i0)))
        (cond ((and (= i:start 0)
                    (= j:end (- j0 i0)))
               sp)
              ((< (* (- j:end i:start) %subspan:threshold-multiplier)
                  (bytevector-length bv))
               (%utf8->span
                (bytevector-copy bv (+ i0 i:start) (+ i0 j:end))))
              (else
               (%make-raw-span bv
                               (+ i0 i:start)
                               (+ i0 j:end)
                               (- (span-cursor->index sp curs:end)
                                  (span-cursor->index sp curs:start))
                               (%span:char-indexes sp))))
        (%error 'subspan/cursors
                "cursor out of range"
                sp
                curs:start
                curs:end))))

(define (%subspan sp start end)
  (%check-span sp 'subspan)
  (%subspan/cursors sp
                    (span-index->cursor sp start)
                    (span-index->cursor sp end)))

;;; Nothing in the specification of span cursors precludes their
;;; representation as exact integers.  To provide a small degree
;;; of sanity checking, this implementation adds a fixed offset
;;; so span cursors will be less likely to be valid indexes into
;;; a related string.

(define *cursor-offset* -1000)

;;; Using a different offset for string cursors improves sanity
;;; checking.

(define *string-cursor-offset* -54321)

;;; Given a UTF-8 representation that is not accessible outside
;;; of this library, returns a span of its characters.

(define (%utf8->span bv)
  (call-with-values
   (lambda () (%utf8->char-indexes bv))
   (lambda (char-indexes n)
     (%make-raw-span bv 0 (bytevector-length bv) n char-indexes))))

;;; Given a UTF-8 representation that is not accessible outside
;;; of this library, returns two values:
;;;
;;;     the char-indexes vector for the bytevector
;;;     the number of characters in the bytevector

(define (%utf8->char-indexes bv)
  (if (zero? (bytevector-length bv))
      (values '#() 0)
      (let loop ((i 0)           ; index into bv
                 (n 0)           ; number of characters to left of i
                 (indexes '()))  ; prefix of char-indexes for first n chars
        (let* ((indexes (if (zero? (mod n 32))
                            (cons i indexes)
                            indexes))
               (j (%utf8-next-index bv i)))
          (cond (j
                 (loop j (+ n 1) indexes))
                (else
                 (values (list->vector (reverse indexes))
                         (+ n 1))))))))

;;; Given a UTF-8 bytevector and an index into that bytevector or -1,
;;; returns the least larger index that begins a character, or
;;; returns false if there are no more characters.

(define (%utf8-next-index bv i)
  (let ((n (bytevector-length bv))
        (i (+ i 1)))
    (if (>= i n)
        #f
        (let ((bits (bytevector-u8-ref bv i)))
          (cond ((< bits #b10000000) i)
                ((< bits #b11000000) (%utf8-next-index bv i))
                (else i))))))

;;; Given a UTF-8 bytevector and an index into that bytevector
;;; or its length or -1,
;;; returns the greatest smaller index that begins a character, or
;;; returns false if there are no more characters.

(define (%utf8-prev-index bv i)
  (let ((i (- i 1)))
    (if (< i 0)
        #f
        (let ((bits (bytevector-u8-ref bv i)))
          (cond ((< bits #b10000000) i)
                ((< bits #b11000000) (%utf8-prev-index bv i))
                (else i))))))

;;; Given a UTF-8 bytevector and an index into that bytevector
;;; at the start of a character, returns that character.

(define (%utf8-char-ref bv i)
  (let ((bits (bytevector-u8-ref bv i)))
    (cond ((< bits #b10000000)
           (integer->char bits))
          ((< bits #b11000000)
           (%error '%utf8-char-ref
                   "index not on character boundary"
                   bv i))
          ((< bits #b11100000)
           (integer->char
            (+ (* 64 (- bits #b11000000))
               (- (bytevector-u8-ref bv (+ i 1)) #b10000000))))
          ((< bits #b11110000)
           (integer->char
            (+ (* 4096 (- bits #b11100000))
               (*   64 (- (bytevector-u8-ref bv (+ i 1)) #b10000000))
               (- (bytevector-u8-ref bv (+ i 2)) #b10000000))))
          (else
           (integer->char
            (+ (* 262144 (- bits #b11110000))
               (*   4096 (- (bytevector-u8-ref bv (+ i 1)) #b10000000))
               (*     64 (- (bytevector-u8-ref bv (+ i 2)) #b10000000))
               (- (bytevector-u8-ref bv (+ i 3)) #b10000000)))))))

;;; Procedures that aren't exported but may be called by the
;;; representation-independent part of the implementation.

(define (%string->span str)
  (cond ((string=? str "")
         (%span:empty))
        ((%debugging)
         (%subspan (%utf8->span
                    (string->utf8 (string-append "***" str "&&&&&")))
                   3
                   (+ 3 (string-length str))))
        (else
         (%utf8->span (string->utf8 str)))))

(define (%span->string sp)
  (%check-span sp '%span->string)
  (utf8->string (%span:bytevector sp)
                (%span:start sp)
                (%span:end sp)))

(define (%check-span sp name)
  (if (not (%span? sp))
      (%error name
              "not a span"
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
  (+ (- (%span:end sp)
        (%span:start sp))
     *cursor-offset*))

(define (string-cursor-next str curs)
  (min (string-cursor-end str) (+ curs 1)))

(define (span-cursor-next sp curs)
  (let* ((bv (%span:bytevector sp))
         (i0 (%span:start sp))
         (j0 (%span:end sp))
         (i  (- curs *cursor-offset*))
         (i  (%utf8-next-index bv (+ i i0))))
    (cond ((or (not i) (>= i j0))
           (+ (- j0 i0) *cursor-offset*))
          (else
           (+ (- i i0) *cursor-offset*)))))

(define (string-cursor-prev str curs)
  (max (+ -1 *string-cursor-offset*) (- curs 1)))

(define (span-cursor-prev sp curs)
  (let* ((bv (%span:bytevector sp))
         (i0 (%span:start sp))
         (j0 (%span:end sp))
         (i  (- curs *cursor-offset*))
         (i  (%utf8-prev-index bv (+ i i0))))
    (cond ((or (not i) (< i i0))
           (+ -1 *cursor-offset*))
          (else
           (+ (- i i0) *cursor-offset*)))))

(define (string-cursor-forward str curs n)
  (min (string-cursor-end str) (+ curs n)))

(define (span-cursor-forward sp curs n)
  (cond ((= n 0)
         curs)
        ((< n %char-indexes:resolution)
         (span-cursor-forward sp (span-cursor-next sp curs) (- n 1)))
        (else
         (span-index->cursor sp
                             (+ (span-cursor->index sp curs) n)))))

(define (string-cursor-backward str curs n)
  (max (+ -1 *string-cursor-offset*) (- curs n)))

(define (span-cursor-backward sp curs n)
  (cond ((= n 0)
         curs)
        ((< n %char-indexes:resolution)
         (span-cursor-backward sp (span-cursor-prev sp curs) (- n 1)))
        (else
         (span-index->cursor sp
                             (- (span-cursor->index sp curs) n)))))

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
  (let* ((i0 (%span:start sp))
         (j0 (%span:end sp))
         (i1 (+ i0 (- curs *cursor-offset*)))
         (idx0 (%bytevector-index->char-index sp i0)))
    (cond ((< i1 i0)
           -1)
          ((<= j0 i1)
           (%span:length sp))
          (else
           (let ((idx1 (%bytevector-index->char-index sp i1)))
             (- idx1 idx0))))))

(define (string-index->cursor str idx)
  (+ idx *string-cursor-offset*))

(define (span-index->cursor sp idx)
  (let* ((i0 (%span:start sp))
         (n  (%span:length sp))
         (idx0 (%bytevector-index->char-index sp i0)))
    (cond ((< idx 0)
           (span-cursor-prev sp (span-cursor-start sp)))
          ((>= idx n)
           (span-cursor-end sp))
          (else
           (+ (- (%char-index->bytevector-index sp (+ idx idx0))
                 i0)
              *cursor-offset*)))))

(define (string-cursor-difference str curs1 curs2)
  (- curs2 curs1))

(define (span-cursor-difference sp curs1 curs2)
  (- curs2 curs1))

;;;

(define (string-cursor-ref str curs)
  (string-ref str (string-cursor->index str curs)))

(define (span-cursor-ref sp curs)
  (%check-span sp 'span-cursor-ref)
  (let ((bv (%span:bytevector sp))
        (i0 (%span:start sp))
        (j0 (%span:end sp))
        (icurs (- curs *cursor-offset*)))
    (if (and (<= 0 icurs)
             (< icurs (- j0 i0)))
        (%utf8-char-ref bv (+ icurs i0))
        (%error 'span-cursor-ref
                "span cursor out of range"
                sp curs))))

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
  (span-cursor-ref sp (span-index->cursor sp k)))

(define (subspan sp start end)
  (%subspan sp start end))

(define (subspan/cursors sp start end)
  (%subspan/cursors sp start end))

;;; Padding, trimming, and compressing.

(define (%space? x)
  (eqv? x #\space))

(define (%span-trim-predicate rest)
  (if (null? rest)
      %space?
      (car rest)))

(define (span-trim sp . rest)
  (let* ((pred (%span-trim-predicate rest))
         (bv (%span:bytevector sp))
         (i0 (%span:start sp))
         (j0 (%span:end sp)))
    (let loop ((k i0))
      (cond ((or (not k)
                 (>= k j0))
             (%span:empty))
            ((pred (%utf8-char-ref bv k))
             (loop (%utf8-next-index bv k)))
            (else
             (subspan/cursors sp
                              (+ (- k i0) *cursor-offset*)
                              (+ (- j0 i0) *cursor-offset*)))))))

(define (span-trim-right sp . rest)
  (let* ((pred (%span-trim-predicate rest))
         (bv (%span:bytevector sp))
         (i0 (%span:start sp))
         (j0 (%span:end sp)))
    (let loop ((k (%utf8-prev-index bv j0)))
      (cond ((or (not k)
                 (< k i0))
             (%span:empty))
            ((pred (%utf8-char-ref bv k))
             (loop (%utf8-prev-index bv k)))
            (else
             (let ((j0 (or (%utf8-next-index bv k)
                           j0)))
               (subspan/cursors sp
                                (+ 0 *cursor-offset*)
                                (+ (- j0 i0) *cursor-offset*))))))))

;;; Searching.
;;;
;;; These algorithms are representation-dependent because we might want
;;; to search a span's underlying string or bytevector.

;;; Precondition: needle is non-empty.

(define (%span-contains:naive haystack needle)
  (let* ((bv0 (%span:bytevector haystack))
         (i0 (%span:start haystack))
         (j0 (%span:end haystack))
         (n0 (- j0 i0))
         (bv1 (%span:bytevector needle))
         (i1 (%span:start needle))
         (j1 (%span:end needle))
         (n1 (- j1 i1))
         (end (- j0 n1)))

    (define (at? k0 k1)
      (cond ((= k1 j1)
             #t)
            ((= (bytevector-u8-ref bv0 k0)
                (bytevector-u8-ref bv1 k1))
             (at? (+ 1 k0)
                  (+ 1 k1)))
            (else
             #f)))

    (let loop ((i i0))
      (cond ((> i end)
             #f)
            (else
             (if (at? i i1)
                 (+ (- i i0) *cursor-offset*)
                 (loop (+ 1 i))))))))

(define (%span-contains:rabin-karp haystack needle)
  (let* ((bv0 (%span:bytevector haystack))
         (i0 (%span:start haystack))
         (j0 (%span:end haystack))
         (n0 (- j0 i0))
         (bv1 (%span:bytevector needle))
         (i1 (%span:start needle))
         (j1 (%span:end needle))
         (n1 (- j1 i1))
         (end (- j0 n1)))

    (define (hash bv i j)
      (do ((h 0 (+ h (bytevector-u8-ref bv i)))
           (i i (+ i 1)))
          ((= i j)
           h)))

    (define (at? k0 k1)
      (cond ((= k1 j1)
             #t)
            ((= (bytevector-u8-ref bv0 k0)
                (bytevector-u8-ref bv1 k1))
             (at? (+ 1 k0)
                  (+ 1 k1)))
            (else
             #f)))

    (cond ((< n0 n1)
           #f)
          (else
           (let loop ((i i0)
                      (h0 (hash bv0 i0 (+ i0 n1)))
                      (h1 (hash bv1 i1 j1)))
             (cond ((and (= h0 h1)
                         (at? i i1))
                    (+ (- i i0) *cursor-offset*))
                   ((>= i end)
                    #f)
                   (else
                    (loop (+ 1 i)
                          (+ (- (bytevector-u8-ref bv0 i))
                             h0
                             (bytevector-u8-ref bv0 (+ i n1)))
                          h1))))))))

;;; Boyer-Moore string search.
;;;
;;; Translated into Scheme by starting with the (buggy) Java code in
;;; http://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm
;;; as of 16 March 2015 and then debugging from first principles.
;;;
;;; Precondition: needle is non-empty.
;;;
;;; Benchmarking shows the simplified Boyer-Moore-Horspool algorithm
;;; is usually faster than the full Boyer-Moore algorithm as implemented
;;; here.

;;; FIXME: for reasons documented below, the full Boyer-Moore algorithm
;;; isn't yet implemented for this representation.

(define (full-boyer-moore?) #f)    ; FIXME: must always be #f; see above

(define (%span-contains:boyer-moore haystack needle)
  (let* ((bv0 (%span:bytevector haystack))
         (i0 (%span:start haystack))
         (j0 (%span:end haystack))
         (n0 (- j0 i0))
         (bv1 (%span:bytevector needle))
         (i1 (%span:start needle))
         (j1 (%span:end needle))
         (n1 (- j1 i1)))

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

    (define (original-makeOffsetTable)
      (let ((table (make-vector (- n1 1) -1)))

        ;; i is the leftmost index of the matching suffix.
        ;; By increasing i, longest matches will be found first.
        ;;
        ;; FIXME: the call to subspan/cursors may not be on a character
        ;; boundary, which violates an invariant of this representation.

        (do ((i 1 (+ i 1)))
            ((>= (+ i 1) n1))
          (if (span-prefix? (subspan/cursors needle
                                             (+ i *cursor-offset*)
                                             (+ n1 *cursor-offset*))
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
        ;;
        ;; FIXME: the call to subspan/cursors may not be on a character
        ;; boundary, which violates an invariant of this representation.
        ;;
        ;; FIXME: the table is filled with character indexes, not
        ;; bytevector indexes, so this doesn't work at all.

        (do ((i 1 (+ i 1)))
            ((>= (+ i 1) n1))
          (let ((slen (span-suffix-length
                       (subspan/cursors needle
                                        (+ 0 *cursor-offset*)
                                        (+ i *cursor-offset*))
                       needle)))
            (if (< 0 slen i)
                (vector-set! table
                             (- n1 slen 1) ; index of first mismatch
                             (- n1 i)))))

        table))

    ;; Computing the good suffix table as above takes too long.
    ;; The Boyer-Moore-Horspool algorithm doesn't use that table,
    ;; which is equivalent to using a table in which all entries are 1.

    (define (horspool-makeOffsetTable)
      '#(1))

    (define (makeOffsetTable)
      (if (full-boyer-moore?)
          (original-makeOffsetTable)
          (horspool-makeOffsetTable)))

    (let ((charTable (make-vector 256 n1))
          (offsetTable (makeOffsetTable)))

      ;; Initializes the bad character table, which maps characters that
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
        (do ((i 0 (+ i 1)))
            ((>= (+ i 1) n1))
          (let ((bits (bytevector-u8-ref bv1 (+ i i1)))
                (jump (- n1 1 i)))
            (vector-set! charTable bits jump))))

      (define (show-entry c jump)
        (write-string "#\\")
        (write-char c)
        (write-string " ")
        (write-string (number->string jump))
        (newline))

      (makeCharTable)

      (if (%debugging)
          (begin
           (do ((i 0 (+ i 1)))
               ((= i 128))
             (show-entry (integer->char i) (vector-ref charTable i)))
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

        ;; 0 <= i <= (- j0 i0)
        ;; -1 <= j < (- j1 i1)
        ;; (+ i i0) is an index into bv0 (haystack),
        ;; (+ j i1) an index into bv1 (needle).
        ;; Returns the j at which a mismatch occurs, or -1 for a match.

        (define (loop2 i j)
          (cond ((< j 0) j)
                ((= (bytevector-u8-ref bv1 (+ j i1))
                    (bytevector-u8-ref bv0 (+ i i0)))
                 (loop2 (- i 1) (- j 1)))
                (else j)))

        (if (> (+ i n1) n0)
            #f
            (let* ((j (loop2 (- (+ i n1) 1) (- n1 1)))
                   (bits (bytevector-u8-ref bv0 (+ i n1 -1 i0)))
                   (jumpA (vector-ref charTable bits)))
              (cond ((< j 0)
                     (+ i *cursor-offset*))
                    ((= j (- n1 1))
                     (loop1 (+ i jumpA)))
                    (else
                     (let ((jumpB (if (full-boyer-moore?)
                                      (vector-ref offsetTable j)
                                      1)))
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
                       (loop1 (+ i
                                 (if (full-boyer-moore?)
                                     (max jumpA jumpB)
                                     jumpA))))))))))))

;;; The whole character span or string.

(define (span-length sp)
  (%span:length sp))

;;; Conversion.

(define (string->span str)
  (%string->span str))

(define (span->string sp)
  (%span->string sp))

; eof
