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

;;; Representation-independent part of a
;;; sample implementation of character spans.
;;;
;;; If the representation-dependent part is reasonably efficient,
;;; then this part should also be reasonably efficient.

(define (span-transform proc sp . rest)
  (string->span (apply proc (span->string sp) rest)))

(define (span-unfold stop? mapper successor . rest)
  (let ((seed (if (null? rest)
                  (if #f #f)      ; literally unspecified
                  (car rest)))
        (out (open-output-string)))
    (let loop ((x seed))
      (if (stop? x)
          (string->span (call-with-port out get-output-string))
          (begin (write-char (mapper x) out)
                 (loop (successor x)))))))

(define (span-unfold-right stop? mapper successor . rest)
  (let ((seed (if (null? rest)
                  (if #f #f)      ; literally unspecified
                  (car rest))))
    (let loop ((x seed)
               (chars '()))
      (if (stop? x)
          (string->span (list->string chars))
          (loop (successor x)
                (cons (mapper x) chars))))))

;;; FIXME: deprecated because of argument order.
;;; This implementation accepts either order.

(define (span-tabulate len proc)
  (if (and (exact-integer? proc)
           (procedure? len))
      (span-tabulate proc len)
      (do ((i 0 (+ i 1))
           (chars '() (cons (proc i) chars)))
          ((= i len)
           (string->span (list->string (reverse chars)))))))

;;; Predicates.

(define (span-every? pred sp)
  (let ((start (span-cursor-start sp))
        (end (span-cursor-end sp)))
    (let loop ((curs start))
      (cond ((span-cursor=? sp curs end)
             #t)
            ((pred (span-cursor-ref sp curs))
             (loop (span-cursor-next sp curs)))
            (else
             #f)))))

(define (span-any? pred sp)
  (let ((start (span-cursor-start sp))
        (end (span-cursor-end sp)))
    (let loop ((curs start))
      (cond ((span-cursor=? sp curs end)
             #f)
            ((pred (span-cursor-ref sp curs))
             #t)
            (else
             (loop (span-cursor-next sp curs)))))))

;;; Selection.

(define (span-take sp n)
  (subspan sp 0 n))

(define (span-take-right sp n)
  (let ((len (span-length sp)))
    (subspan sp (- len n) len)))

(define (span-drop sp n)
  (subspan sp n (span-length sp)))

(define (span-drop-right sp n)
  (subspan sp 0 (- (span-length sp) n)))

(define (span-split-at sp n)
  (values (span-take sp n)
          (span-drop sp n)))

(define (span-replicate sp from to)
  (let* ((n (- to from))
         (len (span-length sp)))
    (cond ((= n 0)
           (string->span ""))
          ((or (< n 0)
               (= len 0))
           (error "span-replicate: bad arguments" sp from to))
          (else
           (let* ((from (mod from len)) ; make from non-negative
                  (to (+ from n)))
             (do ((replicates '() (cons sp replicates))
                  (replicates-length 0 (+ replicates-length len)))
                 ((>= replicates-length to)
                  (subspan (span-concatenate replicates) from to))))))))

;;; Padding, trimming, and compressing.

(define (span-pad sp len . rest)
  (let ((char (if (null? rest) #\space (car rest)))
        (n (span-length sp)))
    (cond ((= len n)
           sp)
          ((< len n)
           (span-take-right sp len))
          (else
           (span-append (string->span (make-string (- len n) char))
                        sp)))))

(define (span-pad-right sp len . rest)
  (let ((char (if (null? rest) #\space (car rest)))
        (n (span-length sp)))
    (cond ((= len n)
           sp)
          ((< len n)
           (span-take sp len))
          (else
           (span-append sp
                        (string->span (make-string (- len n) char)))))))

(define (span-trim-both sp . rest)
  (apply span-trim-right (apply span-trim sp rest) rest))

(define (span-compress sp . rest)
  (let* ((char (if (null? rest) #\space (car rest)))
         (start (span-cursor-start sp))
         (end (span-cursor-end sp))
         (last (span-cursor-prev sp end))
         (out (open-output-string)))
    (let loop ((curs start)
               (changed? #f))
      (cond ((span-cursor>=? sp curs end)
             (if changed?
                 (string->span (call-with-port out get-output-string))
                 sp))
            ((and (span-cursor<? sp curs last)
                  (char=? char
                          (span-cursor-ref sp curs)
                          (span-cursor-ref sp (span-cursor-next sp curs))))
             (loop (span-cursor-next sp curs) #t))
            (else
             (write-char (span-cursor-ref sp curs) out)
             (loop (span-cursor-next sp curs) changed?))))))

;;; Prefixes and suffixes.

(define (span-prefix sp1 sp2)
  (let ((start1 (span-cursor-start sp1))
        (start2 (span-cursor-start sp2))
        (end1 (span-cursor-end sp1))
        (end2 (span-cursor-end sp2)))
    (let loop ((curs1 start1)
               (curs2 start2))
      (cond ((span-cursor=? sp1 curs1 end1)
             sp1)
            ((span-cursor=? sp2 curs2 end2)
             sp2)
            ((char=? (span-cursor-ref sp1 curs1)
                     (span-cursor-ref sp2 curs2))
             (loop (span-cursor-next sp1 curs1)
                   (span-cursor-next sp2 curs2)))
            (else
             (subspan/cursors sp1 start1 curs1))))))

(define (span-suffix sp1 sp2)
  (let ((last1 (span-cursor-prev sp1 (span-cursor-start sp1)))
        (last2 (span-cursor-prev sp2 (span-cursor-start sp2))))
    (let loop ((curs1 (span-cursor-prev sp1 (span-cursor-end sp1)))
               (curs2 (span-cursor-prev sp2 (span-cursor-end sp2))))
      (cond ((span-cursor=? sp1 curs1 last1)
             sp1)
            ((span-cursor=? sp2 curs2 last2)
             sp2)
            ((char=? (span-cursor-ref sp1 curs1)
                     (span-cursor-ref sp2 curs2))
             (loop (span-cursor-prev sp1 curs1)
                   (span-cursor-prev sp2 curs2)))
            (else
             (subspan/cursors sp1
                              (span-cursor-next sp1 curs1)
                              (span-cursor-end sp1)))))))

(define (span-prefix-length sp1 sp2)
  (let ((start1 (span-cursor-start sp1))
        (start2 (span-cursor-start sp2))
        (end1 (span-cursor-end sp1))
        (end2 (span-cursor-end sp2)))
    (let loop ((curs1 start1)
               (curs2 start2)
               (n 0))
      (cond ((span-cursor=? sp1 curs1 end1)
             n)
            ((span-cursor=? sp2 curs2 end2)
             n)
            ((char=? (span-cursor-ref sp1 curs1)
                     (span-cursor-ref sp2 curs2))
             (loop (span-cursor-next sp1 curs1)
                   (span-cursor-next sp2 curs2)
                   (+ n 1)))
            (else
             n)))))

(define (span-suffix-length sp1 sp2)
  (let ((last1 (span-cursor-prev sp1 (span-cursor-start sp1)))
        (last2 (span-cursor-prev sp2 (span-cursor-start sp2))))
    (let loop ((curs1 (span-cursor-prev sp1 (span-cursor-end sp1)))
               (curs2 (span-cursor-prev sp2 (span-cursor-end sp2)))
               (n 0))
      (cond ((span-cursor=? sp1 curs1 last1)
             n)
            ((span-cursor=? sp2 curs2 last2)
             n)
            ((char=? (span-cursor-ref sp1 curs1)
                     (span-cursor-ref sp2 curs2))
             (loop (span-cursor-prev sp1 curs1)
                   (span-cursor-prev sp2 curs2)
                   (+ n 1)))
            (else
             n)))))

(define (span-mismatch sp1 sp2)
  (let ((start1 (span-cursor-start sp1))
        (start2 (span-cursor-start sp2))
        (end1 (span-cursor-end sp1))
        (end2 (span-cursor-end sp2)))
    (let loop ((curs1 start1)
               (curs2 start2))
      (cond ((or (span-cursor=? sp1 curs1 end1)
                 (span-cursor=? sp2 curs2 end2))
             (if (span-null? sp2)
                 (span-cursor-prev sp2 start2)
                 curs2))
            ((char=? (span-cursor-ref sp1 curs1)
                     (span-cursor-ref sp2 curs2))
             (loop (span-cursor-next sp1 curs1)
                   (span-cursor-next sp2 curs2)))
            (else
             curs2)))))

(define (span-mismatch-right sp1 sp2)
  (let ((last1 (span-cursor-prev sp1 (span-cursor-start sp1)))
        (last2 (span-cursor-prev sp2 (span-cursor-start sp2))))
    (let loop ((curs1 (span-cursor-prev sp1 (span-cursor-end sp1)))
               (curs2 (span-cursor-prev sp2 (span-cursor-end sp2))))
      (cond ((span-cursor=? sp1 curs1 last1)
             curs2)
            ((span-cursor=? sp2 curs2 last2)
             curs2)
            ((char=? (span-cursor-ref sp1 curs1)
                     (span-cursor-ref sp2 curs2))
             (loop (span-cursor-prev sp1 curs1)
                   (span-cursor-prev sp2 curs2)))
            (else
             curs2)))))

(define (span-prefix? sp1 sp2)
  (= (span-length sp1) (span-prefix-length sp1 sp2)))

(define (span-suffix? sp1 sp2)
  (= (span-length sp1) (span-suffix-length sp1 sp2)))

;;; Searching.

(define (span-count pred sp)
  (let ((end (span-cursor-end sp)))
    (do ((curs (span-cursor-start sp)
               (span-cursor-next sp curs))
         (n    0
               (if (pred (span-cursor-ref sp curs))
                   (+ n 1)
                   n)))
        ((span-cursor=? sp curs end)
         n))))

(define (span-find pred sp)
  (let ((end (span-cursor-end sp)))
    (let loop ((curs (span-cursor-start sp)))
      (cond ((span-cursor=? sp curs end)
             curs)
            ((pred (span-cursor-ref sp curs))
             curs)
            (else
             (loop (span-cursor-next sp curs)))))))

(define (span-find-right pred sp)
  (let ((last (span-cursor-prev sp (span-cursor-start sp))))
    (let loop ((curs (span-cursor-prev sp (span-cursor-end sp))))
      (cond ((span-cursor=? sp curs last)
             curs)
            ((pred (span-cursor-ref sp curs))
             curs)
            (else
             (loop (span-cursor-prev sp curs)))))))

(define (span-skip pred sp)
  (let ((end (span-cursor-end sp)))
    (let loop ((curs (span-cursor-start sp)))
      (cond ((span-cursor=? sp curs end)
             curs)
            ((pred (span-cursor-ref sp curs))
             (loop (span-cursor-next sp curs)))
            (else
             curs)))))

(define (span-skip-right pred sp)
  (let ((last (span-cursor-prev sp (span-cursor-start sp))))
    (let loop ((curs (span-cursor-prev sp (span-cursor-end sp))))
      (cond ((span-cursor=? sp curs last)
             curs)
            ((pred (span-cursor-ref sp curs))
             (loop (span-cursor-prev sp curs)))
            (else
             curs)))))

(define (span-take-while pred sp)
  (subspan/cursors sp (span-cursor-start sp) (span-skip pred sp)))

(define (span-drop-while pred sp)
  (subspan/cursors sp (span-skip pred sp) (span-cursor-end sp)))

(define (span-span pred sp)
  (let ((curs (span-skip pred sp)))
    (values (subspan/cursors sp (span-cursor-start sp) curs)
            (subspan/cursors sp curs (span-cursor-end sp)))))

(define (span-break pred sp)
  (let ((curs (span-find pred sp)))
    (values (subspan/cursors sp (span-cursor-start sp) curs)
            (subspan/cursors sp curs (span-cursor-end sp)))))

;;; Naive string search.

;;; Benchmarking says naive search is fastest for small needles
;;; and for haystacks that aren't much larger than the needle.
;;;
;;; Rabin-Karp is usually twice as slow as naive search, but is
;;; less likely to hit a worst case.
;;;
;;; Boyer-Moore-Horspool is usually best for random haystacks
;;; at least 20 times as long as the needle, provided the needle
;;; has length 5 or more, but its worst case is a best case for
;;; naive search.  The worst case for naive search is only
;;; slightly faster than the worst case for Boyer-Moore-Horspool.
;;;
;;; The full Boyer-Moore algorithm as implemented here rarely
;;; outperforms Rabin-Karp and often performs much worse.
;;;
;;; The worst case for Boyer-Moore-Horspool can usually be avoided by
;;; using a different algorithm when the three rightmost characters
;;; of the needle are all the same.

(define (span-contains haystack needle)
  (%check-span haystack 'span-contains)
  (%check-span needle 'span-contains)
  (let ((n0 (%span-length:estimated haystack))
        (n1 (%span-length:estimated needle)))
    (cond ((span-null? needle)
           #f)
          ((and (>= n1 5)
                (>= n0 (* 20 n1))
                (not (let* ((curs1 (span-cursor-prev needle
                                                     (span-cursor-end needle)))
                            (curs2 (span-cursor-prev needle curs1))
                            (curs3 (span-cursor-prev needle curs2)))
                       (char=? (span-cursor-ref needle curs1)
                               (span-cursor-ref needle curs2)
                               (span-cursor-ref needle curs3)))))
           (%span-contains:boyer-moore haystack needle))
          ((and (> n1 10)
                (> n0 (* 2 n1)))
           (%span-contains:rabin-karp haystack needle))
          (else
           (%span-contains:naive haystack needle)))))

;;; The whole character span or string.

(define (span-reverse sp)
  (let ((end (span-cursor-end sp)))
    (do ((curs (span-cursor-start sp)
               (span-cursor-next sp curs))
         (chars '()
                (cons (span-cursor-ref sp curs) chars)))
        ((span-cursor=? sp curs end)
         (list->span chars)))))

(define (span-append . args)
  (span-concatenate args))

;;; The things can be spans or strings.

(define (span-concatenate things)
  (if (= 1 (length things))
      (if (span? (car things))
          (car things)
          (string->span (car things)))
      (let* ((out (open-output-string))
             (put (lambda (c) (write-char c out))))
        (for-each (lambda (sp)
                    (if (span? sp)
                        (span-for-each put sp)
                        (string-for-each put sp)))
                  things)
        (string->span (call-with-port out get-output-string)))))

;;; FIXME: why is this part of the API?

(define (span-concatenate-reverse things)
  (span-concatenate (reverse things)))

;;; Folding and mapping.

;;; FIXME: this would be more efficient going right to left.

(define (span-map proc sp1 . rest)

  (define (span-map1 sp)
    (let ((end (span-cursor-end sp)))
      (do ((curs (span-cursor-start sp)
                 (span-cursor-next sp curs))
           (chars '()
                  (cons (proc (span-cursor-ref sp curs))
                        chars)))
          ((span-cursor=? sp curs end)
           (string->span (list->string (reverse chars)))))))

  (cond ((null? rest)
         (span-map1 sp1))
        (else
         (let* ((spans (cons sp1 rest))
                (cursors (map span-cursor-start spans))
                (n (apply min (map span-length spans))))
           (define (%span-map cursors i chars)
             (cond ((= i n)
                    (string->span (list->string (reverse chars))))
                   (else
                    (%span-map (map span-cursor-next spans cursors)
                               (+ i 1)
                               (cons (apply proc
                                            (map span-cursor-ref
                                                 spans
                                                 cursors))
                                     chars)))))
           (%span-map cursors 0 '())))))

(define (span-for-each proc sp1 . rest)

  (define (span-for-each1 sp)
    (let ((end (span-cursor-end sp)))
      (do ((curs (span-cursor-start sp)
                 (span-cursor-next sp curs)))
          ((span-cursor=? sp curs end))
        (proc (span-cursor-ref sp curs)))))

  (cond ((null? rest)
         (span-for-each1 sp1))
        (else
         (let* ((spans (cons sp1 rest))
                (cursors (map span-cursor-start spans))
                (n (apply min (map span-length spans))))
           (define (%span-for-each cursors i)
             (cond ((= i n)
                    (if #f #f))
                   (else
                    (apply proc
                           (map span-cursor-ref
                                spans
                                cursors))
                    (%span-for-each (map span-cursor-next spans cursors)
                                    (+ i 1)))))
           (%span-for-each cursors 0)))))

(define (span-fold proc nil . rest)

  (define (span-fold1 sp)
    (let ((end (span-cursor-end sp)))
      (do ((curs (span-cursor-start sp)
                 (span-cursor-next sp curs))
           (nil  nil
                 (proc (span-cursor-ref sp curs) nil)))
          ((span-cursor=? sp curs end)
           nil))))

  (cond ((null? rest)
         nil)
        ((null? (cdr rest))
         (span-fold1 (car rest)))
        (else
         (let* ((spans rest)
                (cursors (map span-cursor-start spans))
                (n (apply min (map span-length spans))))
           (define (%span-fold cursors i nil)
             (cond ((= i n)
                    nil)
                   (else
                    (%span-fold (map span-cursor-next spans cursors)
                                (+ i 1)
                                (apply proc
                                       (append (map span-cursor-ref
                                                    spans
                                                    cursors)
                                               (list nil)))))))
           (%span-fold cursors 0 nil)))))

(define (span-fold-right proc nil . rest)

  (define (span-fold-right1 sp)
    (let ((last (span-cursor-prev sp (span-cursor-start sp))))
      (do ((curs (span-cursor-prev sp (span-cursor-end sp))
                 (span-cursor-prev sp curs))
           (nil  nil
                 (proc (span-cursor-ref sp curs) nil)))
          ((span-cursor=? sp curs last)
           nil))))

  (cond ((null? rest)
         nil)
        ((null? (cdr rest))
         (span-fold-right1 (car rest)))
        (else
         (let* ((spans rest)
                (cursors (map span-cursor-prev
                              spans
                              (map span-cursor-end spans)))
                (n (apply min (map span-length spans))))
           (define (%span-fold-right cursors i nil)
             (cond ((= i n)
                    nil)
                   (else
                    (%span-fold-right (map span-cursor-prev spans cursors)
                                      (+ i 1)
                                      (apply proc
                                             (append (map span-cursor-ref
                                                          spans
                                                          cursors)
                                                     (list nil)))))))
           (%span-fold-right cursors 0 nil)))))

;;; Parsing and unparsing.

;;; FIXME:  The specification of span-split is unclear, but I'm guessing
;;; the semantics of span-split is the union of three special cases:
;;;
;;;     no separator is supplied
;;;     the separator is supplied and is an empty span
;;;     the separator is supplied and is not an empty span

(define (span-split sp . rest)
  (let ((separator (if (null? rest)
                       #f
                       (car rest)))
        (limit (if (or (null? rest) (null? (cdr rest)))
                   (+ 1 (span-length sp))
                   (car (cdr rest))))
        (end (span-cursor-end sp)))
    (cond ((not separator)
           (span-split-using-whitespace sp))
          ((= 0 (span-length separator))
           (span-split-into-characters sp limit))
          (else
           (span-split-using-word sp separator limit)))))

(define (span-split-using-whitespace sp)
  (let ((end (span-cursor-end sp)))
    (let loop ((curs (span-cursor-start sp))
               (word-start-maybe #f)
               (words '()))
      (cond ((span-cursor=? sp curs end)
             (reverse (if word-start-maybe
                          (cons (subspan/cursors sp word-start-maybe curs)
                                words)
                          words)))
            ((char-whitespace? (span-cursor-ref sp curs))
             (if word-start-maybe
                 (loop (span-cursor-next sp curs)
                       #f
                       (cons (subspan/cursors sp word-start-maybe curs)
                             words))
                 (loop (span-cursor-next sp curs)
                       #f
                       words)))
            (else
             (loop (span-cursor-next sp curs)
                   (or word-start-maybe curs)
                   words))))))

(define (span-split-into-characters sp limit)
  (let ((n (span-length sp)))
    (cond ((> n (+ limit 1))
           (append (span-split-into-characters (subspan sp 0 limit) limit)
                   (subspan sp limit n)))
          (else
           (map span (span->list sp))))))

(define (span-split-using-word sp sep limit)
  (cond ((= 0 limit)
         (list sp))
        (else
         (let ((curs (span-contains sp sep)))
           (if curs
               (cons (subspan/cursors sp
                                      (span-cursor-start sp)
                                      curs)
                     (span-split-using-word
                      (subspan/cursors sp
                                       (span-cursor-forward sp
                                                            curs
                                                            (span-length sep))
                                       (span-cursor-end sp))
                      sep
                      (- limit 1)))
               (list sp))))))

;;; FIXME: allows delimiter to be a span or a string.

(define (span-join things . rest)
  (let ((delimiter (if (null? rest)
                       (span #\space)
                       (car rest)))
        (grammar (if (or (null? rest) (null? (cdr rest)))
                     'infix
                     (car (cdr rest)))))
    (let loop ((things-in (reverse things))
               (things-out (if (eq? 'suffix grammar)
                               (list delimiter)
                               '())))
      (if (null? things-in)
          (cond ((eq? 'prefix grammar)
                 (span-concatenate things-out))
                ((and (eq? 'strict-infix grammar)
                      (null? things))
                 (error "span-join: empty join with strict-infix"
                        things delimiter grammar))
                ((null? things-out)
                 (span))
                (else
                 (span-concatenate (cdr things-out))))
          (loop (cdr things-in)
                (cons delimiter
                      (cons (car things-in)
                            things-out)))))))

;;; Filtering and partitioning.

(define (span-filter pred sp)
  (let ((start (span-cursor-start sp))
        (end (span-cursor-end sp)))
    (let loop ((curs start)
               (start start)
               (spans '()))
      (cond ((span-cursor=? sp curs end)
             (span-concatenate (reverse (cons (subspan/cursors sp start curs)
                                              spans))))
            ((pred (span-cursor-ref sp curs))
             (loop (span-cursor-next sp curs)
                   start
                   spans))
            (else
             (let ((next (span-cursor-next sp curs)))
               (loop next
                     next
                     (cons (subspan/cursors sp start curs)
                           spans))))))))

(define (span-remove pred sp)
  (span-filter (lambda (x) (not (pred x))) sp))

(define (span-partition pred sp)
  (values (span-filter pred sp)
          (span-remove pred sp)))

;;; Conversion.

(define (span->list sp)
  (string->list (%span->string sp)))

(define (span->vector sp)
  (string->vector (%span->string sp)))

(define (list->span chars)
  (%string->span (list->string chars)))

(define (vector->span v)
  (list->span (vector->list v)))

(define (reverse-list->span chars)
  (list->span (reverse chars)))

;;; Case.

(define (span-upcase sp)
  (%string->span (string-upcase (%span->string sp))))

(define (span-downcase sp)
  (%string->span (string-downcase (%span->string sp))))

(define (span-foldcase sp)
  (%string->span (string-foldcase (%span->string sp))))

;;; Comparison.

(define (%span-comparison pred)
  (lambda (sp1 sp2 . rest)
    (apply pred (map %span->string (cons sp1 (cons sp2 rest))))))

(define span=?  (%span-comparison string=?))
(define span<?  (%span-comparison string<?))
(define span>?  (%span-comparison string>?))
(define span<=? (%span-comparison string<=?))
(define span>=? (%span-comparison string>=?))

(define span-ci=?  (%span-comparison string-ci=?))
(define span-ci<?  (%span-comparison string-ci<?))
(define span-ci>?  (%span-comparison string-ci>?))
(define span-ci<=? (%span-comparison string-ci<=?))
(define span-ci>=? (%span-comparison string-ci>=?))

;;; Comparator.

(define span-comparator
  (let ((string-type-test  (comparator-type-test-procedure string-comparator))
        (string-equality   (comparator-equality-predicate string-comparator))
        (string-comparison (comparator-comparison-procedure string-comparator))
        (string-hash       (comparator-hash-function string-comparator)))
    (make-comparator
     span?
     (lambda (sp1 sp2)
       (string-equality (%span->string sp1)
                        (%span->string sp2)))
     (lambda (sp1 sp2)
       (string-comparison (%span->string sp1)
                          (%span->string sp2)))
     (lambda (sp)
       (string-hash (%span->string sp))))))

; eof
