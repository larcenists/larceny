; Copyright 2007 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright and permission notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Tools for constructing and testing unary predicates on
; Unicode characters.
;
; Requires an implementation of R5RS Scheme or better that
; supports Unicode characters and the char-general-category
; predicate proposed for R6RS.
;
; Typical usage:
;     ; First use parseUCD.sch to update all of the tables that
;     ; are generated automatically, notably the tables used by
;     ; char-general-category
;     ; Then, in an implementation whose char-general-category
;     ; procedure has been updated, do this:
;
;     (load "parseUCD.sch")
;     (load "strategy.sch")
;     (read-unicode-files)
;     (decision-strategy (extract-other-alphabetic))
;     ...
;
; For the specific calls to decision-strategy that were used to
; guide the implementation of the reference implementation,
; search the unicode*.sch files for "FIXME" comments.
;
; The code in this file is quick-and-dirty, since it is needed
; only when updating the reference implementation for a new
; revision of the Unicode Character Database.

; Given a unary predicate on characters, returns a sorted
; list of all characters that satisfy the predicate.

(define (filter-all-chars p?)
  (do ((i 0 (+ i 1))
       (chars '()
              (if (and (not (<= #xd800 i #xdfff))
                       (p? (integer->char i)))
                  (cons (integer->char i) chars)
                  chars)))
      ((= i #x110000)
       (reverse chars))))

; Given a list of general categories, returns a sorted
; list of all characters in those categories.

(define (from-categories categories)
  (filter-all-chars
   (lambda (c) (memq (char-general-category c) categories))))

; Given a sorted list of characters, returns a list of
; their ranges.  A range consists of a single character
; or a list of two characters (inclusive).

(define (chars->ranges chars)
  (define (loop chars ranges)
    (if (null? chars)
        (reverse ranges)
        (next-range (cdr chars) ranges (car chars) (car chars))))
  (define (next-range chars ranges c0 c1)
    (cond ((null? chars)
           (loop chars (cons (make-range c0 c1) ranges)))
          ((= (+ (char->integer c1) 1) (char->integer (car chars)))
           (next-range (cdr chars) ranges c0 (car chars)))
          (else
           (loop chars (cons (make-range c0 c1) ranges)))))
  (define (make-range c0 c1)
    (if (char=? c0 c1)
        c0
        (list c0 c1)))
  (loop chars '()))

; Given a list of characters or pairs of characters,
; returns a sorted list of the characters denoted by
; those characters and ranges.

(define (ranges->chars input)
  (define (loop input numbers)
    (cond ((null? input)
           (map integer->char (mysort <= (reverse numbers))))
          ((char? (car input))
           (loop (cdr input)
                 (cons (char->integer (car input)) numbers)))
          (else
           (let* ((x (car input))
                  (n1 (char->integer (car x)))
                  (n2 (char->integer (cadr x))))
             (loop (cdr input)
                   (range-loop n1 n2 numbers))))))
  (define (range-loop n1 n2 numbers)
    (if (> n1 n2)
        numbers
        (range-loop (+ n1 1) n2 (cons n1 numbers))))
  (loop input '()))

; Given a list of ranges as returned by chars->ranges,
; returns an equivalent list of hexadecimal strings and
; hexadecimal ranges that can be passed to decision-strategy.

(define (ranges->hexranges input)
  (map (lambda (x)
         (cond ((char? x)
                (number->string (char->integer x) 16))
               ((and (pair? x)
                     (char? (car x)))
                (ranges->hexranges x))
               (else
                (error "ranges->hexranges: invalid argument" input))))
       input))

; Given a list of strings of hexadecimal digits or pairs of
; strings of hexadecimal digits (indicating ranges), returns
; a sorted list of the characters denoted by those strings
; and ranges.

(define (hexranges->chars input)
  (define (loop input numbers)
    (cond ((null? input)
           (map integer->char (mysort <= (reverse numbers))))
          ((string? (car input))
           (loop (cdr input)
                 (cons (string->number (car input) 16) numbers)))
          (else
           (let* ((x (car input))
                  (n1 (string->number (car x) 16))
                  (n2 (string->number (cadr x) 16)))
             (loop (cdr input)
                   (range-loop n1 n2 numbers))))))
  (define (range-loop n1 n2 numbers)
    (if (> n1 n2)
        numbers
        (range-loop (+ n1 1) n2 (cons n1 numbers))))
  (loop input '()))

; Given a sorted list of characters and a sorted list of excluded
; characters and ranges,
; returns a sorted list of excluded characters and ranges
; that excludes the same characters from the list but may merge
; some adjacent ranges.
;
; The basic rule is:
; Two adjacent excluded ranges may be merged unless
; a character falls between them.
;
; Examples:
; (merge-excluded-ranges '(#\a #\e #\i) '(#\a #\z)) => (#\a #\z)
; (merge-excluded-ranges '(#\a #\e #\i) '(#\a #\e)) => ((#\a #\e))
; (merge-excluded-ranges '(#\a #\e #\i) '((#\a #\c) (#\w #\z)))
;     => ((#\a #\c) (#\w #\z))
; (merge-excluded-ranges '(#\a #\e #\i) '((#\a #\m) (#\w #\z)))
;     => ((#\a #\m))
; (merge-excluded-ranges '(#\a #\e #\i #\y) '((#\a #\m) (#\w #\z)))
;     => ((#\a #\z))

(define (merge-excluded-ranges chars ranges)
  (cond ((null? chars)
         (if (null? ranges) ranges (list (car ranges))))
        ((or (null? chars) (null? ranges) (null? (cdr ranges)))
         ranges)
        ; There is at least one character and at least two ranges.
        (else
         (let* ((c (car chars))
                (r01 (car ranges))
                (r02 (cadr ranges))
                (r1 (if (char? r01) (list r01 r01) r01))
                (r2 (if (char? r02) (list r02 r02) r02)))
           (cond ((char<=? c (cadr r1))
                  (merge-excluded-ranges (cdr chars) ranges))
                 ((char<? c (car r2))
                  ; can't merge
                  (cons r01 (merge-excluded-ranges chars (cdr ranges))))
                 (else
                  ; merge
                  (merge-excluded-ranges chars
                                         (cons (list (car r1) (cadr r2))
                                               (cddr ranges)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; decision-strategy
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a list of entries that enumerate a set of scalar values
; in increasing order, where each entry is either a string of
; hexadecimal digits or a list of two such strings denoting an
; inclusive range, returns a suggested strategy for deciding
; whether an arbitrary scalar value belongs to that set.
;
; The strategy consists of a set of Unicode general categories
; whose scalar values, possibly with some exceptions, belong
; to the set, plus a set of oddball scalar values that also
; belong to the set.
;
; A strategy is represented by a list of the following form:
;
; <strategy>      ::=  ( <categories> (oddball <additions>) )
; <categories>    ::=  <empty>  |  <category> <categories>
; <additions>     ::=  <empty>  |  <addition> <additions>
; <subtractions>  ::=  <additions>
; <category>      ::=  <symbol>
;                   |  ( <symbol> <subtractions> )
;                   |  ( ( <symbol> <symbol> ... ) <subtractions> )
; <addition>      ::=  <hexstring>  |  ( <hexstring> <hexstring> )
;
; Note that a strategy may consist of set of ranges.

(define-syntax show
  (syntax-rules ()
   ((show x)
    (begin (display 'x) (display ":") (newline) (pretty-print x) (newline)))))

(define (decision-strategy charset0)

  (let* ((chars (hexranges->chars charset0))
         (ranges (chars->ranges chars))
         (categories (map char-general-category chars))

         ; Will become an alist that associates
         ; Unicode general categories
         ; with the sorted subset of chars that is in the category.

         (chars-by-category '())

         ; Will become an alist that associates
         ; the Unicode general categories from the above alist
         ; with the set of all characters in those categories.

         (all-chars-by-category '())

         ; Will become an alist that associates
         ; the Unicode general categories from the above alists
         ; with the sets of characters in those categories
         ; that are not in chars.

         (diffs-by-category '())

         ; Will become an alist that associates
         ; the Unicode general categories from the above alists
         ; with the ordered list of ranges of characters in those
         ; categories that are not in chars.

         (excluded-ranges-by-category '())

         ; Will become an alist that associates
         ; the Unicode general categories from the above alists
         ; with the ordered list of ranges of characters in those
         ; categories that are in chars.

         (ranges-by-category '())

         ; Will become a sorted list of characters that should probably
         ; be recognized separately, independent of general categories.

         (oddball-chars '())

         ; A possible strategy.

         (strategy ranges)

         ; True if the current strategy uses Unicode general categories.

         (strategy-uses-categories? #f))

    ; Given two sorted lists of characters,
    ; of which the second list is a sublist of the first,
    ; returns a sorted list of the characters in the first list
    ; that aren't in the second.

    (define (the-difference x y)
      (define (loop x y diff)
        (cond ((null? x)
               (reverse diff))
              ((null? y)
               (loop (cdr x) y (cons (car x) diff)))
              ((char=? (car x) (car y))
               (loop (cdr x) (cdr y) diff))
              (else
               (loop (cdr x) y (cons (car x) diff)))))
      (loop x y '()))

    ; Given two sorted lists of characters,
    ; returns a sorted list of the characters
    ; that are in either list.

    (define (the-union x y)
      (define (loop x y union)
        (cond ((and (null? x) (null? y))
               (reverse union))
              ((null? x)
               (append (reverse union) y))
              ((null? y)
               (append (reverse union) x))
              ((char=? (car x) (car y))
               (loop (cdr x) (cdr y) (cons (car x) union)))
              ((char<? (car x) (car y))
               (loop (cdr x) y (cons (car x) union)))
              (else
               (loop x (cdr y) (cons (car y) union)))))
      (loop x y '()))

    (for-each (lambda (char cat)
                (let ((x (assq cat chars-by-category)))
                  (if x
                      (set-cdr! x (cons char (cdr x)))
                      (set! chars-by-category
                            (cons (list cat char) chars-by-category)))))
              chars
              categories)

    (set! chars-by-category
          (map (lambda (x)
                 (cons (car x) (reverse (cdr x))))
               chars-by-category))

    (set! all-chars-by-category
          (map (lambda (cat) (cons cat (from-categories (list cat))))
               (map car chars-by-category)))

    (set! diffs-by-category
          (map (lambda (x y)
                 (if (eq? (car x) (car y))
                     (cons (car x)
                           (the-difference (cdr y) (cdr x)))
                     ???))
               chars-by-category
               all-chars-by-category))

    (set! excluded-ranges-by-category
          (map (lambda (x) (cons (car x) (chars->ranges (cdr x))))
               diffs-by-category))

    (set! ranges-by-category
          (map (lambda (x) (cons (car x) (chars->ranges (cdr x))))
               chars-by-category))

    ; Another possible strategy.

    (set! strategy excluded-ranges-by-category)

    ; Try to merge adjacent excluded ranges.

    (for-each (lambda (x)
                (let ((cat (car x)))
                  (if (not (eq? cat 'oddball))
                      (let* ((chars (from-categories (list cat)))
                             (y (cons cat
                                      (merge-excluded-ranges chars
                                                             (cdr x)))))
                        (if (< (length y) (length x))
                            (set! strategy
                                  (cons y
                                        (filter (lambda (z) (not (eq? x z)))
                                                strategy))))))))
              strategy)

    ; Find categories for which it would be better to recognize
    ; the characters separately.

    (for-each (lambda (cat)
                (if (not (eq? cat 'oddball))
                    (let ((x (assq cat strategy))
                          (y (assq cat ranges-by-category)))
                      (if (< (length y) (length x))
                          (begin (set! oddball-chars
                                       (the-union (ranges->chars (cdr y))
                                                  oddball-chars))
                                 (set! strategy
                                       (filter (lambda (z)
                                                 (not (eq? x z)))
                                               strategy)))))))
              (map car strategy))

    (if (null? oddball-chars)
        strategy
        (append strategy
                (list (cons 'oddball (chars->ranges oddball-chars)))))))

; To do:
;
; Try combining several categories to see if that
; simplifies the excluded ranges.
