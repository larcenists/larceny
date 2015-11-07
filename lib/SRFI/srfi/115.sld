;; SRFI 115: Scheme Regular Expressions
;; Copyright (C) Alex Shinn 2014. All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Known bugs in SRFI 115 or the reference implementation:
;;;
;;; SRFI 115 does not mention &, but the reference implementation treats
;;; & as a synonym for and.
;;;
;;; In the productions for <cset-sre>, SRFI 115 fails to list |\|| as
;;; a synonym for or.
;;;
;;; SRFI 115 does not list letter as a <cset-sre>, but several examples
;;; assume letter is a <cset-sre>.  Those examples should be corrected
;;; to use alpha instead.
;;;
;;; SRFI 115 says the following example should match, but that's wrong:
;;;
;;;     (regexp-search '(: bow "foo") "")
;;;
;;; SRFI 115 examples say all of the next four should succeed,
;;; but the second of the four paragraphs preceding these examples
;;; implies all four should fail.
;;;
;;;     (regexp-search '(w/nocase (~ ("Aab"))) "B")
;;;     (regexp-search '(w/nocase (~ ("Aab"))) "b")
;;;     (regexp-search '(~ (w/nocase ("Aab"))) "B")
;;;     (regexp-search '(~ (w/nocase ("Aab"))) "b")
;;;
;;; Reference implementation does not implement the following optional stuff:
;;;   "...only supported if the feature regexp-non-greedy is provided."
;;;     ?? non-greedy-optional
;;;     *? non-greedy-zero-or-more
;;;     **? non-greedy-repeated
;;;   "...only supported if the feature regexp-look-around is provided."
;;;     look-ahead look-behind neg-look-ahead neg-look-behind

;; From
;; https://code.google.com/p/chibi-scheme/source/browse/lib/chibi/char-set/boundary.sld
;;
;; The included file was renamed for Larceny,
;; and the export list was moved to its canonical position.

;; Character sets for Unicode boundaries, TR29.

(define-library (chibi char-set boundary)

  (cond-expand (else)) ; keeps this library from showing up in (features)

  (export char-set:regional-indicator
          char-set:extend-or-spacing-mark
          char-set:hangul-l
          char-set:hangul-v
          char-set:hangul-t
          char-set:hangul-lv
          char-set:hangul-lvt)
  (cond-expand
   (chibi
    (import (chibi) (chibi char-set)))
   (else
    (import (scheme base) (srfi 14))
    (begin (define (immutable-char-set cs) cs))))
  ;; generated with:
  ;; tools/extract-unicode-props.scm --derived GraphemeBreakProperty.txt
  ;;   Control extend-or-spacing-mark=Extend,SpacingMark Regional_Indicator
  ;;   hangul-l=:L hangul-v=:V hangul-t=:T hangul-lv=:LV hangul-lvt=:LVT
  (include "115.boundary.scm"))

;; From
;; https://code.google.com/p/chibi-scheme/source/browse/lib/chibi/regexp.sld
;;
;; The included file was renamed for Larceny,
;; and importation of SRFI 33 (withdrawn) was replaced by importation
;; of (rnrs arithmetic bitwise).

(define-library (chibi regexp)

  (cond-expand (else)) ; keeps this library from showing up in (features)

  (export regexp regexp? valid-sre? rx regexp->sre char-set->sre
          regexp-matches regexp-matches? regexp-search
          regexp-replace regexp-replace-all
          regexp-fold regexp-extract regexp-split regexp-partition
          regexp-match? regexp-match-count
          regexp-match-submatch regexp-match-submatch/list
          regexp-match-submatch-start regexp-match-submatch-end
          regexp-match->list regexp-match->sexp)
  (import (rnrs arithmetic bitwise) (srfi 69))
  ;; Chibi's char-set library is more factored than SRFI-14.
  (cond-expand
   (chibi
    (import (rename (chibi)
                    (protect guard)
                    (char-downcase %char-downcase)
                    (char-upcase %char-upcase))
            (only (scheme char) char-downcase char-upcase)
            (srfi 9)
            (chibi char-set)
            (chibi char-set full)
            (prefix (chibi char-set ascii) %))
    (begin
      (define char-set:title-case
        (char-set-union
         (ucs-range->char-set #x1F88 #x1F90)
         (ucs-range->char-set #x1F98 #x1FA0)
         (ucs-range->char-set #x1FA8 #x1FB0)
         (char-set #\x01C5 #\x01C8 #\x01CB #\x01F2 #\x1FBC #\x1FCC #\x1FFC)))))
   (else
    (import (scheme base) (scheme char) (srfi 14))
    (begin
      (define %char-set:letter
        (char-set-intersection char-set:ascii char-set:letter))
      (define %char-set:lower-case
        (char-set-intersection char-set:ascii char-set:lower-case))
      (define %char-set:upper-case
        (char-set-intersection char-set:ascii char-set:upper-case))
      (define %char-set:digit
        (char-set-intersection char-set:ascii char-set:digit))
      (define %char-set:letter+digit
        (char-set-intersection char-set:ascii char-set:letter+digit))
      (define %char-set:punctuation
        (char-set-intersection char-set:ascii char-set:punctuation))
      (define %char-set:symbol
        (char-set-intersection char-set:ascii char-set:symbol))
      (define %char-set:graphic
        (char-set-intersection char-set:ascii char-set:graphic))
      (define %char-set:whitespace
        (char-set-intersection char-set:ascii char-set:whitespace))
      (define %char-set:printing
        (char-set-intersection char-set:ascii char-set:printing))
      (define %char-set:iso-control
        (char-set-intersection char-set:ascii char-set:iso-control)))))
  (import (chibi char-set boundary))
  ;; Use string-cursors where available.
  (begin
    (define string-cursor? integer?))
  (cond-expand
   (chibi
    (begin
      (define (string-start-arg s o)
        (if (pair? o) (string-index->offset s (car o)) (string-cursor-start s)))
      (define (string-end-arg s o)
        (if (pair? o) (string-index->offset s (car o)) (string-cursor-end s)))
      (define (string-concatenate-reverse ls)
        (string-concatenate (reverse ls)))))
   (else
 (import (scheme write))
    (begin
      (define (string-start-arg s o)
        (if (pair? o) (string-index->offset s (car o)) 0))
      (define (string-end-arg s o)
        (if (pair? o) (string-index->offset s (car o)) (string-length s)))
      (define string-cursor=? =)
      (define string-cursor<? <)
      (define string-cursor<=? <=)
      (define string-cursor>? >)
      (define string-cursor>=? >=)
      (define string-cursor-ref string-ref)
      (define (string-cursor-next s i) (+ i 1))
      (define (string-cursor-prev s i) (- i 1))
      (define substring-cursor substring)
      (define (string-offset->index str off) off)
      (define (string-index->offset str i) i)
      (define (string-concatenate ls) (apply string-append ls))
      (define (string-concatenate-reverse ls)
        (string-concatenate (reverse ls))))))

  (include "115.body.scm"))


(define-library (srfi 115 regexp)
  (export regexp regexp? valid-sre? rx regexp->sre char-set->sre
          regexp-matches regexp-matches? regexp-search
          regexp-replace regexp-replace-all
          regexp-fold regexp-extract regexp-split regexp-partition
          regexp-match? regexp-match-count
          regexp-match-submatch regexp-match-submatch/list
          regexp-match-submatch-start regexp-match-submatch-end
          regexp-match->list regexp-match->sexp)
  (import (chibi regexp)))


(define-library (srfi 115)
  (export regexp regexp? valid-sre? rx regexp->sre char-set->sre
          regexp-matches regexp-matches? regexp-search
          regexp-replace regexp-replace-all
          regexp-fold regexp-extract regexp-split regexp-partition
          regexp-match? regexp-match-count
          regexp-match-submatch regexp-match-submatch/list
          regexp-match-submatch-start regexp-match-submatch-end
          regexp-match->list regexp-match->sexp)
  (import (srfi 115 regexp)))


