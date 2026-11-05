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

(define-library (scheme regex)
  (export regexp regexp? valid-sre? rx regexp->sre char-set->sre
          regexp-matches regexp-matches? regexp-search
          regexp-replace regexp-replace-all
          regexp-fold regexp-extract regexp-split regexp-partition
          regexp-match? regexp-match-count
          regexp-match-submatch regexp-match-submatch/list
          regexp-match-submatch-start regexp-match-submatch-end
          regexp-match->list regexp-match->sexp)
  (import (srfi 115)))
