;; SRFI 113: Sets and Bags
;; Copyright (C) John Cowan 2013. All Rights Reserved.
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

(define-library (srfi 113 sets)

  (export set set-unfold
          set? set-contains? set-empty? set-disjoint?
          set-member set-element-comparator
          set-adjoin set-adjoin! set-replace set-replace!
          set-delete set-delete! set-delete-all set-delete-all! set-search!
          set-size set-find set-count set-any? set-every?
          set-map set-for-each set-fold
          set-filter set-remove set-partition
          set-filter! set-remove! set-partition!
          set-copy set->list list->set list->set!
          set=? set<? set>? set<=? set>=?
          set-union set-intersection set-difference set-xor
          set-union! set-intersection! set-difference! set-xor!
          set-comparator
  
          bag bag-unfold
          bag? bag-contains? bag-empty? bag-disjoint?
          bag-member bag-element-comparator
          bag-adjoin bag-adjoin! bag-replace bag-replace!
          bag-delete bag-delete! bag-delete-all bag-delete-all! bag-search!
          bag-size bag-find bag-count bag-any? bag-every?
          bag-map bag-for-each bag-fold
          bag-filter bag-remove bag-partition
          bag-filter! bag-remove! bag-partition!
          bag-copy bag->list list->bag list->bag!
          bag=? bag<? bag>? bag<=? bag>=?
          bag-union bag-intersection bag-difference bag-xor
          bag-union! bag-intersection! bag-difference! bag-xor!
          bag-comparator
  
  
          bag-sum bag-sum! bag-product bag-product!
          bag-unique-size bag-element-count bag-for-each-unique bag-fold-unique
          bag-increment! bag-decrement! bag->set set->bag set->bag!
          bag->alist alist->bag)

  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          ;(srfi 114 comparators)
          (except (srfi 128) string-hash string-ci-hash)
          (srfi 69))

  (include "113.body.scm")
)


(define-library (srfi 113)

  (export set set-unfold
          set? set-contains? set-empty? set-disjoint?
          set-member set-element-comparator
          set-adjoin set-adjoin! set-replace set-replace!
          set-delete set-delete! set-delete-all set-delete-all! set-search!
          set-size set-find set-count set-any? set-every?
          set-map set-for-each set-fold
          set-filter set-remove set-partition
          set-filter! set-remove! set-partition!
          set-copy set->list list->set list->set!
          set=? set<? set>? set<=? set>=?
          set-union set-intersection set-difference set-xor
          set-union! set-intersection! set-difference! set-xor!
          set-comparator
  
          bag bag-unfold
          bag? bag-contains? bag-empty? bag-disjoint?
          bag-member bag-element-comparator
          bag-adjoin bag-adjoin! bag-replace bag-replace!
          bag-delete bag-delete! bag-delete-all bag-delete-all! bag-search!
          bag-size bag-find bag-count bag-any? bag-every?
          bag-map bag-for-each bag-fold
          bag-filter bag-remove bag-partition
          bag-filter! bag-remove! bag-partition!
          bag-copy bag->list list->bag list->bag!
          bag=? bag<? bag>? bag<=? bag>=?
          bag-union bag-intersection bag-difference bag-xor
          bag-union! bag-intersection! bag-difference! bag-xor!
          bag-comparator
  
  
          bag-sum bag-sum! bag-product bag-product!
          bag-unique-size bag-element-count bag-for-each-unique bag-fold-unique
          bag-increment! bag-decrement! bag->set set->bag set->bag!
          bag->alist alist->bag)

  (import (srfi 113 sets))
)

; eof
