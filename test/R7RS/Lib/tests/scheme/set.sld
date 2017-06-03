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

;;; The copyright notice above is taken from srfi-113-test.sps7,
;;; from which this file is derived.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme set) procedures:
;;;
;;;     set
;;;     set-unfold
;;;     set?
;;;     set-contains?
;;;     set-empty?
;;;     set-disjoint?
;;;     set-member
;;;     set-element-comparator
;;;     set-adjoin
;;;     set-adjoin!
;;;     set-replace
;;;     set-replace!
;;;     set-delete
;;;     set-delete!
;;;     set-delete-all
;;;     set-delete-all!
;;;     set-search!
;;;     set-size
;;;     set-find
;;;     set-count
;;;     set-any?
;;;     set-every?
;;;     set-map
;;;     set-for-each
;;;     set-fold
;;;     set-filter
;;;     set-remove
;;;     set-partition
;;;     set-filter!
;;;     set-remove!
;;;     set-partition!
;;;     set-copy
;;;     set->list
;;;     list->set
;;;     list->set!
;;;     set=?
;;;     set<?
;;;     set>?
;;;     set<=?
;;;     set>=?
;;;     set-union
;;;     set-intersection
;;;     set-difference
;;;     set-xor
;;;     set-union!
;;;     set-intersection!
;;;     set-difference!
;;;     set-xor!
;;;     set-comparator
;;;  
;;;     bag
;;;     bag-unfold
;;;     bag?
;;;     bag-contains?
;;;     bag-empty?
;;;     bag-disjoint?
;;;     bag-member
;;;     bag-element-comparator
;;;     bag-adjoin
;;;     bag-adjoin!
;;;     bag-replace
;;;     bag-replace!
;;;     bag-delete
;;;     bag-delete!
;;;     bag-delete-all
;;;     bag-delete-all!
;;;     bag-search!
;;;     bag-size
;;;     bag-find
;;;     bag-count
;;;     bag-any?
;;;     bag-every?
;;;     bag-map
;;;     bag-for-each
;;;     bag-fold
;;;     bag-filter
;;;     bag-remove
;;;     bag-partition
;;;     bag-filter!
;;;     bag-remove!
;;;     bag-partition!
;;;     bag-copy
;;;     bag->list
;;;     list->bag
;;;     list->bag!
;;;     bag=?
;;;     bag<?
;;;     bag>?
;;;     bag<=?
;;;     bag>=?
;;;     bag-union
;;;     bag-intersection
;;;     bag-difference
;;;     bag-xor
;;;     bag-union!
;;;     bag-intersection!
;;;     bag-difference!
;;;     bag-xor!
;;;     bag-comparator
;;;  
;;;     bag-sum
;;;     bag-sum!
;;;     bag-product
;;;     bag-product!
;;;     bag-unique-size
;;;     bag-element-count
;;;     bag-for-each-unique
;;;     bag-fold-unique
;;;     bag-increment!
;;;     bag-decrement!
;;;     bag->set
;;;     set->bag
;;;     set->bag!
;;;     bag->alist
;;;     alist->bag


(define-library (tests scheme set)
  (export run-set-tests)
  (import (scheme base)
          (scheme set)
          (tests scheme test)
          (scheme comparator)
          (scheme char))

  ;; Adapted from srfi-113-test.sps7

  (begin

   (define-syntax test-assert
     (syntax-rules ()
       ((test-assert expr)
        (test expr #t))))

   (define-syntax test-deny
     (syntax-rules ()
       ((test-assert expr)
        (test expr #f))))

   (define-syntax test-error
     (syntax-rules ()
       ((test-error expr)
        (test/unspec-or-exn expr &error))))

   (define-syntax test/set
     (syntax-rules ()
      ((test/set expr expected)
       (test-assert (set=? expr expected)))))

   (define-syntax test/bag
     (syntax-rules ()
      ((test/set expr expected)
       (test-assert (bag=? expr expected)))))

   (define default-comparator (make-default-comparator))

   ;; SRFI 128 says the following definition will work, but that's
   ;; an error in SRFI 128; the hash function produce non-integers.

#; (define number-comparator
     (make-comparator real? = < (lambda (x) (exact (abs x)))))

   (define number-comparator
     (make-comparator real? = < (lambda (x) (exact (abs (round x))))))

   (define string-ci-comparator
     (make-comparator string? string-ci=? string-ci<? string-ci-hash))

   (define char-comparator
     (make-comparator char? char=? char<? char->integer))

   (define eq-comparator (make-eq-comparator))

   (define eqv-comparator (make-eqv-comparator))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (define (run-set-tests)

     ;; test-group "sets"

     (define (big x) (> x 5))

     ;; test-group "sets"

     ;; test-group "sets/simple"

     (let ()
       (define nums (set number-comparator))
       ;; nums is now {}
       (define syms (set eq-comparator 'a 'b 'c 'd))
       ;; syms is now {a, b, c, d}
       (define nums2 (set-copy nums))
       ;; nums2 is now {}
       (define syms2 (set-copy syms))
       ;; syms2 is now {a, b, c, d}
       (define esyms (set eq-comparator))
       ;; esyms is now {}
       (define total 0)
       (test-assert (set-empty? esyms))
       (test-assert (set? nums))
       (test-assert (set? syms))
       (test-assert (set? nums2))
       (test-assert (set? syms2))
       (test-assert (not (set? 'a)))
       (set-adjoin! nums 2)
       (set-adjoin! nums 3)
       (set-adjoin! nums 4)
       (set-adjoin! nums 4)
       ;; nums is now {2, 3, 4}
       (test (set-size (set-adjoin nums 5)) 4)
       (test (set-size nums) 3)
       (test (set-size (set-delete syms 'd)) 3)
       (test (set-size (set-delete-all syms '(c d))) 2)
       (test (set-size syms) 4)
       (set-adjoin! syms 'e 'f)
       ;; syms is now {a, b, c, d, e, f}
       (test (set-size (set-delete-all! syms '(e f))) 4)
       ;; syms is now {a, b, c, d}
       (test (set-size nums2) 0)
       (test (set-size syms2) 4)
       (set-delete! nums 2)
       ;; nums is now {3, 4}
       (test (set-size nums) 2)
       (set-delete! nums 1)
       (test (set-size nums) 2)
       (set! nums2 (set-map (lambda (x) (* 10 x)) number-comparator nums))
       ;; nums2 is now {30, 40}
       (test-assert (set-contains? nums2 30))
       (test-assert (not (set-contains? nums2 3)))
       (set-for-each (lambda (x) (set! total (+ total x))) nums2)
       (test total 70)
       (test (set-fold + 3 nums) 10)
       (set! nums (set eqv-comparator 10 20 30 40 50))
       ;; nums is now {10, 20, 30, 40, 50}
       (test-assert
        (set=? nums (set-unfold
                     (lambda (i) (= i 0))
                     (lambda (i) (* i 10))
                     (lambda (i) (- i 1))
                     5
                     eqv-comparator)))
       (test (set->list (set eq-comparator 'a)) '(a))
       (set! syms2 (list->set eq-comparator '(e f)))
       ;; syms2 is now {e, f}
       (test (set-size syms2) 2)
       (test-assert (set-contains? syms2 'e))
       (test-assert (set-contains? syms2 'f))
       (list->set! syms2 '(a b))
       (test (set-size syms2) 4)
       )

     ;; test-group "sets/search"

     (let ()
       (define yam (set char-comparator #\y #\a #\m))
       (define (failure/insert insert ignore)
         (insert 1))
       (define (failure/ignore insert ignore)
         (ignore 2))
       (define (success/update element update remove)
         (update #\b 3))
       (define (success/remove element update remove)
         (remove 4))
       (define yam! (set char-comparator #\y #\a #\m #\!))
       (define bam (set char-comparator #\b #\a #\m))
       (define ym (set char-comparator #\y #\m))
       (define-values (set1 obj1)
         (set-search! (set-copy yam) #\! failure/insert error))
       (define-values (set2 obj2)
         (set-search! (set-copy yam) #\! failure/ignore error))
       (define-values (set3 obj3)
         (set-search! (set-copy yam) #\y error success/update))
       (define-values (set4 obj4)
         (set-search! (set-copy yam) #\a error success/remove))
       (test-assert (set=? yam! set1))
       (test obj1 1)
       (test-assert (set=? yam set2))
       (test obj2 2)
       (test-assert (set=? bam set3))
       (test obj3 3)
       (test-assert (set=? ym set4))
       (test obj4 4)
       )

     ;; test-group "sets/subsets"

     (let ()
       (define set2 (set number-comparator 1 2))
       (define other-set2 (set number-comparator 1 2))
       (define set3 (set number-comparator 1 2 3))
       (define set4 (set number-comparator 1 2 3 4))
       (define setx (set number-comparator 10 20 30 40))
       (test-assert (set=? set2 other-set2))
       (test-assert (not (set=? set2 set3)))
       (test-assert (not (set=? set2 set3 other-set2)))
       (test-assert (set<? set2 set3 set4))
       (test-assert (not (set<? set2 other-set2)))
       (test-assert (set<=? set2 other-set2 set3))
       (test-assert (not (set<=? set2 set3 other-set2)))
       (test-assert (set>? set4 set3 set2))
       (test-assert (not (set>? set2 other-set2)))
       (test-assert (set>=? set3 other-set2 set2))
       (test-assert (not (set>=? other-set2 set3 set2)))
       )

     ;; test-group "sets/ops"

     (let ()
  
       ;; Potentially mutable
  
       (define abcd (set eq-comparator 'a 'b 'c 'd))
       (define efgh (set eq-comparator 'e 'f 'g 'h))
       (define abgh (set eq-comparator 'a 'b 'g 'h))

       ;; Never get a chance to be mutated

       (define other-abcd (set eq-comparator 'a 'b 'c 'd))
       (define other-efgh (set eq-comparator 'e 'f 'g 'h))
       (define other-abgh (set eq-comparator 'a 'b 'g 'h))
       (define all (set eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
       (define none (set eq-comparator))
       (define ab (set eq-comparator 'a 'b))
       (define cd (set eq-comparator 'c 'd))
       (define ef (set eq-comparator 'e 'f))
       (define gh (set eq-comparator 'g 'h))
       (define cdgh (set eq-comparator 'c 'd 'g 'h))
       (define abcdgh (set eq-comparator 'a 'b 'c 'd 'g 'h))
       (define abefgh (set eq-comparator 'a 'b 'e 'f 'g 'h))
       (define efgh2 (set-copy efgh))
       (define abcd2 (set-copy abcd))
       (define abcd3 (set-copy abcd))
       (define abcd4 (set-copy abcd))
       (test-assert (set-disjoint? abcd efgh))
       (test-assert (not (set-disjoint? abcd ab)))
       (test/set (set-union abcd efgh) all)
       (test/set (set-union abcd abgh) abcdgh)
       (test/set (set-union efgh abgh) abefgh)
       (set-union! efgh2 abgh)
       (test/set efgh2 abefgh)
       (test/set (set-intersection abcd efgh) none)
       (set-intersection! abcd2 efgh)
       (test/set abcd2 none)
       (test/set (set-intersection abcd abgh) ab)
       (test/set (set-intersection abgh abcd) ab)
       (test/set (set-difference abcd ab) cd)
       (test/set (set-difference abcd gh) abcd)
       (test/set (set-difference abcd abcd) none)
       (set-difference! abcd3 abcd)
       (test/set abcd3 none)
       (test/set (set-xor abcd abgh) cdgh)
       (test/set (set-xor abcd efgh) all)
       (test/set (set-xor abcd other-abcd) none)
       ;; don't test xor! effect
       (test/set (set-xor! abcd4 other-abcd) none)
       (test/set abcd other-abcd)
       (test/set efgh other-efgh)
       (test/set abgh other-abgh)
       )

     ;; test-group "sets/mismatch"

     (let ()
       (define nums (set number-comparator 1 2 3))
       (define syms (set eq-comparator 'a 'b 'c))
       (test-error (set=? nums syms))
       (test-error (set<? nums syms))
       (test-error (set<=? nums syms))
       (test-error (set>? nums syms))
       (test-error (set>=? nums syms))
       (test-error (set-union nums syms))
       (test-error (set-intersection nums syms))
       (test-error (set-difference nums syms))
       (test-error (set-xor nums syms))
       (test-error (set-union! nums syms))
       (test-error (set-intersection! nums syms))
       (test-error (set-difference! nums syms))
       (test-error (set-xor! nums syms))
       )

     ;; test-group "sets/whole"

     (let ()
       (define whole (set eqv-comparator 1 2 3 4 5 6 7 8 9 10))
       (define whole2 (set-copy whole))
       (define whole3 (set-copy whole))
       (define whole4 (set-copy whole))
       (define bottom (set eqv-comparator 1 2 3 4 5))
       (define top (set eqv-comparator 6 7 8 9 10))
       (define-values (topx bottomx)
         (set-partition big whole))
       (define hetero (set eqv-comparator 1 2 'a 3 4))
       (define homo (set eqv-comparator 1 2 3 4 5))
       (set-partition! big whole4)
       (test/set (set-filter big whole) top)
       (test/set (set-remove big whole) bottom)
       (set-filter! big whole2)
       (test-assert (not (set-contains? whole2 1)))
       (set-remove! big whole3)
       (test-assert (not (set-contains? whole3 10)))
       (test/set topx top)
       (test/set bottomx bottom)
       (test/set whole4 top)
       (test (set-count big whole) 5)
       (test (set-find symbol? hetero (lambda () (error "wrong"))) 'a)
       (test-error  (set-find symbol? homo (lambda () (error "wrong"))))
       (test-assert (set-any? symbol? hetero))
       (test-assert (set-any? number? hetero))
       (test-assert (not (set-every? symbol? hetero)))
       (test-assert (not (set-every? number? hetero)))
       (test-assert (not (set-any? symbol? homo)))
       (test-assert (set-every? number? homo))
       )

     ;; test-group "sets/lowlevel"

     (let ()
       (define bucket (set string-ci-comparator "abc" "def"))
       (define nums (set number-comparator 1 2 3))
       ;; nums is now {1, 2, 3}
       (define nums2 (set-replace nums 2.0))
       ;; nums2 is now {1, 2.0, 3}
       (define sos
         (set set-comparator
              (set eqv-comparator 1 2)
              (set eqv-comparator 1 2)))
       (test (set-element-comparator bucket) string-ci-comparator)
       (test-assert (set-contains? bucket "abc"))
       (test-assert (set-contains? bucket "ABC"))
       (test (set-member bucket "DEF" "fqz") "def")
       (test (set-member bucket "lmn" "fqz") "fqz")
       (test-assert (set-any? inexact? nums2))
       (set-replace! nums 2.0)
       ;; nums is now {1, 2.0, 3}
       (test-assert (set-any? inexact? nums))
       (test (set-size sos) 1)
       )

     ;; test-group "bags"

     ;; test-group "bags/simple"

     (let ()
       (define nums (bag number-comparator))
       ;; nums is now {}
       (define syms (bag eq-comparator 'a 'b 'c 'd))
       ;; syms is now {a, b, c, d}
       (define nums2 (bag-copy nums))
       ;; nums2 is now {}
       (define syms2 (bag-copy syms))
       ;; syms2 is now {a, b, c, d}
       (define esyms (bag eq-comparator))
       ;; esyms is now {}
       (define total 0)
       (test-assert (bag-empty? esyms))
       (test-assert (bag? nums))
       (test-assert (bag? syms))
       (test-assert (bag? nums2))
       (test-assert (bag? syms2))
       (test-assert (not (bag? 'a)))
       (bag-adjoin! nums 2)
       (bag-adjoin! nums 3)
       (bag-adjoin! nums 4)
       ;; nums is now {2, 3, 4}
       (test (bag-size (bag-adjoin nums 5)) 4)
       (test (bag-size nums) 3)
       (test (bag-size (bag-delete syms 'd)) 3)
       (test (bag-size (bag-delete-all syms '(c d))) 2)
       (test (bag-size syms) 4)
       (bag-adjoin! syms 'e 'f)
       ;; syms is now {a, b, c, d, e, f}
       (test (bag-size (bag-delete-all! syms '(e f))) 4)
       ;; syms is now {a, b, c, d}
       (test (bag-size nums) 3)
       (bag-delete! nums 1)
       (test (bag-size nums) 3)
       (set! nums2 (bag-map (lambda (x) (* 10 x)) number-comparator nums))
       ;; nums2 is now {20, 30, 40}
       (test-assert (bag-contains? nums2 30))
       (test-assert (not (bag-contains? nums2 3)))
       (bag-for-each (lambda (x) (set! total (+ total x))) nums2)
       (test total 90)
       (test (bag-fold + 3 nums) 12)
       (set! nums (bag eqv-comparator 10 20 30 40 50))
       ;; nums is now {10, 20, 30, 40, 50}
       (test-assert
        (bag=? nums (bag-unfold
                     (lambda (i) (= i 0))
                     (lambda (i) (* i 10))
                     (lambda (i) (- i 1))
                     5
                     eqv-comparator)))
       (test (bag->list (bag eq-comparator 'a)) '(a))
       (set! syms2 (list->bag eq-comparator '(e f)))
       ;; syms2 is now {e, f}
       (test (bag-size syms2) 2)
       (test-assert (bag-contains? syms2 'e))
       (test-assert (bag-contains? syms2 'f))
       (list->bag! syms2 '(e f))
       ;; syms2 is now {e, e, f, f}
       (test (bag-size syms2) 4)
       )

     ;; test-group "bags/search"

     (let ()
       (define yam (bag char-comparator #\y #\a #\m))
       (define (failure/insert insert ignore)
         (insert 1))
       (define (failure/ignore insert ignore)
         (ignore 2))
       (define (success/update element update remove)
         (update #\b 3))
       (define (success/remove element update remove)
         (remove 4))
       (define yam! (bag char-comparator #\y #\a #\m #\!))
       (define bam (bag char-comparator #\b #\a #\m))
       (define ym (bag char-comparator #\y #\m))
       (define-values (bag1 obj1)
         (bag-search! (bag-copy yam) #\! failure/insert error))
       (define-values (bag2 obj2)
         (bag-search! (bag-copy yam) #\! failure/ignore error))
       (define-values (bag3 obj3)
         (bag-search! (bag-copy yam) #\y error success/update))
       (define-values (bag4 obj4)
         (bag-search! (bag-copy yam) #\a error success/remove))
       (test-assert (bag=? yam! bag1))
       (test obj1 1)
       (test-assert (bag=? yam bag2))
       (test obj2 2)
       (test-assert (bag=? bam bag3))
       (test obj3 3)
       (test-assert (bag=? ym bag4))
       (test obj4 4)
       )

     ;; test-group "bags/elemcount"

     (let ()
       (define mybag (bag eqv-comparator 1 1 1 1 1 2 2))
       (test (bag-element-count mybag 1) 5)
       (test (bag-element-count mybag 3) 0)
       )

     ;; test-group "bags/subbags"

     (let ()
       (define bag2 (bag number-comparator 1 2))
       (define other-bag2 (bag number-comparator 1 2))
       (define bag3 (bag number-comparator 1 2 3))
       (define bag4 (bag number-comparator 1 2 3 4))
       (define bagx (bag number-comparator 10 20 30 40))
       (test-assert (bag=? bag2 other-bag2))
       (test-assert (not (bag=? bag2 bag3)))
       (test-assert (not (bag=? bag2 bag3 other-bag2)))
       (test-assert (bag<? bag2 bag3 bag4))
       (test-assert (not (bag<? bag2 other-bag2)))
       (test-assert (bag<=? bag2 other-bag2 bag3))
       (test-assert (not (bag<=? bag2 bag3 other-bag2)))
       (test-assert (bag>? bag4 bag3 bag2))
       (test-assert (not (bag>? bag2 other-bag2)))
       (test-assert (bag>=? bag3 other-bag2 bag2))
       (test-assert (not (bag>=? other-bag2 bag3 bag2)))
       )

     ;; test-group "bags/multi"

     (let ()
       (define one (bag eqv-comparator 10))
       (define two (bag eqv-comparator 10 10))
       (test-assert (not (bag=? one two)))
       (test-assert (bag<? one two))
       (test-assert (not (bag>? one two)))
       (test-assert (bag<=? one two))
       (test-assert (not (bag>? one two)))
       (test-assert (bag=? two two))
       (test-assert (not (bag<? two two)))
       (test-assert (not (bag>? two two)))
       (test-assert (bag<=? two two))
       (test-assert (bag>=? two two))
       (test (let ((result '()))
               (bag-for-each-unique
                (lambda (x y) (set! result (cons (cons x y) result)))
                two)
               result)
             '((10 . 2)))
       (test (bag-fold + 5 two) 25)
       (test (bag-fold-unique (lambda (k n r) (+ k n r)) 0 two) 12)
       )

     ;; test-group "bags/ops"

     (let ()
       ;; Potentially mutable
       (define abcd (bag eq-comparator 'a 'b 'c 'd))
       (define efgh (bag eq-comparator 'e 'f 'g 'h))
       (define abgh (bag eq-comparator 'a 'b 'g 'h))
       ;; Never get a chance to be mutated
       (define other-abcd (bag eq-comparator 'a 'b 'c 'd))
       (define other-efgh (bag eq-comparator 'e 'f 'g 'h))
       (define other-abgh (bag eq-comparator 'a 'b 'g 'h))
       (define all (bag eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
       (define none (bag eq-comparator))
       (define ab (bag eq-comparator 'a 'b))
       (define cd (bag eq-comparator 'c 'd))
       (define ef (bag eq-comparator 'e 'f))
       (define gh (bag eq-comparator 'g 'h))
       (define cdgh (bag eq-comparator 'c 'd 'g 'h))
       (define abcdgh (bag eq-comparator 'a 'b 'c 'd 'g 'h))
       (define abefgh (bag eq-comparator 'a 'b 'e 'f 'g 'h))
       (define efgh2 (bag-copy efgh))
       (define abcd2 (bag-copy abcd))
       (define abcd3 (bag-copy abcd))
       (define abcd4 (bag-copy abcd))
       (define abab (bag eq-comparator 'a 'b 'a 'b))
       (define ab2 (bag-copy ab))
       (define ab3 (bag-copy ab))
       (test-assert (bag-disjoint? abcd efgh))
       (test-assert (not (bag-disjoint? abcd ab)))
       (test/bag (bag-union abcd efgh) all)
       (test/bag (bag-union abcd abgh) abcdgh)
       (test/bag (bag-union efgh abgh) abefgh)
       (bag-union! efgh2 abgh)
       (test/bag efgh2 abefgh)
       (test/bag (bag-intersection abcd efgh) none)
       (bag-intersection! abcd2 efgh)
       (test/bag abcd2 none)
       (test/bag (bag-intersection abcd abgh) ab)
       (test/bag (bag-intersection abgh abcd) ab)
       (test/bag (bag-difference abcd ab) cd)
       (test/bag (bag-difference abcd gh) abcd)
       (test/bag (bag-difference abcd abcd) none)
       (bag-difference! abcd3 abcd)
       (test/bag abcd3 none)
       (test/bag (bag-xor abcd abgh) cdgh)
       (test/bag (bag-xor abcd efgh) all)
       (test/bag (bag-xor abcd other-abcd) none)
       (test/bag (bag-xor! abcd4 other-abcd) none)
       (test/bag (bag-sum! ab2 ab) abab)
       (test/bag ab2 abab)
       (test/bag (bag-product 2 ab) abab)
       (bag-product! 2 ab3)
       (test/bag ab3 abab)
       (test/bag abcd other-abcd)
       (test/bag abcd other-abcd)
       (test/bag efgh other-efgh)
       (test/bag abgh other-abgh)
       )

     ;; test-group "bags/mismatch"

     (let ()
       (define nums (bag number-comparator 1 2 3))
       (define syms (bag eq-comparator 'a 'b 'c))
       (test-error (bag=? nums syms))
       (test-error (bag<? nums syms))
       (test-error (bag<=? nums syms))
       (test-error (bag>? nums syms))
       (test-error (bag>=? nums syms))
       (test-error (bag-union nums syms))
       (test-error (bag-intersection nums syms))
       (test-error (bag-difference nums syms))
       (test-error (bag-xor nums syms))
       (test-error (bag-union! nums syms))
       (test-error (bag-intersection! nums syms))
       (test-error (bag-difference! nums syms))
       )

     ;; test-group "bags/whole"

     (let ()
       (define whole (bag eqv-comparator 1 2 3 4 5 6 7 8 9 10))
       (define whole2 (bag-copy whole))
       (define whole3 (bag-copy whole))
       (define whole4 (bag-copy whole))
       (define bottom (bag eqv-comparator 1 2 3 4 5))
       (define top (bag eqv-comparator 6 7 8 9 10))
       (define-values (topx bottomx)
         (bag-partition big whole))
       (define hetero (bag eqv-comparator 1 2 'a 3 4))
       (define homo (bag eqv-comparator 1 2 3 4 5))
       (bag-partition! big whole4)
       (test/bag (bag-filter big whole) top)
       (test/bag (bag-remove big whole) bottom)
       (bag-filter! big whole2)
       (test-assert (not (bag-contains? whole2 1)))
       (bag-remove! big whole3)
       (test-assert (not (bag-contains? whole3 10)))
       (test/bag topx top)
       (test/bag bottomx bottom)
       (test/bag whole4 top)
       (test (bag-count big whole) 5)
       (test (bag-find symbol? hetero (lambda () (error "wrong"))) 'a)
       (test-error  (bag-find symbol? homo (lambda () (error "wrong"))))
       (test-assert (bag-any? symbol? hetero))
       (test-assert (bag-any? number? hetero))
       (test-assert (not (bag-every? symbol? hetero)))
       (test-assert (not (bag-every? number? hetero)))
       (test-assert (not (bag-any? symbol? homo)))
       (test-assert (bag-every? number? homo))
       )

     ;; test-group "bags/lowlevel"

     (let ()
       (define bucket (bag string-ci-comparator "abc" "def"))
       (define nums (bag number-comparator 1 2 3))
       ;; nums is now {1, 2, 3}
       (define nums2 (bag-replace nums 2.0))
       ;; nums2 is now {1, 2.0, 3}
       (define bob
         (bag bag-comparator
              (bag eqv-comparator 1 2)
              (bag eqv-comparator 1 2)))
       (test (bag-element-comparator bucket) string-ci-comparator)
       (test-assert (bag-contains? bucket "abc"))
       (test-assert (bag-contains? bucket "ABC"))
       (test (bag-member bucket "DEF" "fqz") "def")
       (test (bag-member bucket "lmn" "fqz") "fqz")
       (test-assert (bag-any? inexact? nums2))
       (bag-replace! nums 2.0)
       ;; nums is now {1, 2.0, 3}
       (test-assert (bag-any? inexact? nums))
       (test (bag-size bob) 2)
       )

     ;; test-group "bags/semantics"

     (let ()
       (define mybag (bag number-comparator 1 2))
       ;; mybag is {1, 2}
       (test (bag-size mybag) 2)
       (bag-adjoin! mybag 1)
       ;; mybag is {1, 1, 2}
       (test (bag-size mybag) 3)
       (test (bag-unique-size mybag) 2)
       (bag-delete! mybag 2)
       ;; mybag is {1, 1}
       (bag-delete! mybag 2)
       (test (bag-size mybag) 2)
       (bag-increment! mybag 1 3)
       ;; mybag is {1, 1, 1, 1, 1}
       (test (bag-size mybag) 5)
       (bag-decrement! mybag 1 2)
       ;; mybag is {1, 1, 1}
       (test (bag-size mybag) 3)
       (bag-decrement! mybag 1 5)
       ;; mybag is {}
       (test (bag-size mybag) 0)
       )

     ;; test-group "bags/convert"

     (let ()
       (define multi (bag eqv-comparator 1 2 2 3 3 3))
       (define single (bag eqv-comparator 1 2 3))
       (define singleset (set eqv-comparator 1 2 3))
       (define minibag (bag eqv-comparator 'a 'a))
       (define alist '((a . 2)))
       (test (bag->alist minibag) alist)
       (test-assert (bag=? minibag (alist->bag eqv-comparator alist)))
       (test-assert (set=? singleset (bag->set single)))
       (test-assert (set=? singleset (bag->set multi)))
       (test-assert (bag=? single (set->bag singleset)))
       (test-assert (not (bag=? multi (set->bag singleset))))
       (set->bag! minibag singleset)
       ;; minibag is now {a, a, a, a, 1, 2, 3}
       (test-assert (bag-contains? minibag 1))
       )

     ;; test-group "bags/sumprod"

     (let ()
       (define abb (bag eq-comparator 'a 'b 'b))
       (define aab (bag eq-comparator 'a 'a 'b))
       (define total (bag-sum abb aab))
       (define bag1 (bag eqv-comparator 1))
       (test (bag-count (lambda (x) (eqv? x 'a)) total) 3)
       (test (bag-count (lambda (x) (eqv? x 'b)) total) 3)
       (test (bag-size (bag-product 2 total)) 12)
       (bag-sum! bag1 bag1)
       (test (bag-size bag1) 2)
       (bag-product! 2 bag1)
       (test (bag-size bag1) 4)
       )

     ;; test-group "comparators"

     (let ()
       (define a (set number-comparator 1 2 3))
       (define b (set number-comparator 1 2 4))
       (define aa (bag number-comparator 1 2 3))
       (define bb (bag number-comparator 1 2 4))
       (test-assert (not (=? set-comparator a b)))
       (test-assert (=? set-comparator a (set-copy a)))
       (test-error (<? set-comparator a b))
       (test-assert (not (=? bag-comparator aa bb)))
       (test-assert (=? bag-comparator aa (bag-copy aa)))
       (test-error (<? bag-comparator aa bb))
       (test-assert (not (=? default-comparator a aa)))
       )

     ;; test-group "regression"

     ;; test-group "Larceny ticket #721"

     (let* ((S1 (set default-comparator 1 2))
            (S2 (set default-comparator 2 3))
            (S (set-xor! S1 S2)))
       (test-assert (set=? S (list->set default-comparator '(1 3)))))

     (let* ((S0 (set default-comparator))
            (S2 (set default-comparator 2 3))
            (S (set-xor! S0 S2)))
       (test-assert (set=? S (list->set default-comparator '(2 3)))))

     (let* ((B1 (bag default-comparator 1 2))
            (B2 (bag default-comparator 2 3))
            (B (bag-xor! B1 B2)))
       (test-assert (bag=? B (set->bag (list->set default-comparator '(1 3))))))

     (let* ((B0 (bag default-comparator))
            (B2 (bag default-comparator 2 3))
            (B (bag-xor! B0 B2)))
       (test-assert (bag=? B (set->bag (list->set default-comparator '(2 3))))))

     )))
