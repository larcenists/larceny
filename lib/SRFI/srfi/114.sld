;; SRFI 114: Comparators
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

(define-library (srfi 114 comparators)
  (export comparator? comparator-comparison-procedure? 
          comparator-hash-function?
          boolean-comparator char-comparator char-ci-comparator 
          string-comparator string-ci-comparator symbol-comparator
          exact-integer-comparator integer-comparator rational-comparator
          real-comparator complex-comparator number-comparator
          pair-comparator list-comparator vector-comparator
          bytevector-comparator
          default-comparator
          make-comparator make-inexact-real-comparator make-vector-comparator 
          make-bytevector-comparator make-list-comparator
          make-vectorwise-comparator make-listwise-comparator
          make-car-comparator make-cdr-comparator make-pair-comparator
          make-improper-list-comparator make-selecting-comparator
          make-refining-comparator make-reverse-comparator
          make-debug-comparator
          eq-comparator eqv-comparator equal-comparator
          comparator-type-test-procedure comparator-equality-predicate 
          comparator-comparison-procedure comparator-hash-function
          comparator-test-type comparator-check-type comparator-equal? 
          comparator-compare comparator-hash
          make-comparison< make-comparison> make-comparison<=
          make-comparison>= make-comparison=/< make-comparison=/>
          if3 if=? if<? if>? if<=? if>=? if-not=?
          =? <? >? <=? >=?
          make= make<  make> make<= make>=
          in-open-interval? in-closed-interval? in-open-closed-interval? 
          in-closed-open-interval?
          comparator-min comparator-max
          comparator-register-default!)

  (import (scheme base)
          (scheme char)
          (scheme case-lambda)
          (scheme complex)
          (scheme inexact)
          (srfi 128 kernel)
          (srfi 128))

  (include "114.basics.scm")
  (include "114.default.scm")
  (include "114.constructors.scm")
  (include "114.advanced.scm")

)


(define-library (srfi 114)
  (export comparator? comparator-comparison-procedure? 
          comparator-hash-function?
          boolean-comparator char-comparator char-ci-comparator 
          string-comparator string-ci-comparator symbol-comparator
          exact-integer-comparator integer-comparator rational-comparator
          real-comparator complex-comparator number-comparator
          pair-comparator list-comparator vector-comparator
          bytevector-comparator
          default-comparator
          make-comparator make-inexact-real-comparator make-vector-comparator 
          make-bytevector-comparator make-list-comparator
          make-vectorwise-comparator make-listwise-comparator
          make-car-comparator make-cdr-comparator make-pair-comparator
          make-improper-list-comparator make-selecting-comparator
          make-refining-comparator make-reverse-comparator
          make-debug-comparator
          eq-comparator eqv-comparator equal-comparator
          comparator-type-test-procedure comparator-equality-predicate 
          comparator-comparison-procedure comparator-hash-function
          comparator-test-type comparator-check-type comparator-equal? 
          comparator-compare comparator-hash
          make-comparison< make-comparison> make-comparison<=
          make-comparison>= make-comparison=/< make-comparison=/>
          if3 if=? if<? if>? if<=? if>=? if-not=?
          =? <? >? <=? >=?
          make= make<  make> make<= make>=
          in-open-interval? in-closed-interval? in-open-closed-interval? 
          in-closed-open-interval?
          comparator-min comparator-max
          comparator-register-default!)

  (import (srfi 114 comparators))

)

; eof
