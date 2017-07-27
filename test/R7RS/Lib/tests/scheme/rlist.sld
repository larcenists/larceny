;; Copyright (c) David Van Horn 2009.  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. REMEMBER, THERE IS NO SCHEME UNDERGROUND. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
;; THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; The copyright notice above is taken from srfi-101-test.sps7,
;;; from which this file is derived.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme rlist) procedures:
;;;
;;;     rquote
;;;     rpair?
;;;     rcons
;;;     rcar
;;;     rcdr
;;;     rcaar
;;;     rcadr
;;;     rcddr
;;;     rcdar
;;;     rcaaar
;;;     rcaadr
;;;     rcaddr
;;;     rcadar
;;;     rcdaar
;;;     rcdadr
;;;     rcdddr
;;;     rcddar
;;;     rcaaaar
;;;     rcaaadr
;;;     rcaaddr
;;;     rcaadar
;;;     rcadaar
;;;     rcadadr
;;;     rcadddr
;;;     rcaddar
;;;     rcdaaar
;;;     rcdaadr
;;;     rcdaddr
;;;     rcdadar
;;;     rcddaar
;;;     rcddadr
;;;     rcddddr
;;;     rcdddar
;;;     rnull?
;;;     rlist?
;;;     rlist
;;;     make-rlist
;;;     rlength
;;;     rappend
;;;     rreverse
;;;     rlist-tail
;;;     rlist-ref
;;;     rlist-set
;;;     rlist-ref/update
;;;     rmap
;;;     rfor-each
;;;     rlist->list
;;;     list->rlist


(define-library (tests scheme rlist)
  (export run-rlist-tests)
  (import (scheme base)
          (scheme rlist)
          (tests scheme test))

  ;; Adapted from srfi-101-test.sps7

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

   (define-syntax rtest
     (syntax-rules ()
      ((rtest expr expected)
       (test-assert (rtest-equal? expr expected)))))

   (define (rtest-equal? c e)
     (if (rpair? c)
         (and (rtest-equal? (rcar c)
                            (rcar e))
              (rtest-equal? (rcdr c)
                            (rcdr e)))
         (equal? c e)))

   (define (run-rlist-tests)

     ;; FIXME: no tests for rcaar etc

     ;; rquote  
     (rtest (rquote 5) (quote 5))
     (rtest (rquote x) (quote x))

     (rtest (let ((f (lambda () '(x))))
              (eq? (f) (f)))
            #t)

     (rtest (rquote (1 2 3)) (rlist 1 2 3))

     ;; rpair?
     (rtest (rpair? (rcons 'a 'b)) #t)
     (rtest (rpair? (rlist 'a 'b 'c)) #t)
     (rtest (rpair? '()) #f)
     (rtest (rpair? '#(a b)) #f)

     ;; rcons
     (rtest (rcons 'a '()) (rlist 'a))
     (rtest (rcons (rlist 'a) (rlist 'b 'c 'd))
            (rlist (rlist 'a) 'b 'c 'd))
     (rtest (rcons "a" (rlist 'b 'c))
            (rlist "a" 'b 'c))
     (rtest (rcons 'a 3)
            (rcons 'a 3))
     (rtest (rcons (rlist 'a 'b) 'c)
            (rcons (rlist 'a 'b) 'c))

     ;; rcar
     (rtest (rcar (rlist 'a 'b 'c))
            'a)
     (rtest (rcar (rlist (rlist 'a) 'b 'c 'd))
            (rlist 'a))
     (rtest (rcar (rcons 1 2)) 1)
     (test/unspec-or-exn (rcar '()) &error)

     ;; rcdr
     (rtest (rcdr (rlist (rlist 'a) 'b 'c 'd))
            (rlist 'b 'c 'd))
     (rtest (rcdr (rcons 1 2))
            2)
     (test/unspec-or-exn (rcdr '()) &error)

     ;; rcaar etc
     (let ((x (rlist 0 1 2 3 4 5 6))
           (y (rlist (rlist (rlist (rlist 'a 'b 'c) 'd 'e) (rlist 'f) 'g)
                     (rlist 1) 2 3 4 5 6))
           (z (rlist 0 (rlist (rlist 'i 'j) 'k) 2 3 4 5 6))
           (zz (rlist 0 1 (rlist (rlist 'm 'n) 'o) 'p 'q)))
        (rtest (rcaar y) (rlist (rlist 'a 'b 'c) 'd 'e))
        (rtest (rcadr x) 1)
        (rtest (rcddr x) (rlist 2 3 4 5 6))
        (rtest (rcdar y) (rlist (rlist 'f) 'g))
        (rtest (rcaaar y) (rlist 'a 'b 'c))
        (rtest (rcaadr z) (rlist 'i 'j))
        (rtest (rcaddr x) 2)
        (rtest (rcadar y) (rlist 'f))
        (rtest (rcdaar y) (rlist 'd 'e))
        (rtest (rcdadr z) (rlist 'k))
        (rtest (rcdddr x) (rlist 3 4 5 6))
        (rtest (rcddar y) (rlist 'g))
        (rtest (rcaaaar y) 'a)
        (rtest (rcaaadr z) 'i)
        (rtest (rcaaddr zz) (rlist 'm 'n))
        (rtest (rcaadar y) 'f)
        (rtest (rcadaar y) 'd)
        (rtest (rcadadr z) 'k)
        (rtest (rcadddr x) 3)
        (rtest (rcaddar y) 'g)
        (rtest (rcdaaar y) (rlist 'b 'c))
        (rtest (rcdaadr z) (rlist 'j))
        (rtest (rcdaddr zz) (rlist 'o))
        (rtest (rcdadar y) '())
        (rtest (rcddaar y) (rlist 'e))
        (rtest (rcddadr z) '())
        (rtest (rcddddr x) (rlist 4 5 6))
        (rtest (rcdddar y) '()))

     ;; rnull?
     (rtest (eq? rnull? null?) #t)
     (rtest (rnull? '()) #t)
     (rtest (rnull? (rcons 1 2)) #f)
     (rtest (rnull? 4) #f)

     ;; rlist?
     (rtest (rlist? (rlist 'a 'b 'c)) #t)
     (rtest (rlist? '()) #t)
     (rtest (rlist? (rcons 'a 'b)) #f)

     ;; rlist
     (rtest (rlist 'a (+ 3 4) 'c)
            (rlist 'a 7 'c))
     (rtest (rlist) '())

     ;; make-rlist
     (rtest (rlength (make-rlist 5)) 5)
     (rtest (make-rlist 5 0)
            (rlist 0 0 0 0 0))

     ;; rlength
     (rtest (rlength (rlist 'a 'b 'c)) 3)
     (rtest (rlength (rlist 'a (rlist 'b) (rlist 'c))) 3)
     (rtest (rlength '()) 0)

     ;; rappend
     (rtest (rappend (rlist 'x) (rlist 'y)) (rlist 'x 'y))
     (rtest (rappend (rlist 'a) (rlist 'b 'c 'd)) (rlist 'a 'b 'c 'd))
     (rtest (rappend (rlist 'a (rlist 'b)) (rlist (rlist 'c))) 
            (rlist 'a (rlist 'b) (rlist 'c)))
     (rtest (rappend (rlist 'a 'b) (rcons 'c 'd)) 
            (rcons 'a (rcons 'b (rcons 'c 'd))))
     (rtest (rappend '() 'a) 'a)

     ;; rreverse
     (rtest (rreverse (rlist 'a 'b 'c))
            (rlist 'c 'b 'a))
     (rtest (rreverse (rlist 'a (rlist 'b 'c) 'd (rlist 'e (rlist 'f))))
            (rlist (rlist 'e (rlist 'f)) 'd (rlist 'b 'c) 'a))

     ;; rlist-tail
     (rtest (rlist-tail (rlist 'a 'b 'c 'd) 2)
            (rlist 'c 'd))

     ;; rlist-ref
     (rtest (rlist-ref (rlist 'a 'b 'c 'd) 2) 'c)

     ;; rlist-set
     (rtest (rlist-set (rlist 'a 'b 'c 'd) 2 'x)
            (rlist 'a 'b 'x 'd))

     ;; rlist-ref/update
     (let-values (((a b) 
                   (rlist-ref/update (rlist 7 8 9 10) 2 -)))
       (rtest a 9)
       (rtest b (rlist 7 8 -9 10)))

     ;; rmap
     (rtest (rmap rcadr (rlist (rlist 'a 'b) (rlist 'd 'e) (rlist 'g 'h)))
            (rlist 'b 'e 'h))
     (rtest (rmap (lambda (n) (expt n n))
                  (rlist 1 2 3 4 5))
            (rlist 1 4 27 256 3125))
     (rtest (rmap + (rlist 1 2 3) (rlist 4 5 6))
            (rlist 5 7 9))

     ;; rfor-each
     (rtest (let ((v (make-vector 5)))
              (rfor-each (lambda (i)
                           (vector-set! v i (* i i)))
                         (rlist 0 1 2 3 4))
              v)
            '#(0 1 4 9 16))

     ;; rlist->list
     ;; list->rlist
     (rtest (rlist->list '()) '())
     (rtest (list->rlist '()) '())

     (rtest (rlist->list (rlist 1 2 3))
            (list 1 2 3))

     (rtest (list->rlist (list 1 2 3))
            (rlist 1 2 3)))))
