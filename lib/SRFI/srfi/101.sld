;; SRFI 101: Purely Functional Random-Access Pairs and Lists
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

(define-library (srfi 101 random-access-lists)
  (export quote
          pair?
          cons
          car
          cdr
          caar
          cadr
          cddr
          cdar
          caaar
          caadr
          caddr
          cadar
          cdaar
          cdadr
          cdddr
          cddar
          caaaar
          caaadr
          caaddr
          caadar
          cadaar
          cadadr
          cadddr
          caddar
          cdaaar
          cdaadr
          cdaddr
          cdadar
          cddaar
          cddadr
          cddddr
          cdddar
          null?
          list?
          list
          make-list
          length
          append
          reverse
          list-tail
          list-ref
          list-set
          list-ref/update
          map
          for-each
          random-access-list->linear-access-list
          linear-access-list->random-access-list
          )
  
  (import (srfi :101 random-access-lists))
      
  ) ; (srfi 101 random-access-lists)


(define-library (srfi 101)
  (export quote
          pair?
          cons
          car
          cdr
          caar
          cadr
          cddr
          cdar
          caaar
          caadr
          caddr
          cadar
          cdaar
          cdadr
          cdddr
          cddar
          caaaar
          caaadr
          caaddr
          caadar
          cadaar
          cadadr
          cadddr
          caddar
          cdaaar
          cdaadr
          cdaddr
          cdadar
          cddaar
          cddadr
          cddddr
          cdddar
          null?
          list?
          list
          make-list
          length
          append
          reverse
          list-tail
          list-ref
          list-set
          list-ref/update
          map
          for-each
          random-access-list->linear-access-list
          linear-access-list->random-access-list
          )
  
  (import (srfi 101 random-access-lists))
      
  ) ; (srfi 101)
