;;; Copyright (C) Shiro Kawai, John Cowan, Thomas Gilray (2015).
;;; All Rights Reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(define-library (srfi 121)
  (export generator make-iota-generator make-range-generator 
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator
          gcons* gappend gcombine gfilter gremove 
          gtake gdrop gtake-while gdrop-while
          gdelete gdelete-neighbor-dups gindex gselect
          generator->list generator->reverse-list
          generator->vector generator->vector!  generator->string
          generator-fold generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold
          )
  (import (scheme base)
          (scheme case-lambda))

  (include "121.body.scm")
)
