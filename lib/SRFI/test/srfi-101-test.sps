#!r6rs

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

;; This test suite has been successfully run on Ikarus (0.0.3),
;; Larceny (0.97), and PLT Scheme (4.2.1.7).

;; To run:
;;    cp srfi-101.sls srfi/%3A101.sls
;;    ikarus --r6rs-script srfi-101-tests.sls
;;    larceny -r6rs -path . -program srfi-101-tests.sls
;;    plt-r6rs ++path . srfi-101-tests.sls

(import (rename (only (scheme base)
                      include begin define for-each newline)
                (for-each r7:for-each))
        (only (scheme write)
              display)
        (except (rnrs base)
                quote pair? cons car cdr 
                caar cadr cddr cdar
                caaar caadr caddr cadar
                cdaar cdadr cdddr cddar
                caaaar caaadr caaddr caadar
                cadaar cadadr cadddr caddar
                cdaaar cdaadr cdaddr cdadar
                cddaar cddadr cddddr cdddar
                null? list? list length 
                append reverse list-tail
                list-ref map for-each)
        (prefix (rnrs base) r6:)
        (rnrs exceptions)
        (srfi :101))

(include "srfi-101-test.body.scm")

(begin
 (define (writeln . xs)
   (r7:for-each display xs)
   (newline))

 (writeln "Done."))
