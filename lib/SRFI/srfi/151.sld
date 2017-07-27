;;; SRFI 151 (bitwise operations)
;;;
;;; $Id$
;;;
;;; Copyright (C) William D Clinger (2017).
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


(define-library (srfi 151)

  (export

   bitwise-not
   bitwise-and
   bitwise-ior 
   bitwise-xor
   bitwise-eqv                ; not R6RS
   bitwise-nand               ; not R6RS
   bitwise-nor                ; not R6RS
   bitwise-andc1              ; not R6RS
   bitwise-andc2              ; not R6RS
   bitwise-orc1               ; not R6RS
   bitwise-orc2               ; not R6RS

   arithmetic-shift           ; renaming of R6RS procedure
   bit-count                  ; renaming of R6RS procedure
   integer-length             ; renaming of R6RS procedure
   bitwise-if

   bit-set?                   ; not R6RS
   copy-bit                   ; not R6RS
   bit-swap                   ; not R6RS
   any-bit-set?               ; not R6RS
   every-bit-set?             ; not R6RS
   first-set-bit              ; renaming of R6RS procedure

   bit-field                  ; renaming of R6RS procedure
   bit-field-any?             ; not R6RS
   bit-field-every?           ; not R6RS
   bit-field-clear            ; not R6RS
   bit-field-set              ; not R6RS
   bit-field-replace          ; not R6RS
   bit-field-replace-same     ; not R6RS
   bit-field-rotate           ; not R6RS
   bit-field-reverse          ; renaming of R6RS procedure

   bits->list                 ; not R6RS
   list->bits                 ; not R6RS
   bits->vector               ; not R6RS
   vector->bits               ; not R6RS
   bits                       ; not R6RS
   bitwise-fold               ; not R6RS
   bitwise-for-each           ; not R6RS
   bitwise-unfold             ; not R6RS
   make-bitwise-generator     ; not R6RS
   )

  (import (scheme base)
          (rename (rnrs arithmetic bitwise)
                  (bitwise-arithmetic-shift  arithmetic-shift)
                  (bitwise-bit-count         bit-count)
                  (bitwise-length            integer-length)
                  (bitwise-first-bit-set     first-set-bit)
                  (bitwise-bit-field         bit-field)
                  (bitwise-reverse-bit-field bit-field-reverse))
          (only (rnrs lists) fold-left)
          (primitives larceny:errmsg))

  (include "151.body.scm"))
