;;; SRFI 143 (fixnums)
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


(define-library (srfi 143)

  (export

   fx-width            ; NOTE: not R6RS
   fx-greatest         ; NOTE: not R6RS
   fx-least            ; NOTE: not R6RS
   fixnum?
   fx=?                ; FIXME: SRFI 143 allows 0 or more arguments
   fx<?                ; for these comparisons, but does not explain
   fx>?                ; their semantics for the 0- or 1-argument cases.
   fx<=?               ; For the time being, we'll assume SRFI 143's
   fx>=?               ; generalization was just a mistake.
   fxzero?
   fxpositive?
   fxnegative?
   fxodd?
   fxeven?
   fxmax
   fxmin
   fx+
   fx-                 ; FIXME: not restricted to exactly 2 arguments
   fxneg               ; NOTE: not R6RS
   fx*
   fxquotient          ; NOTE: not R6RS
   fxremainder         ; NOTE: not R6RS
   fxabs               ; NOTE: not R6RS
   fxsquare            ; NOTE: not R6RS
   fxsqrt              ; NOTE: not R6RS
   fx+/carry
   fx-/carry
   fx*/carry
   fxnot
   fxand
   fxior
   fxxor
   fxarithmetic-shift
   fxarithmetic-shift-left
   fxarithmetic-shift-right
   fxbit-count
   fxlength
   fxif
   fxbit-set?          ; NOTE: incompatible with R6RS fxbit-set?
   fxcopy-bit          ; NOTE: incompatible with R6RS fxcopy-bit
   fxfirst-set-bit     ; NOTE: not R6RS
   fxbit-field
   fxbit-field-rotate  ; NOTE: not R6RS
   fxbit-field-reverse ; NOTE: not R6RS
   )

  (import (scheme base)
          (rename (rnrs arithmetic fixnums)
                  (fxbit-set? r6rs:bit-set?)
                  (fxcopy-bit r6rs:fxcopy-bit))
          (primitives
           fxneg fxquotient fxremainder fxabs fxsquare fxsqrt
           fxfirst-set-bit fxbit-field-rotate fxbit-field-reverse))

  (begin

   (define fx-width (fixnum-width))

   (define fx-greatest (greatest-fixnum))

   (define fx-least (least-fixnum))

   (define (fxbit-set? index i)
     (r6rs:bit-set? i index))

   (define (fxcopy-bit index i b)
     (r6rs:fxcopy-bit i index (if b 1 0)))

   ))
