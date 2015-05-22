; Copyright 2006 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright and permission notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This file contains a few auxiliary procedures that are used
; by the reference implementation of the (rnrs unicode) library.
; None of the definitions in this file are exported as part of
; (rnrs unicode).
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.

(define-library (r6rs unicode-reference unicode0)
  (export

    div                 ; FIXME
    mod                 ; FIXME
    u8-list->bytevector ; FIXME

    binary-search-of-vector
    binary-search
    binary-search-16bit

    make-comparison-predicate)

  (import (scheme base))

  (begin

   ;; FIXME: This should go away once (rnrs base) is implemented.

   (define (div x y)
     (quotient x y))

   (define (mod x y)
     (remainder x y))

   ;; FIXME: This should go away once (rnrs bytevectors) is implemented.

   (define (u8-list->bytevector bytes)
     (apply bytevector bytes))

   )

  (include "unicode0.body.scm")

  )
