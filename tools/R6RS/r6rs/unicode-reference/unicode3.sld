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
; This file contains all of the code for R6RS library section 1.2
; except for the four normalization procedures, which are in
; (proto-unicode4).  In other words, this file contains the
; case-conversion and case-sensitive comparison operations on
; strings.
;
; The tables in this file were generated from the
; Unicode Character Database, revision 7.0.0.
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.

(define-library (r6rs unicode-reference unicode3)
  (export

    string-upcase
    string-downcase
    string-titlecase
    string-foldcase

    string-ci=?
    string-ci<?
    string-ci>?
    string-ci<=?
    string-ci>=?)

  (import (scheme base)
         ;(rnrs base)
         ;(rnrs control)
         ;(rnrs bytevectors)
         ;(rnrs mutable-strings)
          (r6rs unicode-reference unicode0)
          (r6rs unicode-reference unicode1)
          (r6rs unicode-reference unicode2))

  (include "unicode3.body.scm")

  )
