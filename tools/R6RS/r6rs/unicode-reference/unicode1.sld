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
; This file contains all of the code for R6RS library section 1.1.
; In other words, this file contains the operations on characters,
; but not the operations on strings.
;
; The tables in this file were generated from the
; Unicode Character Database, revision 7.0.0.
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.

(define-library (r6rs unicode-reference unicode1)
  (export

    char-upcase
    char-downcase
    char-titlecase
    char-foldcase

    char-ci=?
    char-ci<?
    char-ci>?
    char-ci<=?
    char-ci>=?

    char-general-category
    char-alphabetic?
    char-numeric?
    char-whitespace?
    char-upper-case?
    char-lower-case?
    char-title-case?)

  (import (scheme base)
         ;(rnrs base)
         ;(rnrs control)
         ;(rnrs bytevectors)
         ;(rnrs lists)
          (r6rs unicode-reference unicode0))

  (include "unicode1.body.scm")

  )
