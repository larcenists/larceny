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
; This file contains an implementation of the official Unicode
; word-breaking algorithm, as best I could figure it out from
; Unicode Standard Annex #29, version 7.0.0.
;
; The tables in this file were generated from the
; Unicode Character Database, revision 7.0.0.
; The same goes for the hard-coded constants.
;
; This file does not rely on any lexical syntax for
; non-Ascii characters and strings.

(define-library (r6rs unicode-reference unicode2)
  (export

    string-next-word-break
    string-previous-word-break)

  (import (scheme base)
         ;(rnrs bytevectors)
         ;(rnrs lists)
          (r6rs unicode-reference unicode0)
          (r6rs unicode-reference unicode1))

  (begin
   (define r7rs-linefeed-character #\newline))

  (include "unicode2.body.scm")

  )
