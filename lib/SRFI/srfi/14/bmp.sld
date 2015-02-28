;;; SRFI-14 character-sets library                            -*- Scheme -*-
;;;
;;; $Id$
;;;
;;; This library supports the BMP subset of Unicode.
;;; See also (srfi 14 unicode) and (srfi 14 latin-1).
;;;

(define-library (srfi 14 bmp)

  (export

   char-set? char-set= char-set<=
   char-set-hash 
   char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
   char-set-fold char-set-unfold char-set-unfold!
   char-set-for-each char-set-map
   char-set-copy char-set

   list->char-set  string->char-set 
   list->char-set! string->char-set! 

   char-set-filter ucs-range->char-set  ->char-set
   char-set-filter! ucs-range->char-set!

   char-set->list char-set->string

   char-set-size char-set-count char-set-contains?
   char-set-every char-set-any

   char-set-adjoin  char-set-delete 
   char-set-adjoin! char-set-delete!

   char-set-complement  char-set-union  char-set-intersection  
   char-set-complement! char-set-union! char-set-intersection! 

   char-set-difference  char-set-xor  char-set-diff+intersection
   char-set-difference! char-set-xor! char-set-diff+intersection!

   char-set:lower-case          char-set:upper-case     char-set:title-case
   char-set:letter              char-set:digit          char-set:letter+digit
   char-set:graphic             char-set:printing       char-set:whitespace
   char-set:iso-control         char-set:punctuation    char-set:symbol
   char-set:hex-digit           char-set:blank          char-set:ascii
   char-set:empty               char-set:full)

  (import (except (rnrs base) error)
          (except (scheme base) error)
          (rnrs unicode)
          (rnrs lists)
          (rnrs control)
          (rnrs arithmetic fixnums)
          (only (rnrs arithmetic bitwise) bitwise-and)
          (rnrs sorting)
          (larceny deprecated)
          (primitives bytevector-ref bytevector-set!))

  (begin

   (define %excluded:min #xd800)    ; The range from #xd800 through #xdfff
   (define %excluded:max #xdfff)    ; (inclusive) is reserved for surrogates
   (define %unicode:limit #x110000) ; all Unicode characters are less than this

   (define %excluded:min/8 (div %excluded:min 8))
   (define %excluded:max/8 (div %excluded:max 8))

   (define (%char-set:minsize) 128)
   (define (%char-set:minsize-in-bytes) (/ (%char-set:minsize) 8))

   (define (%char-set:maxsize) #x10000)
   (define (%char-set:maxsize-in-bytes) (/ (%char-set:maxsize) 8))

   (include "char-sets.body.scm")))
