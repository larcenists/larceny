; Copyright (C) Richard Kelsey (1999). All Rights Reserved. 
;
; $Id$
;
; This document and translations of it may be copied and furnished to
; others, and derivative works that comment on or otherwise explain it or
; assist in its implementation may be prepared, copied, published and
; distributed, in whole or in part, without restriction of any kind,
; provided that the above copyright notice and this paragraph are included
; on all such copies and derivative works. However, this document itself
; may not be modified in any way, such as by removing the copyright notice
; or references to the Scheme Request For Implementation process or
; editors, except as needed for the purpose of developing SRFIs in which
; case the procedures for copyrights defined in the SRFI process must be
; followed, or as required to translate it into languages other than
; English.

; This implementation uses the R3RS-proposed record package to do the
; dirty work.

(require 'record)

; NOTE, this can only be used predictably on the top level; if used
; where internal definitions may be used, the definitions may be
; reordered and the constructors, predicates, etc may be defined
; before the type is defined.  There is no easy fix for this (eg
; rewriting as a bunch of DEFINEs followed by a bunch of SET!s)
; because the record type definition may be followed by more internal
; definitions.

; Definition of DEFINE-RECORD-TYPE

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor . more) ...)
     (begin
       (define type
         (make-record-type (symbol->string 'type) '(field-tag ...)))
       (define constructor
         (record-constructor type '(constructor-tag ...)))
       (define predicate
         (record-predicate type))
       (define-record-field type field-tag accessor . more)
       ...))))

; An auxilliary macro for define field accessors and modifiers.
; This is needed only because modifiers are optional.

(define-syntax define-record-field
  (syntax-rules ()
    ((define-record-field type field-tag accessor)
     (define accessor (record-accessor type 'field-tag)))
    ((define-record-field type field-tag accessor modifier)
     (begin
       (define accessor (record-accessor type 'field-tag))
       (define modifier (record-updater type 'field-tag))))))

; eof
