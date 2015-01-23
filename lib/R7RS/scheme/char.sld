(define-library (scheme char)

  (export

   char-alphabetic?
   char-ci<=?
   char-ci<?
   char-ci=?
   char-ci>=?
   char-ci>?
   char-downcase
   char-foldcase
   char-lower-case?
   char-numeric?
   char-upcase
   char-upper-case?
   char-whitespace?
   digit-value
   string-ci<=?
   string-ci<?
   string-ci=?
   string-ci>=?
   string-ci>?
   string-downcase
   string-foldcase
   string-upcase)

  (import
   (rnrs base)             ; FIXME
   (primitives
    char-alphabetic?
    char-ci<=?
    char-ci<?
    char-ci=?
    char-ci>=?
    char-ci>?
    char-downcase
    char-foldcase
    char-lower-case?
    char-numeric?
    char-upcase
    char-upper-case?
    char-whitespace?
;   digit-value            ; FIXME
    string-ci<=?
    string-ci<?
    string-ci=?
    string-ci>=?
    string-ci>?
    string-downcase
    string-foldcase
    string-upcase

    assertion-violation    ; FIXME

    ))

  (include "char.body.scm"))
