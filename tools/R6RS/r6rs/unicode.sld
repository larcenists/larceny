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
; Reference implementation of (r6rs unicode)
;
; This just combines the various (local unicode*) libraries.
; One of them has to be in a separate file because its copyright
; notice is different from the others, and the others are in
; separate files to impose some modularity upon this library.

(define-library (r6rs unicode)
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
    char-title-case?

    string-upcase
    string-downcase
    string-titlecase
    string-foldcase

    string-ci=?
    string-ci<?
    string-ci>?
    string-ci<=?
    string-ci>=?

    string-normalize-nfd
    string-normalize-nfkd
    string-normalize-nfc
    string-normalize-nfkc)

  ;; Implementations that provide (scheme char) but do not downcase
  ;; Greek sigma correctly should exclude string-downcase from the
  ;; import of (scheme char) and add string-downcase to the list of
  ;; identifiers imported from (r6rs unicode-reference unicode3).

  (cond-expand

   ((and (library (scheme char))
         (library (rnrs unicode))
         (not (library (r6rs no-rnrs))))

    (import (scheme char)
            (only (rnrs unicode)
                  char-titlecase
                  char-title-case?
                  char-general-category
                  string-titlecase
                  string-normalize-nfd
                  string-normalize-nfkd
                  string-normalize-nfc
                  string-normalize-nfkc)))                  

   ((library (scheme char))

    (import (scheme char)
            (only (r6rs unicode-reference unicode1)
                  char-titlecase
                  char-title-case?
                  char-general-category)
            (only (r6rs unicode-reference unicode3)
                  string-titlecase)
            (only (r6rs unicode-reference unicode4)
                  string-normalize-nfd
                  string-normalize-nfkd
                  string-normalize-nfc
                  string-normalize-nfkc)))

   (else

    (import ;(r6rs unicode-reference unicode0)
             (r6rs unicode-reference unicode1)
            ;(r6rs unicode-reference unicode2)
             (r6rs unicode-reference unicode3)
             (r6rs unicode-reference unicode4))))

)
