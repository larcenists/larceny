; Copyright 2000 Lars T Hansen
;
; $Id$
;
; 25 September 2000
;
; This is the interface that pass1 exports to the rest of Twobit, in
; alphabetical order.  Pass1 is included in the bootstrap heap with the
; interpreter, and the standard interaction environment exports the
; pass1 interface -- the names on the RHS below.
;
; This file makes the interface available to Twobit using the names that
; Twobit expects.
;
; If Twobit is extended to use more of pass1, then the new names must
; be exported in the standard interaction environment in the bootstrap 
; by extending the toplevel (Lib/*/toplevel.sch, right now) and by
; extending the definitions in this file.
;
; The reason this file exists in the first place is two-fold:
;   (1) It documents the names that pass1 export
;   (2) It allows the pass1-exported names to be hidden in the
;       default standard environment.

(define define-syntax-scope             .pass1:define-syntax-scope)
(define identifier?                     .pass1:identifier?)
(define identifier->symbol              .pass1:identifier->symbol)
(define pass1                           .pass1:pass1)
(define pass1-block                     .pass1:pass1-block)
(define m-quit                          .pass1:m-quit)
(define macro-expand                    .pass1:macro-expand)
(define make-minimal-syntactic-environment 
  .pass1:make-minimal-syntactic-environment)
(define make-rename-procedure           .pass1:make-rename-procedure)
(define make-unspecified                .pass1:make-unspecified)
(define make-undefined                  .pass1:make-undefined)
(define syntactic-copy                  .pass1:syntactic-copy)
(define syntactic-extend                .pass1:syntactic-extend)
(define syntactic-lookup                .pass1:syntactic-lookup)
(define syntactic-environment-names     .pass1:syntactic-environment-names)
(define the-usual-syntactic-environment .pass1:the-usual-syntactic-environment)

; eof
