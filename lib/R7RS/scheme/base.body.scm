;;; Definitions for things in (scheme base) that aren't in (rnrs)
;;; or some other R6RS or SRFI library.

;;; NOTE that Twobit may open-code calls to MAP and FOR-EACH and thus
;;; ignore the definitions in this file.  The versions of MAP and
;;; FOR-EACH specified by the R6RS are not compliant with SRFI-1 and
;;; the R7RS: the R6RS requires map and for-each to raise exceptions
;;; on lists of unequal length.
;;;
;;; As of Larceny v0.99, this incompatibility is resolved as follows:
;;;
;;;     open-coded calls to map and for-each perform a closed call
;;;         if their list arguments have different lengths
;;;
;;;     in Larceny's r5rs and r7rs execution modes, map and for-each
;;;         use SRFI 1 and R7RS semantics
;;;
;;;     in Larceny's r6rs execution mode, map and for-each enforce
;;;         the R6RS requirement unless the program has imported
;;;         the (srfi 1) or (scheme base) library, which imply
;;;         SRFI 1 and R7RS semantics
;;;
;;;     the (srfi 1) and (scheme base) libraries announce their
;;;         use by calling a larceny:use-r7rs-semantics! procedure

(larceny:use-r7rs-semantics!)

;;; FIXME: assoc now takes optional third argument

;;; FIXME: check semantics for bytevector-copy
;;; FIXME: check semantics for bytevector-copy!

(define bytevector-copy! r7rs:bytevector-copy!)

;;; FIXME: the case macro is slightly different

;;; FIXME: char-ready? needs to be improved

;;; FIXME: cond-expand has to be magic

;(define cond-expand 'FIXME)

;;; FIXME: check semantics of define-syntax

;;; FIXME: check semantics of eq?, equal?, eqv?

;;; FIXME: error procedure is different

(define features larceny:features)

;;; FIXME: check semantics for guard

;;; FIXME: include and include-ci have to be magic

;(define include 'FIXME)
;(define include-ci 'FIXME)

;;; FIXME: check semantics for integer?

;;; FIXME: check semantics for let*-values

;;; FIXME: check semantics for let-syntax

;;; FIXME: check semantics for let-values

;;; FIXME: check semantics for letrec-syntax

;;; FIXME: map is different

;;; FIXME: member takes optional third argument

;;; FIXME: check semantics for number->string

;;; FIXME: check semantics of quasiquote

;;; FIXME: check semantics of raise

;;; FIXME: check semantics of rational?

;;; FIXME: check semantics of real?

;;; FIXME: check semantics of string->utf8 (ought to be different)

;;; FIXME: check semantics of string-copy

;;; FIXME: check semantics of string-for-each (probably the same)

;;; FIXME: check semantics of substring

;;; FIXME: check semantics of syntax-rules

;;; FIXME: check semantics of utf8->string

;;; FIXME: check semantics of vector-fill! (probably different)

;;; FIXME: vector-for-each and vector-map are different

;;; FIXME: check semantics of with-exception-handler

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: should check semantics of modulo, quotient, remainder


