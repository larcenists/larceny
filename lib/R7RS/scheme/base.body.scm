;;; Definitions for things in (scheme base) that aren't in (rnrs)
;;; or some other R6RS or SRFI library.

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


