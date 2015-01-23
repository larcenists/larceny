;;; Definitions for things in (scheme base) that aren't in (rnrs)
;;; or some other R6RS or SRFI library.

;;; FIXME: assoc now takes optional third argument

(define bytevector 'FIXME)
(define bytevector-append 'FIXME)

;;; FIXME: check semantics for bytevector-copy
;;; FIXME: check semantics for bytevector-copy!

;;; FIXME: the case macro is slightly different

;;; FIXME: char-ready? needs to be improved

;;; FIXME: cond-expand has to be magic

;(define cond-expand 'FIXME)

;;; FIXME: check semantics of define-syntax

;;; FIXME: check semantics of eq?, equal?, eqv?

;;; FIXME: error procedure is different

(define error-object-irritants 'FIXME)
(define error-object-message 'FIXME)
(define error-object? 'FIXME)

(define exact-integer? 'FIXME)

(define features 'FIXME)

(define file-error? 'FIXME)

(define floor-quotient 'FIXME)
(define floor-remainder 'FIXME)
(define floor/ 'FIXME)

;;; FIXME: check semantics for guard

;;; FIXME: include and include-ci have to be magic

;(define include 'FIXME)
;(define include-ci 'FIXME)

(define input-port-open? 'FIXME)

;;; FIXME: check semantics for integer?

;;; FIXME: check semantics for let*-values

;;; FIXME: check semantics for let-syntax

;;; FIXME: check semantics for let-values

;;; FIXME: check semantics for letrec-syntax

;;; FIXME: map is different

;;; FIXME: member takes optional third argument

;;; FIXME: check semantics for number->string

(define output-port-open? 'FIXME)

(define peek-u8 'FIXME)

;;; FIXME: check semantics of quasiquote

;;; FIXME: check semantics of raise

;;; FIXME: check semantics of rational?

(define read-bytevector 'FIXME)
(define read-bytevector! 'FIXME)

(define read-error? 'FIXME)

(define read-string 'FIXME)
(define read-u8 'FIXME)

;;; FIXME: check semantics of real?

(define square 'FIXME)

;;; FIXME: check semantics of string->utf8 (ought to be different)

(define string->vector 'FIXME)

;;; FIXME: check semantics of string-copy

(define string-copy! 'FIXME)
(define string-fill! 'FIXME)

;;; FIXME: check semantics of string-for-each (probably the same)

(define string-map 'FIXME)

;;; FIXME: check semantics of substring

(define syntax-error 'FIXME)

;;; FIXME: check semantics of syntax-rules

(define truncate-quotient 'FIXME)
(define truncate-remainder 'FIXME)
(define truncate/ 'FIXME)
(define u8-ready? 'FIXME)

;;; FIXME: check semantics of utf8->string

(define vector->string 'FIXME)
(define vector-append 'FIXME)
(define vector-copy 'FIXME)
(define vector-copy! 'FIXME)

;;; FIXME: check semantics of vector-fill! (probably different)

;;; FIXME: vector-for-each and vector-map are different

;;; FIXME: check semantics of with-exception-handler

(define write-bytevector 'FIXME)
(define write-u8 'FIXME)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: should check semantics of modulo, quotient, remainder


