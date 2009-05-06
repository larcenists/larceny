; SRFI 61: a more general cond clause
;
; $Id$
;
; Conflicts with (rnrs base): cond
;
; The following reference implementation, written by Taylor Campbell,
; is in the public domain.

; The entirety of a syntax transformer for the new cond syntax is
; given here.  It uses an auxiliary macro, cond/maybe-more, to
; simplify the construction of if expressions with or without more
; cond clauses.  The code is in the public domain.

(library (srfi :61 cond)

  (export cond)

  (import (except (rnrs base) cond))          

(define-syntax cond
  (syntax-rules (=> else)

    ((cond (else else1 else2 ...))

     ;; The (IF #T (BEGIN ...)) wrapper ensures that there may be no
     ;; internal definitions in the body of the clause.  R5RS mandates
     ;; this in text (by referring to each subform of the clauses as
     ;; <expression>) but not in its reference implementation of COND,
     ;; which just expands to (BEGIN ...) with no (IF #T ...) wrapper.

     (if #t (begin else1 else2 ...)))

    ((cond (test => receiver) more-clause ...)
     (let ((t test))
       (cond/maybe-more t
                        (receiver t)
                        more-clause ...)))

    ((cond (generator guard => receiver) more-clause ...)
     (call-with-values (lambda () generator)
       (lambda t
         (cond/maybe-more (apply guard    t)
                          (apply receiver t)
                          more-clause ...))))

    ((cond (test) more-clause ...)
     (let ((t test))
       (cond/maybe-more t t more-clause ...)))

    ((cond (test body1 body2 ...) more-clause ...)
     (cond/maybe-more test
                      (begin body1 body2 ...)
                      more-clause ...))))

(define-syntax cond/maybe-more
  (syntax-rules ()
    ((cond/maybe-more test consequent)
     (if test
         consequent))
    ((cond/maybe-more test consequent clause ...)
     (if test
         consequent
         (cond clause ...)))))

)

(library (srfi :61)

  (export cond)

  (import (srfi :61 cond)))

; eof
