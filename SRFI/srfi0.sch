; SRFI 0: Feature-based conditional expansion construct.
;
; $Id$
;
; See <http://srfi.schemers.org/srfi-0/srfi-0.html> for the full document.
;
; Copyright (C) Marc Feeley (1999). All Rights Reserved. 
;
; This document and translations of it may be copied and furnished to
; others, and derivative works that comment on or otherwise explain it or
; assist in its implementation may be prepared, copied, published and
; distributed, in whole or in part, without restriction of any kind,
; provided that the above copyright notice and this paragraph are included
; on all such copies and derivative works.  However, this document itself
; may not be modified in any way, such as by removing the copyright notice
; or references to the Scheme Request For Implementation process or
; editors, except as needed for the purpose of developing SRFIs in which
; case the procedures for copyrights defined in the SRFI process must be
; followed, or as required to translate it into languages other than
; English.

; Add SRFI identifiers to the keyword list as they are implemented, and 
; then add clauses near the end to expand into code.

(define-syntax cond-expand
  (syntax-rules (and or not else 
                 srfi-0                 ; Feature-based conditional expansion
                 srfi-6                 ; Basic string ports
                 srfi-7                 ; Feature-based configuration language
                 )
    ((cond-expand) 
     (error "Unfulfilled cond-expand"))              ; FIXME
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
         (cond-expand
           ((and req2 ...) body ...)
           more-clauses ...))
       more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
        (begin body ...))
       (else
        (cond-expand
           ((or req2 ...) body ...)
           more-clauses ...))))
    ((cond-expand ((not req) body ...) more-clauses ...)
     (cond-expand
       (req
         (cond-expand more-clauses ...))
       (else body ...)))
    ((cond-expand (srfi-0 body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-6 body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (srfi-7 body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (feature-id body ...) more-clauses ...)
       (cond-expand more-clauses ...))))

; eof
