;; $Id$

;; raise : value -> *escapes*

;; current-exception-handler : parameter
;; error-display-handler : parameter
;; error-escape-handler : parameter

;; with-handlers
;; with-handlers*

;; Exception Structures

(define-struct exn (message continuation-marks))
  (define-struct (exn:fail exn) ())
    (define-struct (exn:fail:contract exn:fail) ())
      (define-struct (exn:fail:contract:arity exn:fail:contract) ())
      (define-struct (exn:fail:contract:divide-by-zero exn:fail:contract) ())
      (define-struct (exn:fail:contract:continuation exn:fail:contract) ())
      (define-struct (exn:fail:contract:variable exn:fail:contract) (id))
    (define-struct (exn:fail:syntax exn:fail) (exprs))
    (define-struct (exn:fail:read exn:fail) (srclocs))
      (define-struct (exn:fail:read:eof exn:fail:read) ())
      (define-struct (exn:fail:read:non-char exn:fail:read) ())
    (define-struct (exn:fail:filesystem exn:fail) ())
      (define-struct (exn:fail:filesystem:exists exn:fail:filesystem) ())
      (define-struct (exn:fail:filesystem:version exn:fail:filesystem) ())
    (define-struct (exn:fail:network exn:fail) ())
      (define-struct (exn:fail:out-of-memory exn:fail) ())
      (define-struct (exn:fail:unsupported exn:fail) ())
  (define-struct (exn:break exn) (continuation))

(define-struct-property prop:exn:srclocsprop:exn:srclocs)

;; exn:srclocs? : value -> boolean

;; error : symbol -> *escapes*
;; error : format-string value ... -> *escapes*
;; error : symbol format-string value ... -> *escapes*

;; raise-type-error : symbol string value -> *escapes*
;; raise-type-error : symbol string integer (listof value) -> *escapes*

;; raise-mismatch-error : symbol string value -> *escapes*
;; raise-syntax-error : symbol string [syntax syntax] -> *escapes*

;; object-name : value -> symbol|string|#f

;; -------------------------------------

;; default-error-escape-handler
