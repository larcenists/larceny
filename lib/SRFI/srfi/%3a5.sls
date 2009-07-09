;; SRFI-5: A compatible let form with signatures and rest arguments
;; Reference implementation
;;
;; $Id$
;;
;; Conflicts with (rnrs base): let
;;
;; Taken from http://srfi.schemers.org/srfi-5/srfi-5.html
;; on 09 November 2003.
;;
;; Copyright (C) Andy Gaynor (1999). All Rights Reserved. 
;;
;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain it
;; or assist in its implementation may be prepared, copied, published and
;; distributed, in whole or in part, without restriction of any kind,
;; provided that the above copyright notice and this paragraph are
;; included on all such copies and derivative works. However, this
;; document itself may not be modified in any way, such as by removing
;; the copyright notice or references to the Scheme Request For
;; Implementation process or editors, except as needed for the purpose of
;; developing SRFIs in which case the procedures for copyrights defined
;; in the SRFI process must be followed, or as required to translate it
;; into languages other than English.
;;
;; The limited permissions granted above are perpetual and will not be
;; revoked by the authors or their successors or assigns.
;;
;; This document and the information contained herein is provided on an
;; "AS IS" basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL
;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY
;; WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE ANY
;; RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS FOR A
;; PARTICULAR PURPOSE.

(library (srfi :5 let)

  (export let)

  (import (except (rnrs base) let))

(define-syntax srfi5:standard-let
  (syntax-rules ()
    ((_ ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

(define-syntax let

  (syntax-rules ()

    ;; No bindings: use srfi5:standard-let.
    ((let () body ...)
     (srfi5:standard-let () body ...))
    ;; Or call a lambda.
    ;; ((lambda () body ...))

    ;; All standard bindings: use srfi5:standard-let.
    ((let ((var val) ...) body ...)
     (srfi5:standard-let ((var val) ...) body ...))
    ;; Or call a lambda.
    ;; ((lambda (var ...) body ...) val ...)

    ;; One standard binding: loop.
    ;; The all-standard-bindings clause didn't match,
    ;; so there must be a rest binding.
    ((let ((var val) . bindings) body ...)
     (let-loop #f bindings (var) (val) (body ...)))

    ;; Signature-style name: loop.
    ((let (name binding ...) body ...)
     (let-loop name (binding ...) () () (body ...)))

    ;; defun-style name: loop.
    ((let name bindings body ...)
     (let-loop name bindings () () (body ...)))))

(define-syntax let-loop

  (syntax-rules ()

    ;; Standard binding: destructure and loop.
    ((let-loop name ((var0 val0) binding ...) (var ...     ) (val ...     ) 
	       body)
     (let-loop name (            binding ...) (var ... var0) (val ... val0) 
	       body))

    ;; Rest binding, no name: use srfi5:standard-let, listing the rest values.
    ;; Because of let's first clause, there is no "no bindings, no name" 
    ;; clause.

    ((let-loop #f (rest-var rest-val ...) (var ...) (val ...) body)
     (srfi5:standard-let ((var val) ... (rest-var (list rest-val ...))) 
			 . body))
    ;; Or call a lambda with a rest parameter on all values.
    ;; ((lambda (var ... . rest-var) . body) val ... rest-val ...))
    ;; Or use one of several other reasonable alternatives.

    ;; No bindings, name: call a letrec'ed lambda.
    ((let-loop name () (var ...) (val ...) body)
     ((letrec ((name (lambda (var ...) . body)))
        name)
      val ...))

    ;; Rest binding, name: call a letrec'ed lambda.
    ((let-loop name (rest-var rest-val ...) (var ...) (val ...) body)
     ((letrec ((name (lambda (var ... . rest-var) . body)))
        name)
      val ... rest-val ...))))

)

(library (srfi :5)
  (export let)
  (import (srfi :5 let)))
