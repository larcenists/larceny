; SRFI 12: Exception handling
;
; $Id$
;
; See <http://srfi.schemers.org/srfi-12/srfi-12.html> for the full document.
;
; Copyright (C) William Clinger, R. Kent Dybvig, Matthew Flatt, and 
; Marc Feeley (1999).  All rights reserved.
;
; This document and translations of it may be copied and furnished to
; others, and derivative works that comment on or otherwise explain it or
; assist in its implementation may be prepared, copied, published and
; distributed, in whole or in part, without restriction of any kind,
; provided that the above copyright notice and this paragraph are included
; on all such copies and derivative works. However, this document itself
; may not be modified in any way, such as by removing the copyright notice
; or references to the Scheme Request For Implementation process or
; editors, except as needed for the purpose of developing SRFIs in which
; case the procedures for copyrights defined in the SRFI process must be
; followed, or as required to translate it into languages other than
; English.

; CURRENT-EXCEPTION-HANDLER is read/write rather than read-only.

(define current-exception-handler
  (let ((handler (lambda (obj) 
                   (display "No exception handler has been installed.")
                   (newline)
                   (exit))))
    (lambda args
      (cond ((null? args) handler)
            ((null? (cdr args))
             (set! handler (car args))
             handler)
            (else
             (error "Wrong number of arguments to current-exception-handler."))))))

(define (with-exception-handler handler thunk)
  (parameterize ((current-exception-handler handler))
    (thunk)))

(define (abort obj)
  ((current-exception-handler) obj)
  (error "Exception handler returned."))

(define (signal obj)
  ((current-exception-handler) obj))

(define-syntax handle-exceptions
  (syntax-rules ()
    ((handle-exceptions ?var ?handle-expr ?expr1 ?expr2 ...)
     ((call-with-current-continuation
       (lambda (k)
         (with-exception-handler
          (lambda (?var)
            (k (lambda () ?handle-expr)))
          (lambda ()
            (call-with-values
             (lambda () ?expr1 ?expr2 ...)
             (lambda args (k (lambda () (apply values args)))))))))))))

(define condition?)
(define make-property-condition)
(define make-composite-condition)
(define condition-predicate)
(define condition-property-accessor)

(let ((condition-type 
       (make-record-type "condition" '(attributes))))

  ; "Attributes" is an assoc list of (kind-key . prop-list)
  ; where prop-list is a list of alternating prop-keys and
  ; values.

  (define cond? (record-predicate condition-type))
  (define constr (record-constructor condition-type))
  (define attr (record-accessor condition-type 'attributes))

  (set! condition? cond?)

  (set! make-property-condition
        (lambda (kind-key . prop-vals)
          (constr (list (cons kind-key prop-vals)))))

  (set! make-composite-condition
        (lambda conditions
          (constr
           (apply append (map attr conditions)))))

  (set! condition-predicate
        (lambda (kind-key)
          (lambda (exn)
            (if (and (cond? exn)
                     (assv kind-key (attr exn)))
                #t
                #f))))

  (set! condition-property-accessor 
        (lambda (kind-key prop-key)
          (lambda (exn)
            (let ((probe (assv kind-key (attr exn))))
              (if (not probe)
                  (error "No kind-key " kind-key " in " exn)
                  (let ((probe (memv prop-key probe)))
                    (if (not probe)
                        (error "No prop-key " prop-key 
                               " for kind-key " kind-key 
                               " in " exn)
                        (cadr probe))))))))
  #t)

; Error handler.  Don't mess with this.
;
; The error handler currently signals a noncontinuable exception but it
; can clearly be more sophisticated.  I need to decode the exception
; and construct an appropriate exception object.

(error-handler
 (lambda error
   (abort
    (make-property-condition 'exn
                             'message
                             (let ((s (open-output-string)))
                               (parameterize ((print-level 10)
                                              (print-length 10))
                                 (decode-error error s)
                                 (get-output-string s)))))))

; Default exception handler.
;
; In some sense the standard condition is only a standard condition
; if it isn't composite, but I'm ignoring that here.

(current-exception-handler
 (let ((std-condition? (condition-predicate 'exn))
       (exn-message (condition-property-accessor 'exn 'message)))
   (lambda (exn)
     (cond ((std-condition? exn)
            (display (exn-message exn)))
           ((condition? exn)
            (display "Exception: ")
            (newline)
            (pretty-print ((record-accessor (record-type-descriptor exn)
                                            'attributes)
                           exn)))
           (else
            (display "Exception: ")
            (newline)
            (pretty-print exn)))
     (reset))))

; eof
