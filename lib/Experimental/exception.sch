; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Simple exception system -- prototype.
;
; There are three kinds of exceptions:
;  - System exceptions are signalled by system primitives and are
;    noncontinuable.
;  - Errors are signalled by the ERROR procedure and are noncontinuable.
;  - User exceptions are created with the MAKE-EXCEPTION procedure and
;    signalled by the SIGNAL-EXCEPTION procedure.  They are continuable
;    or not.
;
; (make-exception type values context continuable?) => exn
;   TYPE is a symbol: "system", "error", or "user".  If "user", then
;   values can be any list of values, and context should for the time
;   being be #f.
;
; (exception? object) => boolean
;   Tests whether the object is an exception object.
;
; (exception-type exn) => symbol
;   Returns the type of the exception object EXN.
;
; (exception-values exn) => list
;   Returns the list of exception value in the exception object EXN.
;
; (exception-context exn) => object
;   Returns the context object of the exception object EXN.
;
; (exception-continuable? exn) => boolean
;   EXN must be an exception object.  Returns #t if one can return from the
;   exception handler and expect something reasonable to happen.
;
; (exception-message exn) => string
;   EXN must be an exception object.  A human-readable string explaining the
;   exception is returned.
;
; (call-with-handler handler thunk) => values
;   Calls THUNK with no arguments.  If THUNK signals an exception exn
;   then HANDLER is invoked on exn in the dynamic context of the exception.
;
; (call-with-exception-handler handler thunk) => values
;   Calls THUNK with no arguments.  If THUNK signals an exception exn
;   then HANDLER is invoked on exn in the dynamic context of the caller
;   of CALL-WITH-EXCEPTION-HANDLER.  (Note: a poorly chosen name, not
;   yet corrected because Doug uses this package.)
;
; (error object ...)
;   Signal a noncontinuable error exception where "object ..." are the
;   exception values.
;
; (signal-exception exn) => values
;   EXN must be an exception object.  The exception is signalled, and
;   the closest handler in the dynamic scope is invoked.  If the exception
;   is continuable, and the handler returns, then the values returned by
;   the handler are those returned by signal-exception.  If the exception
;   is noncontinuable, then signal-exception does not return.
;
;
; For example, here's a nested handler and re-raise:
;
;  (call-with-exception-handler
;    (lambda (exn)
;      (format #t "Error: ~a~%" (exception-message exn))
;      (reset))
;    (lambda ()
;      (call-with-exception-handler
;        (lambda (exn)
;  	   (signal-exception exn))
;        (lambda () 
;          (error "This is a test.")))))

'(require 'macros)                      ; PARAMETERIZE and other syntax
'(require 'define-record)               ; DEFINE-RECORD syntax

(define exception/token (vector 'exception))

(define-record exception (type values context continuable?))

(define (exception-message exn)
  (case (exception-type exn)
    ((system error)
     (let ((s (open-output-string)))
       (decode-error (exception-values exn) s)
       (get-output-string s)))
    ((user)
     (let ((s (open-output-string)))
       (decode-error (cons "User exception: " (exception-values exn)) s)
       (get-output-string s)))
    (else ???)))

(define (call-with-handler handler thunk)
  (parameterize 
      ((error-handler
        (lambda (who . args)
          (call-with-current-continuation
           (lambda (context)
             (let ((exn
                    (cond ((eq? who exception/token)
                           (car args))
                          ((number? who)
                           (make-exception 'system (cons who args) context #f))
                          ((null? who)
                           (make-exception 'error args context #f))
                          (else
                           (make-exception 'error (cons who (cons ": " args)) 
                                           context
                                           #f)))))
               (if (not (exception-continuable? exn))
                   (begin
                     (handler exn)
                     (error "Handler for non-continuable exception returned."))
                   (handler exn))))))))
    (thunk)))

(define (call-with-exception-handler handler thunk)
  (let* ((exn #f)
         (r (call-with-current-continuation
             (lambda (escape)
               (call-with-handler
                (lambda (e)
                  (set! exn e)
                  (escape #f))
                thunk)))))
    (if exn
        (handler exn)
        r)))

(define (signal-exception exn)
  ((error-handler) exception/token exn))

; eof
