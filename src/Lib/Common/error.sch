; Copyright 1998 Lars T Hansen.               -*- indent-tabs-mode: nil -*-
;
; $Id$
;
; Larceny library -- higher-level error system.

($$trace "error")

; Heuristically recognizes both R6RS-style and Larceny's old-style
; arguments.

(define (error . args)
  (if (and (pair? args) (pair? (cdr args)))
      (let ((who (car args))
            (msg (cadr args))
            (irritants (cddr args))
            (handler (error-handler)))
        (define (separated irritants)
          (if (null? irritants)
              '()
              (cons " "
                    (cons (car irritants) (separated (cdr irritants))))))
        (if (string? msg)
            (cond ((or (symbol? who) (string? who))
                   (apply handler who msg (separated irritants)))
                  ((eq? who #f)
                   (apply handler msg (separated irritants)))
                  (else
                   ; old-style, not R6RS
                   (apply handler '() args)))
            (apply handler '() args)))
      (apply (error-handler) '() args)))

(define (assertion-violation who msg . irritants)
  (apply error who msg irritants))

(define (reset)
  ((reset-handler)))

; To be replaced by exception system.
(define (call-without-errors thunk . rest)
  (let ((fail (if (null? rest) #f (car rest))))
    (call-with-current-continuation
     (lambda (k)
       (call-with-error-handler (lambda (who . args) (k fail)) thunk)))))

; Old code: clients should use PARAMETERIZE instead.
(define (call-with-error-handler handler thunk)
  (let ((old-handler (error-handler)))
    (dynamic-wind 
     (lambda () (error-handler handler))
     thunk
     (lambda () (error-handler old-handler)))))

; Old code: clients should use PARAMETERIZE instead.
(define (call-with-reset-handler handler thunk)
  (let ((old-handler (reset-handler)))
    (dynamic-wind 
     (lambda () (reset-handler handler))
     thunk
     (lambda () (reset-handler old-handler)))))

; DECODE-ERROR takes an error and optionally a port to print on (defaults
; to the current output port) and prints a human-readable error message 
; to the port based on the information in the error.
;
; The error is a list.  The first element is a key, the rest depend on the
; key.  There are three cases, depending on the key:
;  - a number:  The error is a primitive error.  There will be three
;               additional values, the contents of RESULT, SECOND, and
;               THIRD.
;  - null:      The key is to be ignored, and the following arguments are
;               to be interpreted as a user-level error: objects to be
;               printed.
;  - otherwise: The arguments are to be interpreted as a user-level error:
;               objects to be printed.

(define (decode-error the-error . rest)
  (let ((who (car the-error))
        (port (if (null? rest) (current-output-port) (car rest))))
    (if (number? who)
        (decode-system-error who 
                             (cadr the-error) 
                             (caddr the-error)
                             (cadddr the-error)
                             port)
        (begin
          (newline port)
          (display "Error: " port)
          (if (not (null? who))
              (begin (display who port)
                     (display ": " port)))
          (for-each (lambda (x) (display x port)) (cdr the-error))
          (newline port)
          (flush-output-port port)))))

; eof
