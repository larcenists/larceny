; Copyright (C) Richard Kelsey, Michael Sperber (2002). All Rights Reserved.
;
; Permission is hereby granted, free of charge, to any
; person obtaining a copy of this software and associated
; documentation files (the "Software"), to deal in the
; Software without restriction, including without
; limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of
; the Software, and to permit persons to whom the Software
; is furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice
; shall be included in all copies or substantial portions
; of the Software. 
;
; $Id: raise.sch 5067 2007-11-04 01:45:06Z will $
;
; This is the reference implementation for SRFI 34,
; modified for Larceny.  See
; http://srfi.schemers.org/srfi-34/srfi-34.html

($$trace "raise")

(define *basic-exception-handlers*
  (list unhandled-exception-error))

(define *current-exception-handlers* *basic-exception-handlers*)

(define (custom-exception-handlers?)
  (not (eq? *current-exception-handlers* *basic-exception-handlers*)))

(define (with-exception-handler handler thunk)
  (with-exception-handlers (cons handler *current-exception-handlers*)
                           thunk))

(define (with-exception-handlers new-handlers thunk)
  (let ((previous-handlers *current-exception-handlers*)
        (new-handlers (if (null? new-handlers)
                          *basic-exception-handlers*
                          new-handlers)))
    (dynamic-wind
      (lambda ()
        (set! *current-exception-handlers* new-handlers))
      thunk
      (lambda ()
        (set! *current-exception-handlers* previous-handlers)))))

(define (raise obj)
  (let ((handlers *current-exception-handlers*))
    (with-exception-handlers (cdr handlers)
      (lambda ()
        ((car handlers) obj)
        (error 'raise
               "handler returned"
               (car handlers)
               obj)))))

(define (raise-continuable obj)
  (let ((handlers *current-exception-handlers*))
    (with-exception-handlers (cdr handlers)
      (lambda () ((car handlers) obj)))))

