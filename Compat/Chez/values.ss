; Copyright 1998 Lars T Hansen
;
; $Id$
;
; Multiple values for Chez Scheme v4.x.

(define (values . x) x)

(define (call-with-values proc receiver)
  (apply receiver (proc)))

