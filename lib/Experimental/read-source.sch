; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Read source code and return both datum and source locations

; Returns two values: the datum read and an association list that maps the
; first pair of every list structure to a source position.  That is, every
; time the reader sees an open paren, the location of that open paren is
; recorded in the assoc list.

(define (read-source-code port)
  (let* ((open-paren-reader (readtable-ref #\())
         (class (car open-paren-reader))
         (dispatch (cadr open-paren-reader))
         (dispatch-list (caddr open-paren-reader))
         (l '()))
    (readtable-set! #\(
                    (list class 
                          (lambda (c p)
                            (let ((pos (port-position p)))
                              (let ((datum (dispatch c p)))
                                (set! l (cons (cons datum pos) l))
                                datum)))
                          dispatch-list))
    (let ((datum (read port)))
      (readtable-set! #\( open-paren-reader)
      (values datum (reverse l)))))

; eof


