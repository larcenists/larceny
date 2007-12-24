; Copyright 2007 William D Clinger
;
; $Id$
;
; Interlibrary optimization.
;
; This pass converts the output of Andre van Tonder's macro expander
; into a form that Twobit will optimize more effectively.
;
; FIXME:  This isn't implemented yet, except for a trivial
; optimization of top-level programs that doesn't actually
; involve any interlibrary optimization.

(define (pass0 exp)
  (if (and (integrate-procedures)
           (global-optimization))
      (pass0-optimize exp)
      exp))

; When van Tonder's expander is run on a top-level program,
; its output currently looks something like
;
; (begin (ex:import-libraries-for-run
;          (quote (((rnrs io simple) 0) ((rnrs base) 0)))
;          (quote (\x0;build~1194538684~2675
;                   \x0;build~1194538684~1399))
;          0)
;        <def-or-exp>
;        ...)
;
; where each <def-or-exp> is an R5RS top-level definition
; or expression that has been macro-expanded into core forms.

(define (pass0-optimize exp)
  (cond ((and (pair? exp)
              (eq? (car exp) 'begin)
              (pair? (cdr exp))
              (let ((form1 (cadr exp)))
                (and (pair? form1)
                     (eq? (car form1)
                          'ex:import-libraries-for-run))))
         (pass0-optimize-toplevel exp))
        (else
         exp)))

; Wrapping those definitions and expressions inside a let
; may allow Twobit to generate better code.
;
; FIXME: (gensym) should return an unforgeable symbol,
; but there is no such thing anymore.

(define (pass0-optimize-toplevel exp)
  (define (gensym)
    (set! counter (+ counter 16))
    (string->symbol
     (string-append "ignored0"
                    (number->string counter 16))))
  (define counter #xf0000)
  (let* ((form1 (cadr exp))
         (forms (cddr exp))
         (forms (map (lambda (form)
                       (if (and (pair? form)
                                (eq? (car form) 'define))
                           form
                           (list 'define (gensym) form)))
                     forms)))
    `(begin ,form1
            ((lambda ()
               ,@forms
               0)))))

