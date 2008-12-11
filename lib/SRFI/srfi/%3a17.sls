; SRFI 17: Generalized SET
;
; $Id$
;
; Conflicts with (rnrs base): set!
;
; See <http://srfi.schemers.org/srfi-17/srfi-17.html> for the full document.

; SRFI 17 reference implementation for Twobit.
;
; Use the LET* syntax scope extension in Twobit to let this SET! macro
; reference the old definition of SET! in the second clause.

(library (srfi :17 generalized-set!)

  (export set! setter getter-with-setter)

  (import (rename (rnrs base) (set! r6rs:set!))
          (rnrs lists)
          (rnrs mutable-pairs)
          (rnrs mutable-strings))

(define-syntax set!
  (syntax-rules ()
    ((set! (?e0 ?e1 ...) ?v)
     ((setter ?e0) ?e1 ... ?v))
    ((set! ?i ?v)
     (r6rs:set! ?i ?v))))

(define setter 
  (let ((setters (list (cons car  set-car!)
                       (cons cdr  set-cdr!)
                       (cons caar (lambda (p v) (set-car! (car p) v)))
                       (cons cadr (lambda (p v) (set-car! (cdr p) v)))
                       (cons cdar (lambda (p v) (set-cdr! (car p) v)))
                       (cons cddr (lambda (p v) (set-cdr! (cdr p) v)))
                       (cons caaar (lambda (p v) (set-car! (caar p) v)))
                       (cons caadr (lambda (p v) (set-car! (cadr p) v)))
                       (cons cadar (lambda (p v) (set-car! (cdar p) v)))
                       (cons caddr (lambda (p v) (set-car! (cddr p) v)))
                       (cons cdaar (lambda (p v) (set-cdr! (caar p) v)))
                       (cons cdadr (lambda (p v) (set-cdr! (cadr p) v)))
                       (cons cddar (lambda (p v) (set-cdr! (cdar p) v)))
                       (cons cdddr (lambda (p v) (set-cdr! (cddr p) v)))
                       (cons caaaar (lambda (p v) (set-car! (caaar p) v)))
                       (cons caaadr (lambda (p v) (set-car! (caadr p) v)))
                       (cons caadar (lambda (p v) (set-car! (cadar p) v)))
                       (cons caaddr (lambda (p v) (set-car! (caddr p) v)))
                       (cons cadaar (lambda (p v) (set-car! (cdaar p) v)))
                       (cons cadadr (lambda (p v) (set-car! (cdadr p) v)))
                       (cons caddar (lambda (p v) (set-car! (cddar p) v)))
                       (cons cadddr (lambda (p v) (set-car! (cdddr p) v)))
                       (cons cdaaar (lambda (p v) (set-cdr! (caaar p) v)))
                       (cons cdaadr (lambda (p v) (set-cdr! (caadr p) v)))
                       (cons cdadar (lambda (p v) (set-cdr! (cadar p) v)))
                       (cons cdaddr (lambda (p v) (set-cdr! (caddr p) v)))
                       (cons cddaar (lambda (p v) (set-cdr! (cdaar p) v)))
                       (cons cddadr (lambda (p v) (set-cdr! (cdadr p) v)))
                       (cons cdddar (lambda (p v) (set-cdr! (cddar p) v)))
                       (cons cddddr (lambda (p v) (set-cdr! (cdddr p) v)))
                       (cons vector-ref vector-set!)
                       (cons string-ref string-set!))))
    (letrec ((setter
              (lambda (proc)
                (let ((probe (assv proc setters)))
                  (if probe
                      (cdr probe)
                      (error "No setter for " proc)))))
             (set-setter!
              (lambda (proc setter)
                (set! setters (cons (cons proc setter) setters)))))
      (set-setter! setter set-setter!)
      setter)))

; Contributed by Per Bothner, from the SRFI document.

(define (getter-with-setter get set)
  (let ((proc (lambda args (apply get args))))
    (set! (setter proc) set)
    proc))

)

(library (srfi :17)
  (export set! setter getter-with-setter)
  (import (srfi :17 generalized-set!)))  

; eof
