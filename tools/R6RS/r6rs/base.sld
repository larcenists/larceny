;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-library (r6rs base)
  
  (export 
   
   ;; Syntax exported by (scheme base), or close enough:
   
   begin if lambda quote set! and or
   define define-syntax let-syntax letrec-syntax
   _ ...
   
   let let* letrec letrec* let-values let*-values
   case cond else =>
   quasiquote unquote unquote-splicing
   syntax-rules 
   
   ;; Syntax with no R7RS equivalent:

   assert
   identifier-syntax    ; FIXME: just a stub

   ;; R5RS procedures exported by (scheme base), or close enough:
   
   * + - / < <= = > >= abs append apply
   boolean? call-with-current-continuation 
   call-with-values car cdr caar cadr cdar cddr
   ceiling char? char->integer char=? char<? char>? char<=? char>=?
   complex? cons
   denominator dynamic-wind 
   eq? equal? eqv? even? exact? expt floor for-each
   gcd inexact? integer->char integer?
   lcm length list list->string
   list->vector list-ref list-tail list?
   make-string make-vector map max min
   negative? not null? number->string number? numerator
   odd? pair? 
   positive? procedure? rational? rationalize
   real? reverse round
   string string->list string->number string->symbol
   string-append 
   string-copy string-length string-ref string<=? string<?
   string=? string>=? string>? string? substring symbol->string symbol?
   truncate values vector vector->list
   vector-fill! vector-length vector-ref vector-set! vector? zero?

   ;; R5RS procedures exported by (scheme cxr):
   
   caaar caadr cadar caddr cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar
   cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

   ;; R5RS procedures exported by (scheme inexact):

   acos asin atan cos exp log sin sqrt tan

   ;; R5RS procedures exported by (scheme complex):

   angle imag-part magnitude make-polar make-rectangular real-part

   ;; R6RS procedures exported by (scheme base):

   boolean=?
   call/cc

   error    ; incompatible with (scheme base); see workaround below

   exact exact-integer-sqrt inexact
   symbol=?
   string-for-each vector-map vector-for-each

   ;; R6RS procedures exported by (scheme inexact):

   finite? infinite? nan?

   ;; R6RS procedures with no R7RS equivalent:
   
   real-valued? rational-valued? integer-valued?
   div mod div-and-mod div0 mod0 div0-and-mod0
   assertion-violation
   )
  
  (cond-expand
   ((and (or (library (rnrs base)) larceny) ; FIXME
         (not (library (r6rs no-rnrs))))
    (import (rnrs base)))
   (else
    (import
     (rename (scheme base)
             (error r7rs:error))
     (scheme cxr))))

  ;; Stubs and workarounds.

  (cond-expand
   ((and (or (library (rnrs base)) larceny) ; FIXME
         (not (library (r6rs no-rnrs)))))
   ((library (scheme inexact))
    (import (scheme inexact)))
   (else
    (begin (define (acos z) (unimplemented 'acos))
           (define (asin z) (unimplemented 'asin))
           (define (atan z . rest) (unimplemented 'atan))
           (define (cos z) (unimplemented 'cos))
           (define (exp z) (unimplemented 'exp))
           (define (log z . rest) (unimplemented 'log))
           (define (sin z) (unimplemented 'sin))
           (define (sqrt z) (unimplemented 'sqrt))
           (define (tan z) (unimplemented 'tan))
           (define (finite? z) #t)
           (define (infinite? z) #f)
           (define (nan? z) #f))))

  (cond-expand
   ((and (or (library (rnrs base)) larceny) ; FIXME
         (not (library (r6rs no-rnrs)))))
   ((library (scheme complex))
    (import (scheme complex)))
   (else
    (begin (define (angle z) (unimplemented 'angle))
           (define (imag-part z) 0)
           (define (magnitude z) (abs z))
           (define (make-polar x y) (unimplemented 'make-polar))
           (define (make-rectangular x y) (unimplemented 'make-rectangular))
           (define (real-part z)
             (if (real? z)
                 z
                 (unimplemented 'real-part))))))

  (cond-expand
   ((and (or (library (rnrs base)) larceny) ; FIXME
         (not (library (r6rs no-rnrs)))))
   (else
    (begin

     (define (unimplemented name)
       (error name "R6RS procedure is not implemented"))

     ;; If its arguments are not acceptable to the R6RS error procedure,
     ;; then this version of error just calls the R7RS procedure.
     ;; Otherwise it repackages its arguments in a form that should
     ;; make sense to the R7RS error procedure.

     (define (error who . rest)
       (if (null? rest)
           (r7rs:error who)
           (let ((msg (car rest))
                 (irritants (cdr rest)))
             (cond ((not (string? msg))
                    (apply r7rs:error who msg irritants))
                   ((string? who)
                    (apply r7rs:error (string-append who ": " msg) irritants))
                   ((symbol? who)
                    (apply error (symbol->string who) msg irritants))
                   (else
                    (apply r7rs:error msg irritants))))))

     (define (assertion-violation who msg . irritants)
       (apply error who msg irritants))

     ;; FIXME: just a stub

     (define-syntax identifier-syntax
       (syntax-rules ()
        ((_ . whatever)
         (syntax-error "identifier-syntax is not implemented")))))

    (begin

     (define-syntax assert
       (syntax-rules ()
        ((_ expression)
         (or expression
             (assertion-violation #f "assertion failed" 'expression))))))

    (include "base.body.scm")))
  
  ) ;; rnrs base
