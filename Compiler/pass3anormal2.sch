; Copyright 1999 William D Clinger.
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful noncommercial purpose, and to redistribute
; this software is granted subject to the restriction that all copies
; made of this software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 13 April 1999.
;
; Post-simplification of A-normal form.
;
; Twobit's register allocator has a small problem with A-normal forms:
; It is likely to allocate a register for temporaries that are used
; only once, immediately after they are bound.  With the MacScheme
; machine architecture, these temporaries should be evaluated into the
; result register, and no general register should be allocated for them.
;
; The referencing information isn't available to the code generator, so
; it isn't easy for the code generator to detect that a temporary is
; used only once.  It could figure this out by studying the body of
; the lambda expression that binds the temporary, but that is awkward
; and would make code generation run slower.
;
; As implemented here, the solution is for pass 3 to back out of
; A-normal form just a little bit, by implementing the following source
; transformations.
;
; If T1 is used only once, then:
;
;     original code                       transformed code
; 
;     (let  ((T1 E1))                 ->  (if E1 ... ...)
;       (if T1 ... ...))
; 
;     (let  ((T1 E1))                 ->  (<primop> E1 ...)
;       (<primop> T1 ...))
; 
;     (let  ((T1 E1))                 ->  (E1 ...)
;       (T1 ...))
; 
;     (let  ((T1 E1))                 ->  (set! ... E1)
;       (set! ... T1))
; 
;     (let* ((T1 E1)                  ->  (let ((T2 (if E1 ... ...)))
;            (T2 (if T1 ... ...)))          E3)
;       E3)
; 
;     (let* ((T1 E1)                  ->  (let ((T2 (<primop> E1 ...)))
;            (T2 (<primop> T1 ...)))        E3)
;       E3)
; 
;     (let* ((T1 E1)                  ->  (let ((T2 (E1 ...)))
;            (T2 (T1 ...)))                 E3)
;       E3)
; 
;     (let* ((T1 E1)                  ->  (let ((T2 (set! ... E1)))
;            (T2 (set! ... T1)))            E3)
;       E3)

; Given a lambda expression L0 of
; one alpha-converted argument T1 that is used only once,
; an expression E0 that is either the body of L0 (if L2 is #f)
; or such that the body of L0 is (L2 E0), and
; given an expression E1, the variables that occur free in (L0 E1),
; the register bindings that need to be wrapped around (L0 E1), and
; given L2, which is #f if the body of L0 is E0, and is otherwise a
; lambda expression such that the body of L0 is (L2 E0),
; returns three values:
;     an expression equivalent to (L0 E1)
;     the free variables of the expression
;     the register bindings that need to be wrapped around the expression
;
; If L2 is #f, then the expression that is returned is equivalent to
; (let ((T1 E1)) E0).
;
; If L2 is a lambda expression, then the expression that is returned is
; equivalent to
; (let ((T1 E1)) (L2 E0)), which is itself equivalent to
; (L2 (let ((T1 E1)) E0)).

(define (post-simplify-anf L0 T1 E0 E1 free regbindings L2)
  
  (define (return-normally)
    (values (make-call L0 (list E1))
            free
            regbindings))
  
  (define (return-simplified exp)
    (values (wrap L2 exp)
            free
            regbindings))
  
  (define (wrap L exp)
    (if L
        (make-call L (list exp))
        exp))
  
  (cond ((conditional? E0)
         (let ((exp0 (if.test E0)))
           (if (and (variable? exp0)
                    (eq? T1 (variable.name exp0)))
               (return-simplified
                (make-conditional E1
                                  (if.then E0)
                                  (if.else E0)))
               (return-normally))))
        
        ((call? E0)
         (let ((exp0 (call.proc E0))
               (args (call.args E0)))
           (cond ((variable? exp0)
                  (let ((f (variable.name exp0)))
                    
                    (cond ((eq? T1 f)
                           (return-simplified (make-call E1 args)))
                          
                          ((and (not (null? args))
                                (variable? (car args))
                                (eq? T1 (variable.name (car args)))
                                (integrable? f))
                           (return-simplified
                            (make-call exp0
                                       (cons E1 (cdr args)))))
                          
                          (else
                           (return-normally)))))
                 
                 ((and (not L2)
                       (lambda? exp0)
                       (not (null? args))
                       (null? (cdr args)))
                  (post-simplify-anf
                   L0 T1 (car args) E1 free regbindings exp0))
                 
                 (else
                  (return-normally)))))
        
        ((and (assignment? E0)
              (variable? (assignment.rhs E0))
              (eq? T1 (variable.name (assignment.rhs E0))))
         (return-simplified (make-assignment (assignment.lhs E0) E1)))
        
        (else
         (return-normally))))
