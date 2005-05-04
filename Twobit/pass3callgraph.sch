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
; The tail and non-tail call graphs of known and unknown procedures.
;
; Given an expression E returned by pass 2 of Twobit,
; returns a list of the following form:
;
; ((#t     L ()     <tailcalls> <nontailcalls> <size> #f)
;  (<name> L <vars> <tailcalls> <nontailcalls> <size> #f)
;  ...)
;
; where
;
; Each L is a lambda expression that occurs within E
; as either an escaping lambda expression or as a known
; procedure.  If L is a known procedure, then <name> is
; its name; otherwise <name> is #f.
;
; <vars> is a list of the non-global variables within whose
; scope L occurs.
;
; <tailcalls> is a complete list of names of known local procedures
; that L calls tail-recursively, disregarding calls from other known
; procedures or escaping lambda expressions that occur within L.
;
; <nontailcalls> is a complete list of names of known local procedures
; that L calls non-tail-recursively, disregarding calls from other
; known procedures or escaping lambda expressions that occur within L.
;
; <size> is a measure of the size of L, including known procedures
; and escaping lambda expressions that occur within L.

(define (callgraphnode.name x) (car x))
(define (callgraphnode.code x) (cadr x))
(define (callgraphnode.vars x) (caddr x))
(define (callgraphnode.tailcalls x) (cadddr x))
(define (callgraphnode.nontailcalls x) (car (cddddr x)))
(define (callgraphnode.size x) (cadr (cddddr x)))
(define (callgraphnode.info x) (caddr (cddddr x)))

(define (callgraphnode.size! x v) (set-car! (cdr (cddddr x)) v) #f)
(define (callgraphnode.info! x v) (set-car! (cddr (cddddr x)) v) #f)

(define (callgraph exp)

  ; Returns (union (list x) z).

  (define (adjoin x z)
    (if (memq x z)
        z
        (cons x z)))

  (let ((result '()))

    ; Given a <name> as described above, a lambda expression, a list
    ; of variables that are in scope, and a list of names of known
    ; local procedure that are in scope, computes an entry for L and
    ; entries for any nested known procedures or escaping lambda
    ; expressions, and adds them to the result.

    (define (add-vertex! name L vars known)

      (let ((tailcalls '())
            (nontailcalls '())
            (size 0))

        ; Given an expression, a list of variables that are in scope,
        ; a list of names of known local procedures that are in scope,
        ; and a boolean indicating whether the expression occurs in a
        ; tail context, adds any tail or non-tail calls to known
        ; procedures that occur within the expression to the list
        ; variables declared above.

        (define (graph! exp vars known tail?)
          (set! size (+ size 1))
          (cond ((constant? exp)    #f)

                ((lambda? exp)   (add-vertex! #f exp vars known)
                 (set! size
                       (+ size
                          (callgraphnode.size (car result)))))

                ((assignment? exp)     (graph! (assignment.rhs exp) vars known #f))

                ((conditional? exp)       (graph! (if.test exp) vars known #f)
                 (graph! (if.then exp) vars known tail?)
                 (graph! (if.else exp) vars known tail?))

                ((variable? exp) #f)

                ((begin? exp)
                 (do ((exprs (begin.exprs exp) (cdr exprs)))
                     ((null? (cdr exprs))
                      (graph! (car exprs) vars known tail?))
                   (graph! (car exprs) vars known #f)))

                ((call? exp)       (let ((proc (call.proc exp)))
                                     (cond ((variable? proc)
                                            (let ((name (variable.name proc)))
                                              (if (memq name known)
                                                  (if tail?
                                                      (set! tailcalls
                                                            (adjoin name tailcalls))
                                                      (set! nontailcalls
                                                            (adjoin name nontailcalls))))))
                                           ((lambda? proc)
                                            (graph-lambda! proc vars known tail?))
                                           (else
                                            (graph! proc vars known #f)))
                                     (for-each (lambda (exp)
                                                 (graph! exp vars known #f))
                                               (call.args exp))))
                (else
                 (error "Unrecognized expression" exp))))

;        (define (graph! exp vars known tail?)
;          (set! size (+ size 1))
;          (case (car exp)

;            ((quote)    #f)

;            ((lambda)   (add-vertex! #f exp vars known)
;                        (set! size
;                              (+ size
;                                 (callgraphnode.size (car result)))))

;            ((set!)     (graph! (assignment.rhs exp) vars known #f))

;            ((if)       (graph! (if.test exp) vars known #f)
;                        (graph! (if.then exp) vars known tail?)
;                        (graph! (if.else exp) vars known tail?))

;            ((begin)    (if (not (variable? exp))
;                            (do ((exprs (begin.exprs exp) (cdr exprs)))
;                                ((null? (cdr exprs))
;                                 (graph! (car exprs) vars known tail?))
;                                (graph! (car exprs) vars known #f))))

;            (else       (let ((proc (call.proc exp)))
;                          (cond ((variable? proc)
;                                 (let ((name (variable.name proc)))
;                                   (if (memq name known)
;                                       (if tail?
;                                           (set! tailcalls
;                                                 (adjoin name tailcalls))
;                                           (set! nontailcalls
;                                                 (adjoin name nontailcalls))))))
;                                 ((lambda? proc)
;                                  (graph-lambda! proc vars known tail?))
;                                 (else
;                                  (graph! proc vars known #f)))
;                          (for-each (lambda (exp)
;                                      (graph! exp vars known #f))
;                                    (call.args exp))))))

        (define (graph-lambda! L vars known tail?)
          (let* ((defs (lambda.defs L))
                 (newknown (map def.lhs defs))
                 (vars (append newknown
                               (make-null-terminated
                                (lambda.args L))
                               vars))
                 (known (append newknown known)))
            (for-each (lambda (def)
                        (add-vertex! (def.lhs def)
                                     (def.rhs def)
                                     vars
                                     known)
                        (set! size
                              (+ size
                                 (callgraphnode.size (car result)))))
                      defs)
            (graph! (lambda.body L) vars known tail?)))

        (graph-lambda! L vars known #t)

        (set! result
              (cons (list name L vars tailcalls nontailcalls size #f)
                    result))))

    (add-vertex! #t
                 (make-lambda '() '() '() '() '() '() '() exp)
                 '()
                 '())
    result))

; Displays the callgraph, for debugging.

(define (view-callgraph g)
  (for-each (lambda (entry)
              (let ((name (callgraphnode.name entry))
                    (exp  (callgraphnode.code entry))
                    (vars (callgraphnode.vars entry))
                    (tail (callgraphnode.tailcalls entry))
                    (nt   (callgraphnode.nontailcalls entry))
                    (size (callgraphnode.size entry)))
                (cond ((symbol? name)
                       (write name))
                      (name
                       (display "TOP LEVEL EXPRESSION"))
                      (else
                       (display "ESCAPING LAMBDA EXPRESSION")))
                (display ":")
                (newline)
                (display "Size: ")
                (write size)
                (newline)
                ;(newline)
                ;(display "Variables in scope: ")
                ;(write vars)
                ;(newline)
                (display "Tail calls:     ")
                (write tail)
                (newline)
                (display "Non-tail calls: ")
                (write nt)
                (newline)
                ;(newline)
                ;(pretty-print (make-readable exp))
                ;(newline)
                ;(newline)
                (newline)))
            g))
