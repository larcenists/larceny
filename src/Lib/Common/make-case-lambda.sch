; Copyright 2026 Felix S. Klock II.
;
; .make-case-lambda, which case-lambda expands into.  It is loaded
; early, before the first library file that uses case-lambda, so it
; uses nothing but primitives to make a procedure.  (It is loaded
; before $$trace is defined.)

; case-lambda expands into (.make-case-lambda '(formals ...) clause ...),
; where each clause is the lambda expression of the same formals.
; Where case-lambda:template is a procedure (case-lambda.mal), the
; result is a procedure with its code: its slots hold each clause's
; arity and the clause, then #f, and its code jumps to the first clause
; that accepts the arguments, consing nothing (see $arity-dispatch in
; Asm/Fence/pass5p2.sch).  An arity n >= 0 accepts n arguments, and
; -(m+1) accepts m or more.  Elsewhere, the result applies the clause
; to a list of the arguments.  A last clause, which accepts any number
; of arguments, signals that no clause accepts them.
;
; This procedure depends on the representation of procedures.

(define (.make-case-lambda formals . clauses)

  (define (arity formals)
    (let loop ((f formals) (m 0))
      (cond ((pair? f) (loop (cdr f) (+ m 1)))
            ((null? f) m)
            (else (- -1 m)))))

  (define (accepts? a n)
    (if (>= a 0)
        (= n a)
        (>= n (- -1 a))))

  (define (wrong-number-of-arguments . args)
    (assertion-violation 'case-lambda (errmsg 'msg:wna:caselambda) args))

  (let ((t case-lambda:template))       ; in Lib/Common/case-lambda.mal
    (if t
        (let loop ((k 0) (cs clauses))
          (if (pair? cs)
              (loop (+ k 1) (cdr cs))
              (let ((p (make-procedure (+ 5 (* 2 k)))))
                (procedure-set! p 0 (procedure-ref t 0))
                (procedure-set! p 1 (procedure-ref t 1))
                (let fill ((i 2) (fs formals) (cs clauses))
                  (if (pair? cs)
                      (begin (procedure-set! p i (arity (car fs)))
                             (procedure-set! p (+ i 1) (car cs))
                             (fill (+ i 2) (cdr fs) (cdr cs)))
                      (begin (procedure-set! p i -1)
                             (procedure-set! p (+ i 1)
                                             wrong-number-of-arguments)
                             (procedure-set! p (+ i 2) #f)
                             p))))))
        (lambda args
          (let ((n (length args)))
            (let loop ((fs formals) (cs clauses))
              (cond ((not (pair? cs))
                     (apply wrong-number-of-arguments args))
                    ((accepts? (arity (car fs)) n)
                     (apply (car cs) args))
                    (else
                     (loop (cdr fs) (cdr cs))))))))))

; eof
