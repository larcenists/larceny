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

  ; Twobit names the clauses of a case-lambda that is defined or
  ; assigned (name-procedures! in Compiler/expand.sch).  The template's
  ; constant vector holds only its documentation, #(name ...); a copy
  ; of it, renamed for the first clause, makes procedure-name work.

  (define (clause-name c)
    (let* ((cv (procedure-ref c 1))
           (doc (and (vector? cv) (vector-ref cv 0)))
           (doc (if (and (pair? doc) (pair? (car doc)))
                    (cdr (car doc))                  ; ((pc . doc) ...)
                    doc)))
      (and (vector? doc)
           (< 0 (vector-length doc))
           (vector-ref doc 0))))

  (define (copy-vector v)
    (let* ((n (vector-length v))
           (w (make-vector n #f)))
      (do ((i 0 (+ i 1)))
          ((= i n) w)
        (vector-set! w i (vector-ref v i)))))

  (define (named-constants cv)
    (let ((name (and (pair? clauses) (clause-name (car clauses)))))
      (if name
          (let ((cv (copy-vector cv))
                (doc (copy-vector (vector-ref cv 0))))
            (vector-set! doc 0 name)
            (vector-set! cv 0 doc)
            cv)
          cv)))

  (define (wrong-number-of-arguments . args)
    (assertion-violation 'case-lambda (errmsg 'msg:wna:caselambda) args))

  (let ((t case-lambda:template))       ; in Lib/Common/case-lambda.mal
    (if t
        (let loop ((k 0) (cs clauses))
          (if (pair? cs)
              (loop (+ k 1) (cdr cs))
              (let ((p (make-procedure (+ 5 (* 2 k)))))
                (procedure-set! p 0 (procedure-ref t 0))
                (procedure-set! p 1 (named-constants (procedure-ref t 1)))
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

;; The name of .make-case-lambda in R6RS libraries, where identifiers
;; cannot begin with a dot (see lib/R6RS/rnrs/control.sls).

(define case-lambda:make .make-case-lambda)

; eof
