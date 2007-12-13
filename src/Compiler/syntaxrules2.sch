; Copyright 2007 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; $Id$
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Optimizer for a <transformer spec>.
;
; The representations of macro denotations and compiled rules
; are described in syntaxenv.sch and syntaxrules.sch.  For an
; unoptimized macro denotation, the macro expander just tries
; all of the rules in order.
;
; If there are a lot of rules, that takes a long time.
; The optimizer in this file tries to add a special-purpose
; hash function and hashed rules table to macro denotations.
;
; FIXME:  This optimizer is woefully special-purpose.
;
; Pattern optimization is an interesting problem in general,
; but this optimizer was added to handle patterns of the form
;
;     (<symbol> <symbol> P)
;
; where the symbols have their usual identifier denotation
; in the environment of definition.


(define (m-optimize-macro-denotation d)

  (let* ((rules     (macro-rules d))
         (env       (macro-env d))
         (threshold 10))

    (define (usual-denotation? sym env)
      (let ((symd (syntactic-lookup env sym)))
        (and (identifier-denotation? symd)
             (same-denotation? symd (make-identifier-denotation sym)))))

    (define (starts-with-two-symbols? x env)
      (and (pair? x)
           (pair? (cdr x))
           (symbol? (car x))
           (symbol? (cadr x))
           (usual-denotation? (car x) env)
           (usual-denotation? (cadr x) env)))

    (cond ((> (length rules) threshold)

           (let* ((patterns (map car rules))
                  (easy&hard (do ((rules rules (cdr rules))
                                  (easy '() (cons (car rules) easy)))
                                 ((or (null? rules)
                                      (not (starts-with-two-symbols?
                                            (car (car rules)) env)))
                                  (list (reverse easy) rules))))
                  (easy (car easy&hard))
                  (hard (cadr easy&hard))
                  (n    (length easy)))

             (if (< n threshold)

                 d

                 (let* ((m   (+ n (quotient n 2)))
                        (m-1 (- m 1))
                        (h (lambda (F env)
                             (cond ((starts-with-two-symbols? F env)
                                    (let ((h1 (symbol-hash (car F)))
                                          (h2 (symbol-hash (cadr F))))
                                      (+ 1 (remainder (+ h1 h1 h2) m-1))))
                                   (else 0))))
                        (v (make-vector m hard)))

                   (for-each (lambda (rule)
                               (let ((i (h (car rule) env)))
                                 (vector-set! v
                                              i
                                              (cons rule (vector-ref v i)))))

                             ; Order matters here.

                             (reverse easy))

                   (make-macro-denotation-optimized rules env h v)))))

          (else d))))
