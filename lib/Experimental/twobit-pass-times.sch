(require "Experimental/accum-profile")

;; find-entry : Symbol Sexp -> [Maybe (cons Procedure Symbol)]
;; Traverses sexp looking for a pair of form (cons Procedure key)
(define find-entry 
  (lambda (key sexp)
    (let recur ((sexp sexp))
      (cond ((pair? sexp) 
             (cond ((and (procedure? (car sexp)) (eq? key (cdr sexp)))
                    sexp)
                   (else (or (recur (car sexp)) (recur (cdr sexp))))))
            ((vector? sexp) (recur (vector->list sexp)))
            (else #f)))))

;; find-subprocedure : Symbol Procedure -> [Maybe Procedure]
;; Traverses constant vector of proc trying to find entry matching
;; name.  (does not recursively descend proc; doing so permits
;; structure-shy client code, but would require handling cycles)
(define find-subprocedure 
  (lambda (name proc)
    (cond ((find-entry name (procedure-ref proc 1)) => car)
          (else #f))))

(define twobit-pass-times 
  (let* (
         (ap (make-accum-profile))
         (assemble (find-subprocedure 'assemble compile-file))
         (assembly-postpass-segment 
          (find-subprocedure 'assembly-postpass-segment assemble))
         (sassy-assemble 
          (find-subprocedure 'sassy-assemble assembly-postpass-segment))
         (sassy 
          (find-subprocedure 'sassy sassy-assemble))
         (compile (find-subprocedure 'compile compile-file))
         (pass1 (find-subprocedure 'pass1 compile))
         (twobit-expand (find-subprocedure 'twobit-expand pass1))
         (desugar-definitions (find-subprocedure 'desugar-definitions twobit-expand))
         (syntactic-lookup (find-subprocedure 'syntactic-lookup desugar-definitions))
         (pass2 (find-subprocedure 'pass2 compile))
         (pass3 (find-subprocedure 'pass3 compile))
         (pass4 (find-subprocedure 'pass4 compile)))
    (instrument-accumulating-time! ap 'asm:sassy sassy)
    (instrument-accumulating-time! ap 'assemble assemble)
    (instrument-accumulating-time! ap 'compile compile)
    (instrument-accumulating-time! ap 'twobit:pass4 pass4)
    (instrument-accumulating-time! ap 'twobit:pass3 pass3)
    (instrument-accumulating-time! ap 'twobit:pass2 pass2)
    ;; Still too slow... 
    ;(instrument-accumulating-time/pure! ap 'twobit:syntactic-lookup syntactic-lookup)
    (instrument-accumulating-time! ap 'twobit:pass1 pass1)
    
    (lambda ()
      (accum-profile-times ap))))
