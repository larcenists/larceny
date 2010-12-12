;; format is either a Filename, or a (Filename . [List RequireSym])
(define benchmarks 
  '("overwrite-run-benchmark.sch"
    "earley.sch"
    "graphs.sch"
    "perm.sch"
    "nboyer.sch"
    "sboyer.sch"
    "gcbench.sch"
    "gcold.scm"
    "twobit.sch"
    "twobit-smaller.sch"
    ;; I can't get this working, even with directions in its comments.
    ;; "softscheme.sch" 
    "queue3.sch"
    "pueue3.sch"
    "graphs.sch"
    "num-iters.scm"
    "parsing.scm"
    "dynamic.scm"
    "paraffins.scm"
    ))

(for-each (lambda (x) 
            (let ((file (cond 
                         ((string? x) x)
                         (else
                          (display `("Requiring " ,@(cdr x)))
                          (newline)
                          (for-each require (cdr x))
                          (car x)))))
              (display (string-append "Compiling " file))
              (newline) 
              (compile-file file)))
          benchmarks)

