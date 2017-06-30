(library (larceny compiler)
  (export load require r5rs:require
          current-require-path larceny:current-declared-features
          compile-file compile-library compile-stale-libraries
          compiler-switches
          compile-despite-errors
          issue-warnings
          include-procedure-names include-variable-names include-source-code
          avoid-space-leaks runtime-safety-checking catch-undefined-globals
          integrate-procedures
          faster-arithmetic
          control-optimization
          parallel-assignment-optimization
          lambda-optimization
          benchmark-mode
          global-optimization
          interprocedural-inlining
          interprocedural-constant-propagation
          common-subexpression-elimination
          representation-inference
          local-optimization
          peephole-optimization
          inline-allocation
          inline-assignment
          optimize-c-code)
  (import (rnrs base)
          (err5rs load)
          (primitives require r5rs:require
                      current-require-path larceny:current-declared-features
                      compile-r6rs-file compile-stale-libraries
                      compiler-switches
                      compile-despite-errors
                      issue-warnings
                      include-procedure-names
                      include-variable-names
                      include-source-code
                      avoid-space-leaks
                      runtime-safety-checking
                      catch-undefined-globals
                      integrate-procedures
                      faster-arithmetic
                      control-optimization
                      parallel-assignment-optimization
                      lambda-optimization
                      benchmark-mode
                      global-optimization
                      interprocedural-inlining
                      interprocedural-constant-propagation
                      common-subexpression-elimination
                      representation-inference
                      local-optimization
                      peephole-optimization
                      inline-allocation
                      inline-assignment
                      optimize-c-code))

  (define (compile-file src . rest)
    (compile-file-shared src rest #f))

  (define (compile-library src . rest)
    (compile-file-shared src rest #t))

  (define (compile-file-shared src rest libraries-only?)
    (cond ((null? rest)
           (compile-r6rs-file src #f libraries-only?))
          ((and (string? (car rest)) (null? (cdr rest)))
           (compile-r6rs-file src (car rest) libraries-only?))
          (else
           (assertion-violation
            (if libraries-only? 'compile-library 'compile-file)
            "too many arguments"
            (cons src rest)))))
  )
