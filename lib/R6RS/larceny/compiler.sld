(define-library (larceny compiler)
  (export load
          require
          r5rs:require
          current-require-path
          current-directory
          larceny:current-declared-features
          compile-file
          compile-library
          compile-stale-libraries
          compile-stale
          compile-stale-cautiously
          compile-stale-regardless
          compile-stale-recklessly
          compiler-switches
          compile-despite-errors
          issue-warnings
          include-procedure-names include-variable-names include-source-code
          hygienic-literals
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
          (larceny compile-stale)
          (primitives require r5rs:require
                      current-require-path
                      current-directory
                      larceny:current-declared-features
                      compile-stale-libraries
                      compiler-switches
                      compile-despite-errors
                      issue-warnings
                      include-procedure-names
                      include-variable-names
                      include-source-code
                      hygienic-literals
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
                      optimize-c-code)))
