(library (rnrs syntax-case (6))
  
  (export make-variable-transformer
          identifier? bound-identifier=? free-identifier=?
          generate-temporaries datum->syntax syntax->datum 
          syntax-violation syntax syntax-case quasisyntax 
          unsyntax unsyntax-splicing with-syntax 
          _ ...)
  
  (import (core primitives)
          (core with-syntax)  
          (core quasisyntax))
  
  ) ;; rnrs syntax-case

