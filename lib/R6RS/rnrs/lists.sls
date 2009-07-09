(library (rnrs lists (6))
  (export find for-all exists filter partition fold-left fold-right
          remp remove remq remv memp member memv memq
          assp assoc assv assq cons*)
  (import (rnrs base)
          (primitives 
           find for-all exists filter partition fold-left larceny:fold-right
           remp larceny:remove remq remv memp larceny:member memv memq
           assp larceny:assoc assv assq cons*))

  ;; These are redefined by SRFI 1, which leaks into
  ;; the IAssassin R5RS top level.

  (define fold-right larceny:fold-right)
  (define remove larceny:remove)
  (define member larceny:member)
  (define assoc larceny:assoc)
)
