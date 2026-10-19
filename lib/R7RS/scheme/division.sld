(define-library (scheme division)

  (export ceiling/ ceiling-quotient ceiling-remainder
          floor/ floor-quotient floor-remainder
          truncate/ truncate-quotient truncate-remainder
          round/ round-quotient round-remainder
          euclidean/ euclidean-quotient euclidean-remainder
          balanced/ balanced-quotient balanced-remainder)

  (import (srfi 141)))
