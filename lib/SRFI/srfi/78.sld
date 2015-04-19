;;; $Id$

(define-library (srfi 78 lightweight-testing)

  (export check check-ec check-report
          check-set-mode! check-reset! check-passed?)

  (import (srfi :78 lightweight-testing)))


(define-library (srfi 78)

  (export check check-ec check-report
          check-set-mode! check-reset! check-passed?)

  (import (srfi 78 lightweight-testing)))

; eof
