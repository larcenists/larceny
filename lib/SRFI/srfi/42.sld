; SRFI 42
; Eager Comprehensions in [outer..inner|expr]-Convention
; ======================================================
;
; $Id$
;

(define-library (srfi 42 eager-comprehensions)

  (export do-ec list-ec append-ec string-ec string-append-ec
          vector-ec vector-of-length-ec sum-ec product-ec
          min-ec max-ec any?-ec every?-ec first-ec last-ec
          fold-ec fold3-ec
          : :list :string :vector :range :real-range :char-range :port
          :dispatched
          :integers
          :do :let :parallel :while :until
          :-dispatch-ref :-dispatch-set! make-initial-:-dispatch
          dispatch-union :generator-proc

          ; auxiliary keywords

          if and or begin let not
          nested index)

  (import (srfi :42 eager-comprehensions)))


(define-library (srfi 42)

  (export do-ec list-ec append-ec string-ec string-append-ec
          vector-ec vector-of-length-ec sum-ec product-ec
          min-ec max-ec any?-ec every?-ec first-ec last-ec
          fold-ec fold3-ec
          : :list :string :vector :range :real-range :char-range :port
          :dispatched
          :integers
          :do :let :parallel :while :until
          :-dispatch-ref :-dispatch-set! make-initial-:-dispatch
          dispatch-union :generator-proc

          ; auxiliary keywords

          if and or begin let not
          nested index)

  (import (srfi 42 eager-comprehensions)))

; eof
