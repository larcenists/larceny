(define-library (srfi 124)
  (export ephemeron?
          make-ephemeron
          ephemeron-broken?
          ephemeron-key
          ephemeron-datum
          reference-barrier
          )
  (import (scheme base)
          (rnrs hashtables)
          (primitives sro major-gc-counter))

  (include "124.body.scm"))
