;;; (scheme ephemeron)
;;;
;;; R7RS Red Edition

(define-library (scheme ephemeron)
  (export ephemeron?
          make-ephemeron
          ephemeron-broken?
          ephemeron-key
          ephemeron-datum
          reference-barrier
          )
  (import (srfi 124)))
