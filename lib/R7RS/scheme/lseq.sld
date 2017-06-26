;;; (scheme lseq)
;;;
;;; R7RS Red Edition

(define-library (scheme lseq)
  (export generator->lseq lseq? lseq=?
          lseq-car lseq-first lseq-cdr lseq-rest lseq-ref lseq-take lseq-drop
          lseq-realize lseq->generator lseq-length lseq-append lseq-zip
          lseq-map lseq-for-each lseq-filter lseq-remove
          lseq-find lseq-find-tail lseq-take-while lseq-drop-while
          lseq-any lseq-every lseq-index lseq-member lseq-memq lseq-memv
          )
  (import (srfi 127)))
