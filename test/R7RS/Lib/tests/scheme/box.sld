;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme box) procedures:
;;;
;;;     box
;;;     box?
;;;     unbox
;;;     set-box!


(define-library (tests scheme box)
  (export run-box-tests)
  (import (scheme base)
          (scheme box)
          (tests scheme test))

  ;; Adapted from srfi-111-test.sps

  (begin

   (define (run-box-tests)

     (define b1 (box 101))
     (define b2 (box 102))

     (test (box? b1) #t)

     (test (eqv? 101 (unbox b1)) #t)

     (test/unspec (set-box! b1 (unbox b2)))

     (test (eqv? 102 (unbox b1)) #t)

     (test (eqv? b1 b1) #t)
     (test (eqv? b2 b2) #t)
     (test (not (eqv? b1 b2)) #t)

     (test/unspec (set-box! b2 (square (unbox b1))))

     (test (eqv? 102 (unbox b1)) #t)
     (test (eqv? 10404 (unbox b2)) #t))))
