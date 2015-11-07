;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (scheme cxr) procedures:
;;;
;;;     caar
;;;     cadr
;;;     cdar
;;;     cddr
;;;
;;;     caaar
;;;     caadr
;;;     cadar
;;;     caddr
;;;     cdaar
;;;     cdadr
;;;     cddar
;;;     cdddr
;;;
;;;     caaaar
;;;     caaadr
;;;     caadar
;;;     caaddr
;;;     cadaar
;;;     cadadr
;;;     caddar
;;;     cadddr
;;;
;;;     cdaaar
;;;     cdaadr
;;;     cdadar
;;;     cdaddr
;;;     cddaar
;;;     cddadr
;;;     cdddar
;;;     cddddr


(define-library (tests scheme cxr)
  (export run-cxr-tests)
  (import (scheme base)
          (scheme cxr)
          (tests scheme test))

  (begin
   (define (run-cxr-tests)

     (test (cadr '(1 2)) 2)
     (test (cddr '(1 2)) '())
     (test (cdar '((1) 2)) '())
     (test (caar '((1) 2)) 1)

     (test (cadar '((1 2))) 2)
     (test (cddar '((1 2))) '())
     (test (cdaar '(((1) 2))) '())
     (test (caaar '(((1) 2))) 1)
     (test (caddr '(0 1 2)) 2)
     (test (cdddr '(0 1 2)) '())
     (test (cdadr '(0 (1) 2)) '())
     (test (caadr '(0 (1) 2)) 1)

     (test (cadaar '(((1 2)))) 2)
     (test (cddaar '(((1 2)))) '())
     (test (cdaaar '((((1) 2)))) '())
     (test (caaaar '((((1) 2)))) 1)
     (test (caddar '((0 1 2))) 2)
     (test (cdddar '((0 1 2))) '())
     (test (cdadar '((0 (1) 2))) '())
     (test (caadar '((0 (1) 2))) 1)
     (test (cadadr '(- (1 2))) 2)
     (test (cddadr '(- (1 2))) '())
     (test (cdaadr '(- ((1) 2))) '())
     (test (caaadr '(- ((1) 2))) 1)
     (test (cadddr '(- 0 1 2)) 2)
     (test (cddddr '(- 0 1 2)) '())
     (test (cdaddr '(- 0 (1) 2)) '())
     (test (caaddr '(- 0 (1) 2)) 1)

     ;;;
     )))
