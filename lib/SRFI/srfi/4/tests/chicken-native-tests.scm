(import scheme)
(import (only (chicken base) include define-record-type))
(import (srfi 4))

; Bug in Chicken 5 release version

(define (list->s64vector list)
  (let ((vec (make-s64vector (length list))))
    (let loop ((i 0) (list list))
      (cond
        ((null? list) vec)
        (else
          (s64vector-set! vec i (car list))
          (loop (+ 1 i) (cdr list)))))))

(include "srfi/4/shared-tests.scm")
