;;;
;;; Joint compilation example:
;;;
;;; Libraries and program all in the same file.
;;;
;;; See macros-test.scm for compilation script. 
;;;

(library (my-helpers id-stuff)
  (export find-dup)
  (import (rnrs))
  
  (define (find-dup l)
    (and (pair? l)
         (let loop ((rest (cdr l)))
           (cond ((null? rest)
                  (find-dup (cdr l)))
                 ((bound-identifier=? (car l) (car rest))
                  (car rest))
                 (else (loop (cdr rest))))))))
   
(library (my-helpers value-stuff)
  (export mvlet)
  (import (rnrs)
          (for (my-helpers id-stuff) expand))
  
  (define-syntax mvlet
    (lambda (stx)
      (syntax-case stx ()
        ((_ ((id ...) expr) body0 body ...)
         (not (find-dup (syntax (id ...))))
         (syntax
          (call-with-values
              (lambda () expr)
            (lambda (id ...) body0 body ...))))))))

(library (let-div)
  (export let-div)
  (import (rnrs) (my-helpers value-stuff))
  
  (define (quotient+remainder n d)
    (let ((q (floor (/ n d))))
      (values q (- n (* q d)))))   
  
  (define-syntax let-div
    (syntax-rules ()
      ((_ n d (q r) body0 body ...)
       (mvlet ((q r) (quotient+remainder n d))
              body0 body ...)))))

;;;
;;; Start of program:
;;;

(import (let-div) (rnrs)) 
  
(display (let-div 5 2 (q r) (+ q r)))  ;==> displays 3

