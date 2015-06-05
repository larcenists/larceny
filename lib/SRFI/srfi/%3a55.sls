;;; SRFI 55: require-extension
;;;
;;; $Id$

(library (srfi :55)

  (export require-extension)

  (import (for (scheme base) run expand)
          (for (explicit-renaming) expand))

  (begin

   ;; The hygienic definition doesn't work, probably because it
   ;; introduces a new scope that hides the imported bindings.

#;
   (define-syntax require-extension
     (syntax-rules (srfi)
      ((_ (srfi n ...) ...)
       (cond-expand
        ((and (library (srfi n)) ... ...)
         (import (srfi n)
                 ...
                 ...))))))

   ;; This seems to work.

   (define-syntax require-extension
     (er-transformer
      (lambda (exp rename compare)
        (let* ((k (car exp))
               (clauses (cdr exp))
               (libnames (apply append
                                (map (lambda (clause)
                                       (let ((id (car clause))) ; usually srfi
                                         (map (lambda (n)
                                                (list id n))
                                              (cdr clause))))
                                     clauses))))
          `(,(datum->syntax k 'import)
            ,@libnames)))))

   ))

; eof
