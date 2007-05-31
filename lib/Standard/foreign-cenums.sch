(require 'std-ffi)
(require 'foreign-ctools)

(define-syntax define-c-enum
  (syntax-rules ()
    ((define-c-enum enum-type (ctools-decls ...)
       (scm-name c-name) ...)
     (begin
       (define enum-type
         (let ()
           (define-c-info ctools-decls ... (const scm-name int c-name) ...)
           (let* ((sym2cval-table (quasiquote ((scm-name ,scm-name) ...)))
                  (cval2sym-table (quasiquote ((,scm-name scm-name) ...)))
                  (sym->cval (lambda (sym)
                               (cond ((assq sym sym2cval-table) => cadr)
                                     (else (error 'enum-type ": unknown name " sym)))))
                  (cval->sym (lambda (cval)
                               (cond ((assq cval cval2sym-table) => cadr)
                                     (else (error 'enum-type ": unknown value " cval)))))
                  (high->low (lambda (x name) 
                               (cond ((fixnum? x) x)
                                     ((symbol? x) (sym->cval x))
                                     (else (error 'enum-type ": invalid input " x)))))
                  (low->high (lambda (x name)
                               (cval->sym x))))
             (ffi-add-attribute-core-entry! 'enum-type 'signed32 high->low low->high)
             sym2cval-table
             )))))))

                                        
