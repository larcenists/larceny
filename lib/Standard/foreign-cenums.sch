(require 'std-ffi)
(require 'foreign-ctools)

(define enum-table->high2low
  (lambda (enum-type-name sym2cval-table)
    (let* ((sym->cval
            (lambda (sym)
              (cond ((assq sym sym2cval-table) => cadr)
                    (else (error enum-type-name ": unknown name " sym))))))
      (define (high->low x . name) 
        (cond ((fixnum? x) x)
              ((symbol? x) (sym->cval x))
              (else (error
                     (if (pair? name) (car name) enum-type-name)
                    ": invalid input " x))))
      high->low)))

(define enum-table->set-high2low
  (lambda (enum-type-name sym2cval-table univ)
    (let ((enum-set? (record-predicate (record-type-descriptor (make-enumeration '()))))
          (high2low (enum-table->high2low enum-type-name sym2cval-table)))
      (lambda (eset . name)
        (assert (enum-set? eset))
        (assert (enum-set-subset? eset univ))
        (foldr fxlogior 0 (map high2low (enum-set->list eset)))))))

(define enum-table->low2high
  (lambda (enum-type-name sym2cval-table)
    (let* ((cval2sym-table (map reverse sym2cval-table))
           (cval->sym (lambda (cval)
                        (cond ((assq cval cval2sym-table) => cadr)
                              (else (error enum-type-name
                                           ": unknown value " cval))))))
      (define (low->high x . name)
        (cval->sym x))
      low->high)))

(define enum-table->set-low2high
  (lambda (enum-type-name sym2cval-table univ)
    (let ((high2low (enum-table->high2low enum-type-name sym2cval-table))
          (ctor (enum-set-constructor univ)))
      (lambda (bits . name)
        (ctor (filter
               (lambda (sym)
                 (not (zero? (fxlogand (high2low sym) bits))))
               (enum-set->list univ)))))))

(define-syntax define-c-enum
  (syntax-rules ()
    ((define-c-enum enum-type (ctools-decls ...)
       (scm-name c-name) ...)
     (begin
       (define enum-type
         (let ()
           (define-c-info ctools-decls ... (const scm-name int c-name) ...)
           (let* ((sym2cval-table (quasiquote ((scm-name ,scm-name) ...)))
                  (high->low (enum-table->high2low 'enum-type
                                                   sym2cval-table))
                  (low->high (enum-table->low2high 'enum-type
                                                   sym2cval-table)))
             (ffi-add-attribute-core-entry! 'enum-type 'signed32
                                            high->low low->high)
             (list sym2cval-table high->low low->high)
             )))))))

(define-syntax define-c-enum-set 
  (syntax-rules ()
    ((define-c-enum-set enum-type (ctools-decls ...)
       (scm-name c-name) ...)
     (begin
       (define enum-type
         (let ((univ (make-enumeration (list (quote scm-name) ...))))
           (define-c-info ctools-decls ... (const scm-name int c-name) ...)
           (let* ((sym2cval-table (quasiquote ((scm-name ,scm-name) ...)))
                  (high->low (enum-table->set-high2low 'enum-type 
                                                       sym2cval-table
                                                       univ))
                  (low->high (enum-table->set-low2high 'enum-type
                                                       sym2cval-table
                                                       univ)))
             (ffi-add-attribute-core-entry! 'enum-type 'signed32 
                                            high->low low->high)
             (enum-set-constructor univ)
             )))))))

(define enum->marshall-out cadr)
(define enum->marshall-in caddr)
