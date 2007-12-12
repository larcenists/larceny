;;; 
;;; Runtime include file:
;;; Contains the minimal set of binding necessary
;;; for running a fully expanded program.
;;;

(define ex:unspecified (if #f #f))

(define (ex:make-library name envs exports imports builds visiter invoker build)
  (list name envs exports imports builds visiter invoker build #f #f))

(define (ex:library-name     lib) (car lib))
(define (ex:library-envs     lib) (cadr lib))
(define (ex:library-exports  lib) (caddr lib))
(define (ex:library-imports  lib) (cadddr lib))
(define (ex:library-builds   lib) (car (cddddr lib)))
(define (ex:library-visiter  lib) (car (cdr (cddddr lib))))
(define (ex:library-invoker  lib) (car (cdr (cdr (cddddr lib)))))
(define (ex:library-build    lib) (car (cdr (cdr (cdr (cddddr lib))))))
(define (ex:library-visited? lib) (car (cdr (cdr (cdr (cdr (cddddr lib)))))))
(define (ex:library-invoked? lib) (car (cdr (cdr (cdr (cdr (cdr (cddddr lib))))))))

(define (ex:library-visited?-set! lib b) (set-car! (cdr (cdr (cdr (cdr (cddddr lib))))) b))
(define (ex:library-invoked?-set! lib b) (set-car! (cdr (cdr (cdr (cdr (cdr (cddddr lib)))))) b))

(define ex:imported '())
(define (ex:import-libraries-for imports builds phase importer run-or-expand)
  (define (import-libraries imports builds phase)
    (for-each (lambda (import build)
                (let ((name   (car import))
                      (levels (cdr import)))
                  (for-each (lambda (level)
                              (import-library name build (+ phase level)))
                            levels)))
              imports
              builds)
    (values))
  (define (import-library name build phase)
    (if (not (member (cons name (cons phase run-or-expand)) ex:imported))
        (let ((library (ex:lookup-library name)))
          (or (not build)
              (eq? build (ex:library-build library))
              (assertion-violation 
               'import "Client was expanded against a different build of this library" name))
          (import-libraries (ex:library-imports library) 
                            (ex:library-builds library)
                            phase)
          (importer library phase ex:imported)
          (set! ex:imported (cons (cons name (cons phase run-or-expand)) ex:imported)))))
  (import-libraries imports builds phase))

(define (ex:import-libraries-for-run imports builds phase)
  (ex:import-libraries-for imports 
                           builds
                           phase 
                           (lambda (library phase imported)
                             (if (and (= phase 0)
                                      (not (ex:library-invoked? library)))
                                 (begin 
                                   ((ex:library-invoker library))
                                   (ex:library-invoked?-set! library #t))))
                           'run))

(define ex:register-library! #f)
(define ex:lookup-library    #f)
(let ((table '()))
  (set! ex:register-library! 
        (lambda (library)
          (set! table (cons library table))
          (set! ex:imported (filter (lambda (entry)
                                      (not (equal? (ex:library-name library) 
                                                   (car entry))))
                                    ex:imported))))
  (set! ex:lookup-library 
        (lambda (name)
          (let ((library (assoc name table)))
            (cond (library library)
                  ((larceny:autoload-r6rs-library name)
                   (ex:lookup-library name))
                  (else
                   (assertion-violation 'lookup-library
                                        "library not loaded" name)))))))

;; Only instantiate part of the bootstrap library 
;; that would be needed for invocation at runtime.

(ex:register-library! 
 (let ((error (lambda () 
                (assertion-violation 
                 'runtime.scm
                 "Attempt to use runtime instance of (core primitive-macros) for expansion.  Make sure expander.scm is loaded after runtime.scm."))))
   (ex:make-library
    '(core primitive-macros)
    ;; envs
    error
    ;; exports
    '()
    ;; imported-libraries
    '()
    ;; builds
    '()
    ;; visiter
    error
    ;; invoker
    (lambda () (values))
    ;; build
    'system)))
