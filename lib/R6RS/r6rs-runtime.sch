;;; 
;;; Runtime include file:
;;; Contains the minimal set of binding necessary
;;; for running a fully expanded program.
;;;

(define ex:unspecified (if #f #f))

(define (ex:make-library name envs exports imports builds visiter invoker build)
  (list name envs exports imports builds visiter invoker build))

(define (ex:library-name    lib) (car lib))
(define (ex:library-envs    lib) (cadr lib))
(define (ex:library-exports lib) (caddr lib))
(define (ex:library-imports lib) (cadddr lib))
(define (ex:library-builds  lib) (car (cddddr lib)))
(define (ex:library-visiter lib) (car (cdr (cddddr lib))))
(define (ex:library-invoker lib) (car (cdr (cdr (cddddr lib)))))
(define (ex:library-build   lib) (car (cdr (cdr (cdr (cddddr lib))))))

(define (ex:import-libraries-for imports builds phase importer)
  (let ((imported '()))
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
      (if (not (member (cons name phase) imported))
          (let ((library (ex:lookup-library name)))
            (or (not build)
                (eq? build (ex:library-build library))
                (assertion-violation 
                 'import "Client was expanded against a different build of this library" name))
            (import-libraries (ex:library-imports library) 
                              (ex:library-builds library)
                              phase)
            (importer library phase imported)
            (set! imported (cons (cons name phase) imported)))))
    (import-libraries imports builds phase)))

(define (ex:import-libraries-for-run imports builds phase)
  (ex:import-libraries-for imports 
                           builds
                           phase 
                           (lambda (library phase imported)
                             (if (= phase 0)
                                 ((ex:library-invoker library))))))

(define ex:register-library! #f)
(define ex:lookup-library    #f)
(let ((table '()))
  (set! ex:register-library! 
        (lambda (library)
          (set! table (cons library table))))
  (set! ex:lookup-library 
        (lambda (name)
          (let ((library (assoc name table)))
            (if library
                library
                (assertion-violation 'lookup-library "Library not loaded" name))))))

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
 