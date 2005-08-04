;;; -*-Mode: Scheme; coding: iso-8859-1 -*-

;;;===============================================================================
;;;
;;; Simple Hygienic Macros and Simple Modules:
;;;
;;;   Copyright (c) 2005 André van Tonder
;;;
;;;   Permission is hereby granted, free of charge, to any person obtaining a
;;;   copy of this software and associated documentation files (the ``Software''),
;;;   to deal in the Software without restriction, including without limitation
;;;   the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;   and/or sell copies of the Software, and to permit persons to whom the
;;;   Software is furnished to do so, subject to the following conditions:
;;;
;;;   The above copyright notice and this permission notice shall be included in
;;;   all copies or substantial portions of the Software.
;;;
;;;   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;;   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;   DEALINGS IN THE SOFTWARE.
;;;
;;;===============================================================================

;;; Ported to Common Larceny by jrm

;;; Fix spelling of color
;;; Add second arg to all calls to gensym
;;; Make *source-stack* a parameter.
;;; Make globally unique names.
;;; Make identifiers be a class.
;;; Move free=? to only place it was used.
;;; Move literal=? to only place it was used.
;;; Move relevant procedures to inside of compress-envs and uncommpress-envs
;;; Make *current-module-name* be a parameter.
;;; Move compression to separate file.
;;; Move identifier to separate file.
;;; Pass namespace as argument.

;;; Portability hooks:

;; Try this if your Scheme does not have interaction-environment
;; EVAL.  You may need to insert code to delete the temporary file.

; (define eval
;   (let ((old-eval eval))
;     (lambda (e . maybe-env)
;       (if (null? maybe-env)
;           (begin
;             (call-with-output-file "tmp" (lambda (port) (write e port)))
;             (load "tmp"))
;           (old-eval e (car maybe-env))))))


;; Stopgap if you do not have GENSYM.

; (define gensym
;   (let ((count 0))
;     (lambda maybe-prefix-string
;       (set! count (+ 1 count))
;       (string->symbol
;        (string-append (if (null? maybe-prefix-string)
;                           ""
;                           (car maybe-prefix-string))
;                       " "
;                       (number->string count))))))

;; For Gambit, uncomment the following:
;; Use Gambit's gensym, but allow for an optional string prefix
;;   (as well as symbol prefix) - Ken Dickey.
;; [##gensym is an internal/primop function so I can load the code repeatedly w/o
;; re-re-redefining gensym].

; (define (gensym . optional-prefix)
;   (let ((prefix (if (null? optional-prefix) 'g (car optional-prefix))))
;     (##gensym (if (string? prefix) (string->symbol prefix) prefix))))

;; Used below for writing primitive macros in explicit renaming style.

(define original-macro-expander #f)

(define (install-simple-macros! kernel-environment)
  (if (not original-macro-expander)
      (set! original-macro-expander (macro-expander)))
  (environment-set-auxiliary-info! kernel-environment (make <environment-auxinfo>))
  (expansion-environment kernel-environment)
  (initialize-kernel-namespace kernel-environment)
  (macro-expander expand-toplevel))

(define (uninstall!)
  (if original-macro-expander
      (begin
        (macro-expander original-macro-expander)
        (set! original-macro-expander #f))))

(define (eval-for-expander form)
  (interpret-code-object
   (original-macro-expander form (interaction-environment))
   (interaction-environment)))

(define (make-primitive-renaming-procedure namespace)
  (let ((rename
         (make-renaming-procedure namespace (generate-color) empty-env #f)))
    (lambda (symbolic-name)
      (rename symbolic-name
              symbolic-name))))

;=========================================================================

;;; Expander dispatch:

(define (make-operator-predicate name)
  (define (free=? x symbol)
    (and (identifier? x)
         (eq? (binding-name x) symbol)))

  (lambda (t)
    (and (pair? t)
         (free=? (car t) name))))

(define (expand form namespace)
  (syntax-trace 3 'expand form namespace)
  (parameterize ((*source-stack* (cons form (*source-stack*))))
    (cond ((pair? form)
           (let ((operator (car form)))
             (if (identifier? operator)
                 (cond ((env/lookup-syntax namespace operator)
                        => (lambda (transformer)
                             (transformer
                              (lambda (new-form)
                                (expand new-form namespace))
                              form
                              namespace)))
                       (else (let ((operator-name (binding-name operator)))
                               (cons operator-name
                                     (map-in-order (lambda (subform)
                                                     (expand subform namespace))
                                                   (cdr form))))))
                 (map-in-order (lambda (subform)
                                 (expand subform namespace))
                               form))))

          ((identifier? form) (binding-name form))
          ((const?      form) form)
          (else
           (syntax-error "Expand: Invalid syntax object: " form)))))

(define *builtin-special-forms*
  '(
    define
    define-syntax
    lambda
    let-syntax
    letrec-syntax
    module
    quote
    set-syntax!
    with-fresh-renaming-scope
    ))

(define (head-expand form namespace)
  (syntax-trace 4 'head-expand form namespace)
  (cond ((and (pair? form)
              (identifier? (car form))
              (not (member (binding-name (car form)) *builtin-special-forms*))
              (env/lookup-syntax namespace (car form)))
         => (lambda (transformer)
              (syntax-trace 4 'head-expand "transformer" transformer form)
              (transformer
               (lambda (new-form)
                 (head-expand new-form namespace))
               form
               namespace)))
        (else form)))

(define (expand-quote re-expand form namespace)
  (syntax-trace 5 'expand-quote form)
  (syntax-object->datum form))

;====================================================================================

;; Used by SYNTAX and QUASISYNTAX to generate a fresh renaming procedure in the
;; enclosed lexical scope, *unless* we are already inside an enclosing QUASISYNTAX:

(define *current-renamer*
  (make-parameter "*current-renamer*" 'no-root-renamer))

(define *inside-quasisyntax* (make-parameter "*inside-quasisyntax*" #f))

(define (expand-fresh-scope re-expand form namespace)
  (or (and (list? form) (>= (length form) 1))
      (syntax-error))
  (if (*inside-quasisyntax*)
      (expand (cadr form) namespace)
      (parameterize ((*current-renamer* (gensym "renamer-"))
                     (*inside-quasisyntax* #t))
        `(let ((,(*current-renamer*) (make-meta-renaming-procedure)))
           ,(expand (cadr form) namespace)))))

;====================================================================================

;;; QUASISYNTAX and QUASIQUOTE:

(define (expand-quasisyntax re-expand form namespace)
  (syntax-trace 2 'expand-quasisyntax form)
  (or (and (pair? (cdr form))
           (null? (cddr form)))
      (syntax-error "Quasisyntax:  Should have one argument"))
  (let ((rename (make-primitive-renaming-procedure namespace)))
    (re-expand
     `(,(rename 'with-fresh-renaming-scope)
       ,(quasi rename
               (cadr form)
               'quasisyntax
               (lambda (id) `(,(rename 'syntax) ,id)))))))

(define (expand-quasiquote re-expand form namespace)
  (syntax-trace 2 'expand-quasiquote form)
  (or (and (pair? (cdr form))
           (null? (cddr form)))
      (syntax-error "Quasiquote:  Should have one argument"))
  (let ((rename (make-primitive-renaming-procedure namespace)))
    (re-expand
     (quasi rename
            (cadr form)
            'quasiquote
            (lambda (id) `(,(rename 'quote) ,id))))))


;;=================================================================================

;;; SYNTAX:

(define (expand-syntax0 re-expand form namespace)
  (syntax-trace 5 'expand-syntax0 form)
  (let ((r (make-primitive-renaming-procedure namespace)))
    (define (descend x)
      (cond ((pair? x)
             `(,(r 'cons)
                ,(descend (car x))
                ,(descend (cdr x))))
            ((vector? x)
             `(,(r 'list->vector)
               ,(descend (vector->list x))))
            ((identifier? x)
             `(,(r (*current-renamer*)) (,(r 'expansion-environment)) ,@(reflect-syntax x namespace)))
            ((const? x)
             `(,(r 'quote) ,x))
            (else
             (syntax-error "Syntax: Invalid element in syntax object:" form))))
    (re-expand (descend (cadr form)))))

(define (expand-syntax re-expand form namespace)
  (syntax-trace 4 'expand-syntax form)
  (or (and (pair? (cdr form))
           (null? (cddr form)))
      (syntax-error "Syntax:  Should have one argument"))
  (let ((rename (make-primitive-renaming-procedure namespace)))
    (re-expand `(,(rename 'with-fresh-renaming-scope) (,(rename 'syntax0) ,(cadr form))))))

;;;============================================================================

;; Generating representations of identifiers that can be used
;; in generated code.  We avoid embedding substitution environments,
;; which can be large, instead using indirection to existing expand-
;; time objects (CLOSE-ENVIRONMENT returns a key into *environments*).
;; For separate compilation, substitution environments are
;; serialized as part of the compiled representation of a module.

(define (reflect-syntax id namespace)
  (let ((r (make-primitive-renaming-procedure namespace)))
    `((,(r 'quote) ,(r (color            id)))
      (,(r 'quote) ,(r (symbolic-name     id)))
      (,(r 'quote) ,(r (binding-name      id)))
      (,(r 'quote) ,(r (close-environment namespace id))))))

;===============================================================================

;; As opposed to SYNTAX, EMBEDDED-SYNTAX will not add any color to its argument.
;; This is useful for certain macro-defining macros where bound-identifier=?
;; equivalence has to be preserved.
;; Unlike SYNTAX and QUASISYNTAX, the embedded-syntax environment is not preserved
;; across compilation boundaries.

(define (expand-embedded-syntax re-expand form namespace)
  (syntax-trace 2 'expand-embedded-syntax form)
  (or (and (pair? (cdr form))
           (null? (cddr form)))
      (syntax-error "embedded-syntax:  Should have one argument"))
  (let ((r (make-primitive-renaming-procedure namespace))
        (key (gensym "embedded-syntax-key-")))
    (set-env/embedded-syntax-environment!
     namespace (alist-cons key (cadr form) (env/embedded-syntax-environment namespace)))
    (re-expand
     `(,(r 'embedded-syntax-lookup) (,(r 'quote) ,(r key)) (,(r 'expansion-environment))
       (,(r 'quote) ,(cadr form))))))

(define (embedded-syntax-lookup key namespace x)
  (cond ((assq key (env/embedded-syntax-environment namespace)) => cdr)
        (else
         (syntax-error "embedded-syntax: Invalid usage across compilation boundary:" x))))

;===========================================================================================

;;; LAMBDA:

(define (expand-lambda reexpand exp namespace)

  (define (dotted-map f lst)
    (cond ((pair? lst) (cons (f (car lst))
                             (dotted-map f (cdr lst))))
          ((null? lst) '())
          (else (f lst))))

  (define (dotted-for-each f lst)
    (cond ((pair? lst) (begin (f (car lst))
                              (dotted-for-each f (cdr lst))))
          ((null? lst) '())
          (else (f lst))))

  (syntax-trace 1 'expand-lambda exp namespace)

  (if (and (pair?    (cdr exp))
           (formals? (cadr exp))
           (list?    (cddr exp)))
      (let ((formals (cadr exp))
            (body    (cddr exp)))
        (dotted-for-each bind-lexical! formals)
        (scan-body body namespace
                   (lambda (definitions exp exps)
                     (let ((result
                            `(lambda ,(dotted-map binding-name formals)
                               ,@(map (lambda (def)
                                        `(define ,(binding-name (cadr def))
                                           ,(expand (caddr def) namespace)))
                                      definitions)
                               ,exp
                               ,@(map (lambda (expr)
                                        (expand expr namespace)) exps))
                            ))
                       (for-each unbind! (map cadr definitions))
                       (dotted-for-each unbind! formals)
                       result))))
      (syntax-error "Invalid lambda syntax:" (syntax-debug exp))))

;; Here we expand the first expression atomically in case expansion
;; relies on side effects.  This is important in a procedural macro
;; system.  So that the first expression will be expanded correctly,
;; definition-bound identifiers are bound as soon as they are
;; encountered.

(define scan-body
  (let ((define? (make-operator-predicate 'define))
        (begin?  (make-operator-predicate 'begin)))

    (lambda (forms namespace k)
      (syntax-trace 4 'scan-body forms namespace)

      (let loop ((forms forms)
                 (defs  '()))
        (cond ((pair? forms) (let ((form  (head-expand (car forms) namespace))
                                   (forms (cdr forms)))
                               (cond ((define? form)
                                      (let ((def (normalize-definition form namespace #f)))
                                        (bind-lexical! (cadr def))
                                        (loop forms
                                              (cons def defs))))
                                     ((begin? form)
                                      (loop (append (cdr form) forms)
                                            defs))
                                     (else
                                      (k (reverse defs) (expand form namespace) forms)))))
              ((null? forms) (syntax-error "Lambda: Empty body."))
              (else (syntax-error "Lambda: Improper list in body.")))))))

;======================================================================================

;;; DEFINE, DEFINE-SYNTAX and SET-SYNTAX!:
(define (normalize-definition form namespace syntax-definition?)
  (cond ((pair? (cdr form))
         (let ((_    (car form))
               (head (cadr form))
               (body (cddr form)))
           (cond ((and (identifier? head)
                       (pair? body)
                       (null? (cdr body)))
                  `(,_ ,head . ,body))
                 ((and (pair? head)
                       (identifier? (car head))
                       (formals? (cdr head)))
                  (let ((r (make-primitive-renaming-procedure namespace)))
                    (if syntax-definition?
                        `(,_ ,(car head)
                             ,(let ((transformer (r (symbolic-name (car head)))))
                                `((,(r 'lambda) (,transformer)
                                   (,(r 'lambda) (,(r 'form))
                                    (,(r 'apply) ,transformer ,(r 'form))))
                                  (,(r 'lambda) (,(r 'dummy) . ,(cdr head))
                                   . ,body))))
                        `(,_ ,(car head)
                             (,(r 'lambda) ,(cdr head)
                              . ,body)))))
                 (else (syntax-error "Syntax error in definition:" (syntax-debug form))))))
        (else (syntax-error "Syntax error in definition:" (syntax-debug form)))))

(define (expand-define-syntax re-expand form namespace)
  (syntax-trace 1 'expand-define-syntax form namespace)
  (let ((t (normalize-definition form namespace #t))
        (r (make-primitive-renaming-procedure namespace)))
    (bind-toplevel! (cadr t))
    `(env/extend-syntax! (expansion-environment) ',(binding-name (cadr t))
                         (lambda (re-expand form namespace)
                           (parameterize ((expansion-environment namespace))
                             (re-expand (,(expand (caddr t) namespace) form)))))))

(define (expand-define re-expand t namespace)
  (define (alist-delete key alist)
    (cond ((pair? alist) (if (eq? (caar alist) key)
                             (alist-delete key (cdr alist))
                             (cons (car alist)
                                   (alist-delete key (cdr alist)))))
          ((null? alist) '())
          (else (error "alist-delete: Improper list."))))

  (syntax-trace 1 'expand-define t namespace)
  (let ((t (normalize-definition t namespace #f)))
    (bind-toplevel! (cadr t))
    (set-env/syntax-environment!
     namespace
     (alist-delete (binding-name (cadr t))
                   (env/syntax-environment namespace)))
    `(define ,(binding-name (cadr t))
       ,(expand (caddr t) namespace))))

(define (expand-set-syntax re-expand form namespace)
  (syntax-trace 1 'expand-set-syntax form)
  (if (and (list? form)
           (= (length form) 3)
           (identifier? (cadr form)))
      `(set-syntax! (quote ,(binding-name (cadr form)))
                    (lambda (re-expand form namespace)
                      (parameterize ((expansion-environment namespace))
                        (re-expand (,(expand (caddr form) namespace) form)))))
      (syntax-error)))

(define (set-syntax! binding-name transformer)
  (cond ((assq binding-name (env/syntax-environment (expansion-environment)))
         => (lambda (entry)
              (set-cdr! entry transformer)))
        (else
         (error "Set-syntax! Unbound variable:" binding-name))))


;===========================================================================================

;;; LET-SYNTAX and LETREC-SYNTAX

(define (expand-let-syntax re-expand t namespace)
  (syntax-trace 1 'expand-let-syntax t)
  (scan-let t
    (lambda (formals exps body)
      (or (formals? formals)
          (syntax-error "Invalid formals:" (syntax-debug formals)))
      (let ((transformers (map (lambda (exp)
                                 (parameterize ((*inside-quasisyntax* #f))
                                   (eval-for-expander (expand exp namespace))))
                               exps)))
        (for-each bind-lexical! formals)
        (emit-lexical-syntax namespace formals transformers body)))))

(define (expand-letrec-syntax re-expand t namespace)
  (syntax-trace 1 'expand-letrec-syntax t)
  (scan-let t
    (lambda (formals exps body)
      (or (formals? formals)
          (syntax-error "Invalid formals:" (syntax-debug formals)))
      (for-each bind-lexical! formals)
      (let ((transformers (map (lambda (exp)
                                 (parameterize ((*inside-quasisyntax* #f))
                                   (eval-for-expander (expand exp namespace))))
                               exps)))
        (emit-lexical-syntax namespace formals transformers body)))))

;; LET-SYNTAX is implemented as a splicing form, wrapping its
;; body in an implicit BEGIN instead of LET as in R5RS.
;; This is a more useful semantics, allowing the body to expand
;; into toplevel definitions, and the R5RS semantics can be
;; obtained by explicitly writing the LET.

(define (wrap-lexical-syntax transformer)
  (define (lexical-syntax-wrapper re-expand form namespace)
    (parameterize ((expansion-environment namespace))
      (re-expand (transformer form))))
  lexical-syntax-wrapper)

(define (emit-lexical-syntax namespace formals transformers body)
  (let ((parent-syntax-environment
         (env/syntax-environment namespace)))

    (set-env/syntax-environment! namespace
                                 (append (map (lambda (formal transformer)
                                                (cons (binding-name formal)
                                                      (wrap-lexical-syntax transformer)))
                                              formals
                                              transformers)
                                         (env/syntax-environment namespace)))

    (let ((result (map (lambda (body-form)
                         (expand body-form namespace)) body)))
      (for-each unbind! formals)
      (set-env/syntax-environment! namespace parent-syntax-environment)
      `(begin ,@result))))

;;;================================================================================
;;;
;;; MODULES:
;;;
;;; An implementation of modules with the following properties:
;;;
;;;  * Modules expand to core R5RS Scheme.
;;;
;;;  * Repeatability and
;;;    incremental compilation : The result of expanding or compiling a module does
;;;                              not depend on whether it has been separately or
;;;                              jointly compiled or expanded in a REPL, and is
;;;                              independent of any global variables or the dynamical
;;;                              state of the host Scheme.
;;;                              There is a well-defined protocol for
;;;                              initializing module bindings when imported during
;;;                              expansion time and again when loading the expanded
;;;                              module, ensuring repeatability and consistency with
;;;                              incremental compilation by protecting against
;;;                              inadvertent contamination of the runtime environment
;;;                              by compile-time values during development.
;;;
;;;  * REPL-like semantics   :   Modules have a REPL-like default semantics, with each form
;;;                              expanded in an environment obtained by evaluating
;;;                              previous forms in the module.
;;;                              This means that macros and their helper functions may
;;;                              be in the same module, and that the language for
;;;                              procedural macros is incrementally extended in
;;;                              a module (macro code may rely on previous macros
;;;                              in the same module for their expansion).
;;;                              Thus separating the issue of language tower control from the
;;;                              code organization mechanism simplifies the organization of
;;;                              related code.
;;;
;;;  * Simple phase control :    Normally all expressions are evaluated both at expand-time
;;;                              and import-time.  This can be customized via begin-for-syntax
;;;                              and begin-for-import.  The first evaluates enclosed expressions
;;;                              only at expand-time.  The second only at import-time.
;;;
;;;  * Convenient features  :    Arbitrary computations may be performed on imported
;;;                              symbols.
;;;                              Modules may be nested.
;;;                              A macro may expand to a module, allowing a simple
;;;                              form of parametrized modules.
;;;
;;;


;; To support incremental compilation, substitution environments are
;; included in the representation of a module.

(define (make-module name syntax-exports runtime-exports)
  (list name syntax-exports runtime-exports))

(define module/name    car)
(define module/syntax-exports cadr)
(define module/runtime-exports caddr)

;; Before a module is expanded, its entire body is painted with a fresh
;; color in an empty substitution environment, erasing previous colors
;; completely and making all identifiers disjoint from all previously
;; introduced identifiers.

(define (expand-module-syntax re-expand exp namespace)

  (define (defines->sets forms)
    (map (lambda (form)
           (if (and (pair? form)
                    (eq? (car form) 'define))
               `(set! ,(cadr form) ,(caddr form))
               form))
         forms))

  (define (extract-defines forms)
    (filter (lambda (form)
              (and (pair? form)
                   (eq? (car form) 'define)))
            forms))

  (define (flatten-begins forms)
    (let loop ((result '())
               (forms forms))
      (cond ((pair? forms) (let ((form  (car forms))
                                 (forms (cdr forms)))
                             (if (and (pair? form)
                                      (eq? (car form) 'begin))
                                 (loop result
                                       (append (cdr form) forms))
                                 (loop (cons form result)
                                       forms))))
            ((null? forms) (reverse result))
            (else (error "flatten-begins: improper list")))))

  (let ((r (make-primitive-renaming-procedure namespace)))

    (cond ((and (pair? (cdr exp))
                (pair? (cddr exp))
                (list? (caddr exp)))

           ;; The default initial language is SCHEME.

           (expand-module-syntax re-expand `(,(car exp) ,(cadr exp) ,(r 'scheme)
                                             . ,(cddr exp)) namespace))

          ((and (pair?       (cdr exp))
                (identifier? (cadr exp))
                (pair?       (cddr exp))
                (identifier? (caddr exp))
                (list?       (cadddr exp))
                (formals?    (cadddr exp))
                (list?       (cddddr exp)))

           ;; Since modules can be nested, we save the parent
           ;; environments, to be restored afterwards.

           ;; To ensure repeatability and consistency with incremental compilation,
           ;; modules are expanded in a fresh environment.
           (let ((module-namespace (make-namespace (symbolic-name (cadr exp))
                                                   (env/syntax-environment namespace))))

             (parameterize ((expansion-environment module-namespace))
               (let* ((name           (cadr exp))
                      (initial-import (caddr exp))

                      ;; Paint the entire body with a new color,
                      ;; achieving a private namespace.

                      (rename  (make-renaming-procedure
                                module-namespace
                                (generate-color)
                                (env/reflect! module-namespace '())
                                #f))
                      (body    (datum->syntax-object0 rename (syntax-object->datum (cdddr exp))))
                      (exports (car body))
                      (exps    (cdr body))

                      ;; Modules have a REPL-like semantics, expanding
                      ;; each form in the environment obtained by evaluating
                      ;; previous form in the module.

                      (expanded-initial
                       (let ((expanded
                              (expand-import (lambda (new-form)
                                               (expand new-form module-namespace))
                                             `(,(rename 'import)
                                               ,initial-import)
                                             module-namespace)))
                         (eval-for-expander expanded)
                         expanded))

                      (expanded-body
                       (flatten-begins
                        (map-in-order (lambda (exp)
                                        (let ((expanded (expand exp module-namespace)))
                                          (cond ((begin-for-syntax? expanded)
                                                 (eval-for-expander `(begin . ,(cdr expanded)))
                                                 '(void))
                                                ((begin-for-import? expanded)
                                                 `(begin . ,(cdr expanded)))
                                                (else (eval-for-expander expanded)
                                                      expanded))))
                                      exps)))
                      (module-entry
                       (make-module (symbolic-name name)
                                    '()
                                    (map (lambda (export-id)
                                           (cons (symbolic-name export-id)
                                                 (binding-name  export-id)))
                                         exports)))
                      (expanded-module
                       `(begin ,@(map (lambda (def)
                                        `(define ,(cadr def) #f))
                                      (extract-defines expanded-body))
                               (define ,(symbolic-name name)
                                 (lambda ()
                                   (set-env/module-environment!
                                    (expansion-environment)
                                    (alist-cons (quote ,(symbolic-name name))
                                                (quote ,module-entry)
                                                (env/module-environment (expansion-environment))))

                                   (env/extend-reflected!
                                    (expansion-environment)
                                    (quote ,(compress-envs
                                             (env/reflected-environments
                                              module-namespace))))


                                   ,expanded-initial
                                   ,@(defines->sets expanded-body))))))

                 expanded-module))))

          (else (syntax-error "Invalid module syntax:" (syntax-debug exp))))))


(define (begin-for-import? x)
  (and (pair? x)
       (eq? (car x) 'begin-for-import)))

(define (begin-for-syntax? x)
  (and (pair? x)
       (eq? (car x) 'begin-for-syntax)))

;; Second parameter, if present, is a procedure :: symbol -> (union symbol #f)
;; representing an arbitrary computation on the imported symbols.
;; If #f, the symbol is not imported.

;; needs to re-expand
(define (expand-import re-expand form namespace)
  (syntax-trace 1 'expand-import form)
  (if (and (pair? (cdr form))
           (identifier? (cadr form)))
      (let ((_    (car form))
            (name (symbolic-name (cadr form)))
            (comp (if (pair? (cddr form))
                      (eval-for-expander (expand (caddr form) namespace))
                      (lambda (symbol) symbol))))

        (load-module name)

        (let* ((module-entry (alist-ref name (env/module-environment namespace)))
               (exports      (module/runtime-exports module-entry)))

          (for-each (lambda (export)
                      (let ((import-name (comp (car export))))
                        (if import-name
                            (import! import-name
                                     (cdr export)
                                     _))))
                    exports)
          (let ((rename (make-primitive-renaming-procedure namespace)))
            (re-expand `(,(rename 'load-module) (,(rename 'quote) ,(rename name)))))))
      (syntax-error "Illegal import syntax:" (syntax-debug form))))

(define (load-module name)
  (if (not (alist-ref name (env/module-environment (expansion-environment))))
      (eval-for-expander `(,name))))

;===========================================================================

;;; Standard environments:

(define empty-env #f)
(define scheme-env #f)
(define scheme #f)
(define source-rename #f)

(define (initialize-kernel-namespace kernel-namespace)
  (let ((scheme-tokens `(;; R5RS Scheme minus macros and literals:

                         *
                         +
                         -
                         ;; ...
                         /
                         <
                         <=
                         =
                         ;; =>
                         >
                         >=
                         abs
                         acos
                         and
                         append
                         apply
                         asin
                         assoc
                         assq
                         assv
                         atan
                         begin
                         boolean?
                         caar
                         cadr
                         call-with-current-continuation
                         call-with-input-file
                         call-with-output-file
                         call-with-values
                         call/cc
                         case
                         car
                         cdr
                         caar
                         cadr
                         cdar
                         cddr
                         caaar
                         caadr
                         cadar
                         caddr
                         cdaar
                         cdadr
                         cddar
                         cdddr
                         caaaar
                         caaadr
                         caadar
                         caaddr
                         cadaar
                         cadadr
                         caddar
                         cadddr
                         cdaaar
                         cdaadr
                         cdadar
                         cdaddr
                         cddaar
                         cddadr
                         cdddar
                         cddddr
                         ceiling
                         char->integer
                         char-alphabetic?
                         char-ci<=?
                         char-ci<?
                         char-ci=?
                         char-ci>=?
                         char-ci>?
                         char-downcase
                         char-lower-case?
                         char-numeric?
                         char-ready?
                         char-upcase
                         char-upper-case?
                         char-whitespace?
                         char<=?
                         char<?
                         char=?
                         char>=?
                         char>?
                         char?
                         close-input-port
                         close-output-port
                         complex?
                         cond
                         cons
                         cos
                         current-input-port
                         current-output-port
                         define
                         ;; define-syntax
                         delay
                         denominator
                         display
                         do
                         dynamic-wind
                         ;; else
                         eof-object?
                         eq?
                         equal?
                         eqv?
                         error
                         eval
                         even?
                         exact->inexact
                         exact?
                         exp
                         expt
                         floor
                         for-each
                         force
                         gcd
                         if
                         imag-part
                         inexact->exact
                         inexact?
                         input-port?
                         integer->char
                         integer?
                         interaction-environment
                         lambda
                         lcm
                         length
                         let
                         let*
                         ;; let-syntax
                         letrec
                         ;; letrec-syntax
                         list
                         list->string
                         list->vector
                         list-ref
                         list-tail
                         list?
                         load
                         location
                         log
                         magnitude
                         make-polar
                         make-rectangular
                         make-string
                         make-vector
                         map
                         max
                         member
                         memq
                         memv
                         min
                         modulo
                         negative?
                         newline
                         not
                         null-environment
                         null?
                         number->string
                         number?
                         numerator
                         odd?
                         open-input-file
                         open-output-file
                         or
                         output-port?
                         pair?
                         peek-char
                         port?
                         positive?
                         procedure?
                         quasiquote
                         quote
                         quotient
                         rational?
                         rationalize
                         read
                         read-char
                         real-part
                         real?
                         remainder
                         reverse
                         round
                         scheme-report-environment
                         set!
                         set-car!
                         set-cdr!
                         sin
                         sqrt
                         string
                         string->list
                         string->number
                         string->symbol
                         string-append
                         string-ci<=?
                         string-ci<?
                         string-ci=?
                         string-ci>=?
                         string-ci>?
                         string-copy
                         string-fill!
                         string-length
                         string-ref
                         string-set!
                         string<=?
                         string<?
                         string=?
                         string>=?
                         string>?
                         string?
                         substring
                         symbol->string
                         symbol?
                         ;; syntax-rules
                         tan
                         transcript-off
                         transcript-on
                         truncate
                         unbound
                         ;; unquote
                         ;; unquote-splicing
                         values
                         vector
                         vector->list
                         vector-fill!
                         vector-length
                         vector-ref
                         vector-set!
                         vector?
                         with-input-from-file
                         with-output-to-file
                         write
                         write-char
                         zero?

                         ;; Additions for macros and modules:

                         define-syntax
                         let-syntax
                         letrec-syntax
                         set-syntax!

                         syntax
                         quasisyntax
                         embedded-syntax
                         identifier?
                         bound-identifier=?
                         free-identifier=?
                         literal-identifier=?

                         make-fluid-identifier
                         datum->syntax-object
                         syntax-object->datum

                         load-module
                         module
                         import
                         scheme
                         ;; *default-initial-language*
                         begin-for-syntax
                         begin-for-import

                         expand
                         syntax-debug
                         syntax-error

                         flush-output-port
                         expansion-environment
                         exit

                         env/extend-syntax!
                         make-meta-renaming-procedure
                         syntax-trace

                         uninstall!
                         parameterize
                         )))


    (set-env/reflected-environments! kernel-namespace '())
    (set-env/module-environment! kernel-namespace '())
    (set-env/syntax-environment! kernel-namespace '())
    (set-env/embedded-syntax-environment! kernel-namespace '())

    (env/extend-syntax! kernel-namespace 'define                    expand-define)
    (env/extend-syntax! kernel-namespace 'define-syntax             expand-define-syntax)
    (env/extend-syntax! kernel-namespace 'import                    expand-import)
    (env/extend-syntax! kernel-namespace 'lambda                    expand-lambda)
    (env/extend-syntax! kernel-namespace 'let-syntax                expand-let-syntax)
    (env/extend-syntax! kernel-namespace 'letrec-syntax             expand-letrec-syntax)
    (env/extend-syntax! kernel-namespace 'module                    expand-module-syntax)
    (env/extend-syntax! kernel-namespace 'quasiquote                expand-quasiquote)
    (env/extend-syntax! kernel-namespace 'quasisyntax               expand-quasisyntax)
    (env/extend-syntax! kernel-namespace 'quote                     expand-quote)
    (env/extend-syntax! kernel-namespace 'set-syntax!               expand-set-syntax)
    (env/extend-syntax! kernel-namespace 'syntax                    expand-syntax)
    (env/extend-syntax! kernel-namespace 'syntax0                   expand-syntax0)
    (env/extend-syntax! kernel-namespace 'embedded-syntax           expand-embedded-syntax)
    (env/extend-syntax! kernel-namespace 'with-fresh-renaming-scope expand-fresh-scope)

    (set! empty-env (env/reflect! kernel-namespace '()))
    (set! scheme-env  (env/reflect! kernel-namespace
                                    (map (lambda (name) (cons name name))
                                         scheme-tokens)))
    (let ((scheme-entry (make-module 'scheme
                                     '()
                                     (map (lambda (token) (cons token token))
                                          scheme-tokens))))
      (set-env/module-environment!
       kernel-namespace
       (alist-cons 'scheme scheme-entry '()))

      (set! scheme
            (lambda ()
              (set-env/module-environment! (expansion-environment)
                                           (alist-cons 'scheme scheme-entry
                                                       (env/module-environment (expansion-environment))))))
      (set! source-rename
            (make-renaming-procedure kernel-namespace *source-color* scheme-env #f))

      (let ((rename (make-renaming-procedure kernel-namespace *no-color* empty-env #f)))
        (for-each
         (lambda (exp)
           (let ((expanded (expand
                            (datum->syntax-object0 rename exp)
                            kernel-namespace)))
             (syntax-trace 1 'primitive-repl "Expanded: " expanded)
             (eval-for-expander expanded)))
         '(

           (define-syntax let
             (lambda (t)
               (if (and (pair? t)
                        (pair? (cdr t))
                        (identifier? (cadr t)))
                   (scan-let (cons (car t) (cddr t))
                     (lambda (formals exps body)
                       (quasisyntax
                        ((letrec ((,(cadr t) (lambda ,formals ,@body))) ,(cadr t))
                         ,@exps))))
                   (scan-let t
                     (lambda (formals exps body)
                       (quasisyntax
                        ((lambda ,formals ,@body) ,@exps)))))))


           (define-syntax letrec
             (lambda (t)
               (scan-let t
                 (lambda (formals exps body)
                   (let ((definitions (map (lambda (formal exp)
                                             (quasisyntax (define ,formal ,exp)))
                                           formals
                                           exps)))
                     (quasisyntax ((lambda () ,@definitions ,@body))))))))


           (define-syntax let*
             (lambda (t)
               (scan-let t
                 (lambda (formals exps body)
                   (let ((bindings (cadr t)))
                     (if (or (null? bindings)
                             (null? (cdr bindings)))
                         (quasisyntax (let ,bindings ,@body))
                         (quasisyntax (let (,(car bindings))
                                        (let* ,(cdr bindings) ,@body)))))))))


           (define-syntax (cond . clauses)
             (if (null? clauses)
                 (syntax-error  "Cond: Must have at least one clause"))
             (car
              (let f ((clauses clauses))
                (if (null? clauses)
                    '()
                    (list
                     (if (pair? clauses)
                         (let ((clause (car clauses))
                               (rest   (f (cdr clauses))))
                           (if (or (null? clause)
                                   (not (list? clause)))
                               (syntax-error "Cond: Invalid clause"
                                             (syntax-debug clause)))
                           (if (and (literal-identifier=? (car clause) (syntax else))
                                    (null? rest))
                               (quasisyntax (begin ,@(cdr clause)))
                               (if (null? (cdr clause))
                                   (quasisyntax (let ((t ,(car clause)))
                                                  (if t t ,@rest)))
                                   (if (and (literal-identifier=? (cadr clause) (syntax =>))
                                            (pair? (cddr clause))
                                            (null? (cdddr clause)))
                                       (quasisyntax (let ((t ,(car clause)))
                                                      (if t (,(caddr clause) t) ,@rest)))
                                       (quasisyntax (if ,(car clause)
                                                        (begin ,@(cdr clause))
                                                        ,@rest))))))
                         (syntax-error)))))))


           (define-syntax (case . rest)
             (or (pair? rest)
                 (syntax-error))
             (let ((key (car rest))
                   (temp (syntax temp))
                   (clauses (cdr rest)))
               (or (list? clauses)
                   (syntax-error))
               (quasisyntax
                (let ((,temp ,key))
                  (cond
                   ,@(map (lambda (clause)
                            (or (pair? clause)
                                (syntax-error "Case: Invalid clause:"
                                              (syntax-debug clause)))
                            (quasisyntax
                             (,(cond ((literal-identifier=? (car clause) (syntax else))
                                      (car clause))
                                     ((list? (car clause))
                                      (quasisyntax (memv ,temp ',(car clause))))
                                     (else
                                      (syntax-error "Case: Invalid literals list:"
                                                    (syntax-debug (car clause)))))
                              ,@(cdr clause))))
                          clauses))))))


           (define-syntax do
             (lambda (exp)
               (or (and (pair? (cdr exp))
                        (pair? (cddr exp)))
                   (syntax-error))
               (let ((specs (cadr exp))
                     (end (caddr exp))
                     (body (cdddr exp))
                     (loop (syntax loop)))
                 (or (and (list? specs)
                          (every? do-spec? specs)
                          (list? end))
                     (syntax-error))
                 (quasisyntax
                  (letrec ((,loop
                            (lambda ,(map car specs)
                              (cond ,end
                                    (else ,@body
                                          (,loop
                                           ,@(map (lambda (spec)
                                                    (if (null? (cddr spec))
                                                        (car spec)
                                                        (caddr spec)))
                                                  specs)))))))
                    (,loop ,@(map cadr specs)))))))


           (define (do-spec? s)
             (and (pair? s)
                  (identifier? (car s))
                  (pair? (cdr s))
                  (let ((rest (cddr s)))
                    (or (null? rest)
                        (and (pair? rest)
                             (null? (cdr rest)))))))

           ))))))


;============================================================================

;;; REPL:

;; Each toplevel expression is read into a newly generated context,
;; taking its initial substitution environment from the previous.
;; We could have reused the same context, but since the hygiene
;; algorithm destructively updates contexts, we would have had
;; to guard against inconsistent states in case an error occurred
;; during expansion (relevant in a typical toplevel debugging cycle where
;; the system is not necessarily reinitialized after each error).
;; As a result, identifiers with the same symbolic name in
;; separate toplevel expressions will not be bound-identifier=?
;; This is not a serious restriction.

;(define source-rename (make-renaming-procedure *kernel-namespace* *source-color* scheme-env #f))

;; This procedure should be integrated with the host REPL and
;; compiler.  It takes a source-level s-expression and expands it to
;; a core Scheme expression that can be fed to EVAL.

(define (expand-toplevel exp toplevel-namespace)
  (syntax-trace 0 'expand-toplevel exp toplevel-namespace)
  (*source-stack* '())
  (set! source-rename
        (make-renaming-procedure toplevel-namespace *source-color*
                                 (close-environment toplevel-namespace (source-rename 'dummy))
                                 #f))
  (original-macro-expander
   (expand (datum->syntax-object0 source-rename exp) toplevel-namespace)
   toplevel-namespace))

;===================================================================================

;;; The usual macros:

;; Here we need to redefine all binding forms of the host Scheme.
;; We also need to redefine all forms that treat parts of their body
;; as literals.



