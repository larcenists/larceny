;;; -*-Mode: Scheme; coding: iso-8859-1 -*-

($$trace "envaux")

;;; Extensions to top-level environment structures.

;;; Larceny environments need extra structure in order to support
;;; syntax-case and modules.  The standard Larceny environment
;;; structure has an auxiliary-info field for this purpose.  This code
;;; creates the auxiliary-info data structure and places in that slot.

;;; These fields are designed to hold the auxiliary info for the
;;; simple-macros package by Andre van Tonder.

(define env/embedded-syntax-environment      (generic-getter 'env/embedded-syntax-environment))
(define set-env/embedded-syntax-environment! (generic-setter 'set-env/embedded-syntax-environment!))
(define env/module-environment               (generic-getter 'env/module-environment))
(define set-env/module-environment!          (generic-setter 'set-env/module-environment!))
(define env/reflected-environments           (generic-getter 'env/reflected-environments))
(define set-env/reflected-environments!      (generic-setter 'set-env/reflected-environments!))
(define env/syntax-environment               (generic-getter 'env/syntax-environment))
(define set-env/syntax-environment!          (generic-setter 'set-env/synatx-environment!))

(define <environment-auxinfo>
  (make-class '<environment-auxinfo>
    :direct-slots
    `((reflected-environments :initvalue ()
                              :type ,<list>)

      ;; Maps binding names of modules to compiled modules.
      ;; Each imported module is guaranteed to be loaded exactly
      ;; once during expansion of an importing module, even if it
      ;; is being imported via several different paths.
      (module-environment :initvalue ()
                          :type ,<list>)

      (syntax-environment :initarg :syntax-environment
                          :initvalue ()
                          :type ,<list>)

      (embedded-syntax-environment :initvalue ()
                                   :type ,<list>)
      )))

;;; Instead of the usual slot accessor initialization, we install
;;; custom accessors that indirect through the auxiliary-info field.
;;; All other code uses these accessors, so the indirection is hidden.

(for-each
 (lambda (info)
   (let ((slot (car info))
         (reader (cadr info))
         (writer (caddr info)))
     (extend-generic reader
       :specializers (list <namespace>)
       :procedure (let ((slot-reader (getter-method <environment-auxinfo> slot)))
                    (define (method:env/slot call-next-method env)
                      (slot-reader (environment-auxiliary-info env)))
                    method:env/slot))
     (extend-generic writer
       :specializers (list <namespace>)
       :procedure (let ((slot-writer (setter-method <environment-auxinfo> slot)))
                    (define (method:env/set-slot! call-next-method env new-value)
                      (slot-writer (environment-auxiliary-info env) new-value))
                    method:env/set-slot!))))
 `((reflected-environments      ,env/reflected-environments      ,set-env/reflected-environments!)
   (module-environment          ,env/module-environment          ,set-env/module-environment!)
   (syntax-environment          ,env/syntax-environment          ,set-env/syntax-environment!)
   (embedded-syntax-environment ,env/embedded-syntax-environment ,set-env/embedded-syntax-environment!)
   ))

(define (make-namespace name initial-syntax)
  (let ((env (make-environment name)))
    (environment-set-auxiliary-info! env (make <environment-auxinfo>
                                           :syntax-environment initial-syntax))
    env))

(define (env/reflect! env r)
  ;; Reflect r and save it in env.
  (let ((key (gensym "environment-key-")))
    (set-env/reflected-environments!
     env
     (alist-cons
      key (cons (environment-name env) r)
      (env/reflected-environments env)))
    key))

(define (env/extend-reflected! env compressed-extension)
  (set-env/reflected-environments!
   env
   (append (uncompress-envs compressed-extension)
           (env/reflected-environments env))))

(define (env/reify env key)
  (cdr (alist-ref key (env/reflected-environments env))))

;==========================================================================

;;; Infrastructure for syntax definitions:

;; This could benefit from being implemented as an O(1) data structure.

(define (env/extend-syntax! env name procedure)
  (set-env/syntax-environment!
   env (alist-cons name procedure (env/syntax-environment env))))

(define (env/lookup-syntax env identifier)
  (alist-ref (binding-name identifier)
             (env/syntax-environment env)))

;;; This parameter holds the environment in which expressions are
;;; evaluated during macro expansion.
(define expansion-environment
  (make-parameter "expansion-environment"
                  (make-namespace 'dummy '())
                  (class-predicate <namespace>)))

(define (current-module-name)
  (environment-name (expansion-environment)))
