; Copyright 1992 William Clinger
;
; $Id$
;
; 9 December 1998
; Syntactic environments.
;
; A syntactic environment maps identifiers to denotations,
; where a denotation is one of
;
;    (special <special>)
;    (macro <rules> <env>)
;    (inline <rules> <env>)
;    (identifier <id> <references> <assignments> <calls>)
;
; and where <special> is one of
;
;    quote
;    lambda
;    if
;    set!
;    begin
;    define
;    define-syntax
;    let-syntax
;    letrec-syntax
;    syntax-rules
;
; and where <rules> is a compiled <transformer spec> (see R4RS),
; <env> is a syntactic environment, and <id> is an identifier.
;
; An inline denotation is like a macro denotation, except that it
; is not an error when none of the rules match the use.  Inline
; denotations are created by DEFINE-INLINE.
; The standard syntactic environment should not include any
; identifier denotations; space leaks will result if it does.

($$trace "syntaxenv")

(define standard-syntactic-environment
  `((quote         . (special quote))
    (lambda        . (special lambda))
    (if            . (special if))
    (set!          . (special set!))
    (begin         . (special begin))
    (define        . (special define))
    (define-inline . (special define-inline))
    (define-syntax . (special define-syntax))
    (let-syntax    . (special let-syntax))
    (letrec-syntax . (special letrec-syntax))
    (syntax-rules  . (special syntax-rules))

    ;; MzScheme specific
    (#%datum       . (special #%datum))
    (#%top         . (special #%top))
    (#%app         . (special #%app))
    ))

; Unforgeable synonyms for lambda and set!, used to expand definitions.

(define lambda0 (string->symbol " lambda "))
(define set!0 (string->symbol " set! "))

(define (syntactic-copy env)
  (copy-alist env))

(define (make-basic-syntactic-environment)
  (cons (cons lambda0
              (cdr (assq 'lambda standard-syntactic-environment)))
        (cons (cons set!0
                    (cdr (assq 'set! standard-syntactic-environment)))
              (syntactic-copy standard-syntactic-environment))))

(define (make-minimal-syntactic-environment)
  (list (cons lambda0
              (cdr (assq 'lambda standard-syntactic-environment)))
        (cons set!0
              (cdr (assq 'set! standard-syntactic-environment)))))

; Most macros are stored here.

(define usual-syntactic-environment
  (make-basic-syntactic-environment))

(define (the-usual-syntactic-environment . rest)
  (if (and (not (null? rest))
           (eq? (car rest) 'copy))
      (syntactic-copy usual-syntactic-environment)
      usual-syntactic-environment))

; Support for Larceny.  Macros are packaged up as procedures chiefly
; because they are circular structures.

(define (syntactic-environment-names syntaxenv)
  (let loop ((e syntaxenv) (n '()))
    (if (null? e)
        n
        (let ((name (caar e)))
          (cond ((eq? name lambda0) (loop (cdr e) n))
                ((eq? name set!0) (loop (cdr e) n))
                (else (loop (cdr e) (cons name n))))))))

(define (syntactic-environment-get syntaxenv id)
  (let ((x (syntactic-lookup syntaxenv id)))
    (if (identifier-denotation? x)
        #f
        (lambda () x))))

(define (syntactic-environment-set! syntaxenv id macro)
  (parameterize ((global-syntactic-environment syntaxenv))
    (syntactic-bind-globally! id (macro))))

(define (syntactic-environment-remove! syntaxenv id)
  (parameterize ((global-syntactic-environment syntaxenv))
    (syntactic-bind-globally! id (make-identifier-denotation id))))

(define (usual-syntax id)
  (or (syntactic-environment-get usual-syntactic-environment id)
      (error "usual-syntax: unknown macro: " id)))

; Whatever is stored in the global-syntactic-environment will always be
; a nonempty association list since there is no way to remove the entry
; for lambda0.  That entry is used as a header by destructive
; operations.

(define global-syntactic-environment
  (make-parameter "global-syntactic-environment" #f))

(define global-inline-environment
  (make-parameter "global-inline-environment" '()))

(define (global-syntactic-environment-set! env)
  (set-cdr! (global-syntactic-environment) env)
  #t)

(define (syntactic-bind-globally! id denotation)
  (if (and (identifier-denotation? denotation)
           (eq? id (identifier-name denotation)))
      (letrec ((remove-bindings-for-id
                (lambda (bindings)
                  (cond ((null? bindings) '())
                        ((eq? (caar bindings) id)
                         (remove-bindings-for-id (cdr bindings)))
                        (else (cons (car bindings)
                                    (remove-bindings-for-id (cdr bindings))))))))
        (global-syntactic-environment-set!
         (remove-bindings-for-id (cdr (global-syntactic-environment)))))
      (let ((x (assq id (global-syntactic-environment))))
        (if x
            (begin (set-cdr! x denotation) #t)
            (global-syntactic-environment-set!
             (cons (cons id denotation)
                   (cdr (global-syntactic-environment))))))))

(define (syntactic-divert env1 env2)
  (append env2 env1))

(define (syntactic-extend env ids denotations)
  (syntactic-divert env (map cons ids denotations)))

(define (syntactic-lookup env id)
  (let ((entry (assq id env)))
    (if entry
        (cdr entry)
        (let ((inline-entry (assq id (global-inline-environment))))
          (cond (inline-entry (cdr inline-entry))
                ((and (recognize-javadot-symbols?)
                      (javadot-symbol? id))
                 (make-javadot-denotation id))
                (else (make-identifier-denotation id)))))))

(define (syntactic-assign! env id denotation)
  (let ((entry (assq id env)))
    (if entry
        (set-cdr! entry denotation)
        (m-bug "Bug detected in syntactic-assign!" env id denotation))))

; Denotations.

(define denotation-class car)

(define (special-denotation? denotation)
  (eq? (denotation-class denotation) 'special))

(define (macro-denotation? denotation)
  (eq? (denotation-class denotation) 'macro))

(define (inline-denotation? denotation)
  (eq? (denotation-class denotation) 'inline))

(define (identifier-denotation? denotation)
  (eq? (denotation-class denotation) 'identifier))

(define (javadot-denotation? denotation)
  (eq? (denotation-class denotation) 'javadot))

(define (make-macro-denotation rules env)
  (list 'macro rules env))

(define (make-inline-denotation id rules env)
  (list 'inline rules env id))

(define (make-identifier-denotation id)
  (list 'identifier id '() '() '()))

(define (make-javadot-denotation id)
  (list 'javadot id '() '() '()))

(define macro-rules        cadr)
(define macro-env          caddr)

(define inline-rules       macro-rules)
(define inline-env         macro-env)
(define inline-name        cadddr)

(define identifier-name    cadr)
(define identifier-R-entry cdr)

(define javadot-name       cadr)

(define (same-denotation? d1 d2)
  (or (eq? d1 d2)
      (and (identifier-denotation? d1)
           (identifier-denotation? d2)
           (eq? (identifier-name d1)
                (identifier-name d2)))
      (and (javadot-denotation? d1)
           (javadot-denotation d2)
           (eq? (javadot-name d1)
                (javadot-name d2)))))

(define denotation-of-quote
  (syntactic-lookup standard-syntactic-environment 'quote))

(define denotation-of-lambda
  (syntactic-lookup standard-syntactic-environment 'lambda))

(define denotation-of-if
  (syntactic-lookup standard-syntactic-environment 'if))

(define denotation-of-set!
  (syntactic-lookup standard-syntactic-environment 'set!))

(define denotation-of-begin
  (syntactic-lookup standard-syntactic-environment 'begin))

(define denotation-of-define
  (syntactic-lookup standard-syntactic-environment 'define))

(define denotation-of-define-inline
  (syntactic-lookup standard-syntactic-environment 'define-inline))

(define denotation-of-define-syntax
  (syntactic-lookup standard-syntactic-environment 'define-syntax))

(define denotation-of-let-syntax
  (syntactic-lookup standard-syntactic-environment 'let-syntax))

(define denotation-of-letrec-syntax
  (syntactic-lookup standard-syntactic-environment 'letrec-syntax))

(define denotation-of-syntax-rules
  (syntactic-lookup standard-syntactic-environment 'syntax-rules))

(define denotation-of-...
  (syntactic-lookup standard-syntactic-environment '...))

(define denotation-of-transformer
  (syntactic-lookup standard-syntactic-environment 'transformer))

(define denotation-of-app
  (syntactic-lookup standard-syntactic-environment '#%app))

(define denotation-of-datum
  (syntactic-lookup standard-syntactic-environment '#%datum))

(define denotation-of-top
  (syntactic-lookup standard-syntactic-environment '#%top))

; Given a syntactic environment env to be extended, an alist returned
; by rename-vars, and a syntactic environment env2, extends env by
; binding the fresh identifiers to the denotations of the original
; identifiers in env2.

(define (syntactic-alias env alist env2)
  (syntactic-divert
   env
   (map (lambda (name-pair)
          (let ((old-name (car name-pair))
                (new-name (cdr name-pair)))
            (cons new-name
                  (syntactic-lookup env2 old-name))))
        alist)))

; Given a syntactic environment and an alist returned by rename-vars,
; extends the environment by binding the old identifiers to the fresh
; identifiers.
; For Twobit, it also binds the fresh identifiers to their denotations.
; This is ok so long as the fresh identifiers are not legal Scheme
; identifiers.

(define (syntactic-rename env alist)
  (if (null? alist)
      env
      (let* ((old (caar alist))
             (new (cdar alist))
             (denotation (make-identifier-denotation new)))
        (syntactic-rename
         (cons (cons old denotation)
               (cons (cons new denotation)
                     env))
         (cdr alist)))))

; Renaming of variables.

(define renaming-counter 0)

(define (make-rename-procedure)
  (set! renaming-counter (+ renaming-counter 1))
  (let ((suffix (string-append renaming-suffix (number->string renaming-counter))))
    (lambda (sym)
      (if (symbol? sym)
          (let ((s (symbol->string sym)))
            (if (and (positive? (string-length s))
                     (char=? (string-ref s 0) renaming-prefix-character))
                (string->symbol (string-append s suffix))
                (string->symbol (string-append renaming-prefix s suffix))))
          (m-warn "Illegal use of rename procedure" 'ok:FIXME sym)))))

; Given a datum, strips the suffixes from any symbols that appear within
; the datum, trying not to copy any more of the datum than necessary.

(define (m-strip x)
  (define (original-symbol x)
    (define (loop sym s i n)
      (cond ((= i n) sym)
            ((char=? (string-ref s i)
                     renaming-suffix-character)
             (string->symbol (substring s 1 i)))
            (else
             (loop sym s (+ i 1) n))))
    (let ((s (symbol->string x)))
      (if (and (positive? (string-length s))
               (char=? (string-ref s 0) renaming-prefix-character))
          (loop x s 0 (string-length s))
          x)))
  (cond ((symbol? x)
         (original-symbol x))
        ((pair? x)
         (let ((a (m-strip (car x)))
               (b (m-strip (cdr x))))
           (if (and (eq? a (car x))
                    (eq? b (cdr x)))
               x
               (cons a b))))
        ((vector? x)
         (let* ((v (vector->list x))
                (v2 (map m-strip v)))
           (if (equal? v v2)
               x
               (list->vector v2))))
        (else x)))

; Given a list of identifiers, or a formal parameter "list",
; returns an alist that associates each identifier with a fresh identifier.

(define (rename-vars original-vars)
  (let ((rename (make-rename-procedure)))
    (define (loop vars newvars)
      (cond ((null? vars) (reverse newvars))
            ((pair? vars)
             (let ((var (car vars)))
               (if (symbol? var)
                   (loop (cdr vars)
                         (cons (cons var (rename var))
                               newvars))
                   (m-error "Illegal variable" var))))
            ((symbol? vars)
             (loop (list vars) newvars))
            (else (m-error "Malformed parameter list" original-vars))))
    (loop original-vars '())))

; Given a <formals> and an alist returned by rename-vars that contains
; a new name for each formal identifier in <formals>, renames the
; formal identifiers.

(define (rename-formals formals alist)
  (cond ((null? formals) '())
        ((pair? formals)
         (cons (cdr (assq (car formals) alist))
               (rename-formals (cdr formals) alist)))
        (else (cdr (assq formals alist)))))
