; Copyright 1992 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; 23 November 1998
; Compiler for a <transformer spec>.
;
; References:
;
;    The Revised^4 Report on the Algorithmic Language Scheme.
;    Clinger and Rees [editors].  To appear in Lisp Pointers.
;    Also available as a technical report from U of Oregon,
;    MIT AI Lab, and Cornell.
;
;    Macros That Work.  Clinger and Rees.  POPL '91.
;
; The input is a <transformer spec> and a syntactic environment.
; Syntactic environments are described in another file.
;
; The supported syntax differs from the R4RS in that vectors are
; allowed as patterns and as templates and are not allowed as
; pattern or template data.
;
;    <transformer spec>  -->  (syntax-rules <literals> <rules>)
;    <rules>  -->  ()  |  (<rule> . <rules>)
;    <rule> --> (<pattern> <template>)
;    <pattern> --> <pattern_var>      ; a <symbol> not in <literals>
;                | <symbol>           ; a <symbol> in <literals>
;                | ()
;                | (<pattern> . <pattern>)
;                | (<ellipsis_pattern>)
;                | #(<pattern>*)                     ; extends R4RS
;                | #(<pattern>* <ellipsis_pattern>)  ; extends R4RS
;                | <pattern_datum>
;    <template> --> <pattern_var>
;                |  <symbol>
;                |  ()
;                |  (<template2> . <template2>)
;                |  #(<template>*)                   ; extends R4RS
;                |  <pattern_datum>
;    <template2> --> <template>  |  <ellipsis_template>
;    <pattern_datum> --> <string>                    ; no <vector>
;                     |  <character>
;                     |  <boolean>
;                     |  <number>
;    <ellipsis_pattern>  --> <pattern> ...
;    <ellipsis_template> --> <template> ...
;    <pattern_var>       --> <symbol> ; not in <literals>
;    <literals>  -->  ()  |  (<symbol> . <literals>)
;
; Definitions.
;
; scope of an ellipsis
;
;    Within a pattern or template, the scope of an ellipsis
;    (...) is the pattern or template that appears to its left.
;
; rank of a pattern variable
;
;    The rank of a pattern variable is the number of ellipses
;    within whose scope it appears in the pattern.
;
; rank of a subtemplate
;
;    The rank of a subtemplate is the number of ellipses within
;    whose scope it appears in the template.
;
; template rank of an occurrence of a pattern variable
;
;    The template rank of an occurrence of a pattern variable
;    within a template is the rank of that occurrence, viewed
;    as a subtemplate.
;
; variables bound by a pattern
;
;    The variables bound by a pattern are the pattern variables
;    that appear within it.
;
; referenced variables of a subtemplate
;
;    The referenced variables of a subtemplate are the pattern
;    variables that appear within it.
;
; variables opened by an ellipsis template
;
;    The variables opened by an ellipsis template are the
;    referenced pattern variables whose rank is greater than
;    the rank of the ellipsis template.
;    
;
; Restrictions.
;
;    No pattern variable appears more than once within a pattern.
;
;    For every occurrence of a pattern variable within a template,
;    the template rank of the occurrence must be greater than or
;    equal to the pattern variable's rank.
;
;    Every ellipsis template must open at least one variable.
;    
;    For every ellipsis template, the variables opened by an
;    ellipsis template must all be bound to sequences of the
;    same length.
;
;
; The compiled form of a <rule> is
;
;    <rule> --> (<pattern> <template> <inserted>)
;    <pattern> --> <pattern_var>
;                | <symbol>
;                | ()
;                | (<pattern> . <pattern>)
;                | <ellipsis_pattern>
;                | #(<pattern>)
;                | <pattern_datum>
;    <template> --> <pattern_var>
;                |  <symbol>
;                |  ()
;                |  (<template2> . <template2>)
;                |  #(<pattern>)
;                |  <pattern_datum>
;    <template2> --> <template>  |  <ellipsis_template>
;    <pattern_datum> --> <string>
;                     |  <character>
;                     |  <boolean>
;                     |  <number>
;    <pattern_var>       --> #(<V> <symbol> <rank>)
;    <ellipsis_pattern>  --> #(<E> <pattern> <pattern_vars>)
;    <ellipsis_template> --> #(<E> <template> <pattern_vars>)
;    <inserted> -->     ()  |  (<symbol> . <inserted>)
;    <pattern_vars> --> ()  |  (<pattern_var> . <pattern_vars>)
;    <rank>  -->  <exact non-negative integer>
;
; where <V> and <E> are unforgeable values.
; The pattern variables associated with an ellipsis pattern
; are the variables bound by the pattern, and the pattern
; variables associated with an ellipsis template are the
; variables opened by the ellipsis template.
;
;
; What's wrong with the above?
; If the template contains a big chunk that contains no pattern variables
; or inserted identifiers, then the big chunk will be copied unnecessarily.
; That shouldn't matter very often.

(define pattern-variable-flag (list 'v))
(define ellipsis-pattern-flag (list 'e))
(define ellipsis-template-flag ellipsis-pattern-flag)

(define (make-patternvar v rank)
  (vector pattern-variable-flag v rank))
(define (make-ellipsis-pattern P vars)
  (vector ellipsis-pattern-flag P vars))
(define (make-ellipsis-template T vars)
  (vector ellipsis-template-flag T vars))

(define (patternvar? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) pattern-variable-flag)))

(define (ellipsis-pattern? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) ellipsis-pattern-flag)))

(define (ellipsis-template? x)
  (and (vector? x)
       (= (vector-length x) 3)
       (eq? (vector-ref x 0) ellipsis-template-flag)))

(define (patternvar-name V) (vector-ref V 1))
(define (patternvar-rank V) (vector-ref V 2))
(define (ellipsis-pattern P) (vector-ref P 1))
(define (ellipsis-pattern-vars P) (vector-ref P 2))
(define (ellipsis-template T) (vector-ref T 1))
(define (ellipsis-template-vars T) (vector-ref T 2))

(define (pattern-variable v vars)
  (cond ((null? vars) #f)
        ((eq? v (patternvar-name (car vars)))
         (car vars))
        (else (pattern-variable v (cdr vars)))))

; Given a <transformer spec> and a syntactic environment,
; returns a macro denotation.
;
; A macro denotation is of the form
;
;    (macro (<rule> ...) env)
;
; where each <rule> has been compiled as described above.

(define (m-compile-transformer-spec spec env)
  (if (and (> (safe-length spec) 1)
           (eq? (syntactic-lookup env (car spec))
                denotation-of-syntax-rules))
      (let ((literals (cadr spec))
            (rules (cddr spec)))
        (if (or (not (list? literals))
                (not (every1? (lambda (rule)
                                (and (= (safe-length rule) 2)
                                     (pair? (car rule))))
                              rules)))
            (m-error "Malformed syntax-rules" spec))
        (list 'macro
              (map (lambda (rule)
                     (m-compile-rule rule literals env))
                   rules)
              env))
      (m-error "Malformed syntax-rules" spec)))

(define (m-compile-rule rule literals env)
  (m-compile-pattern (cdr (car rule))
                     literals
                     env
                     (lambda (compiled-rule patternvars)
                       ; FIXME
                       ; should check uniqueness of pattern variables here
                       (cons compiled-rule
                             (m-compile-template
                              (cadr rule)
                              patternvars
                              env)))))

(define (m-compile-pattern P literals env k)
  (define (loop P vars rank k)
    (cond ((symbol? P)
           (if (memq P literals)
               (k P vars)
               (let ((var (make-patternvar P rank)))
                 (k var (cons var vars)))))
          ((null? P) (k '() vars))
          ((pair? P)
           (if (and (pair? (cdr P))
                    (symbol? (cadr P))
                    (same-denotation? (syntactic-lookup env (cadr P))
                                      denotation-of-...))
               (if (null? (cddr P))
                   (loop (car P)
                         '()
                         (+ rank 1)
                         (lambda (P vars1)
                           (k (make-ellipsis-pattern P vars1)
                              (union2 vars1 vars))))
                   (m-error "Malformed pattern" P))
               (loop (car P)
                     vars
                     rank
                     (lambda (P1 vars)
                       (loop (cdr P)
                             vars
                             rank
                             (lambda (P2 vars)
                               (k (cons P1 P2) vars)))))))
          ((vector? P)
           (loop (vector->list P)
                 vars
                 rank
                 (lambda (P vars)
                   (k (vector P) vars))))
          (else (k P vars))))
  (loop P '() 0 k))

(define (m-compile-template T vars env)
  
  (define (loop T inserted referenced rank escaped? k)
    (cond ((symbol? T)
           (let ((x (pattern-variable T vars)))
             (if x
                 (if (>= rank (patternvar-rank x))
                     (k x inserted (cons x referenced))
                     (m-error
                      "Too few ellipses follow pattern variable in template"
                      (patternvar-name x)))
                 (k T (cons T inserted) referenced))))
          ((null? T) (k '() inserted referenced))
          ((pair? T)
           (cond ((and (not escaped?)
                       (symbol? (car T))
                       (same-denotation? (syntactic-lookup env (car T))
                                         denotation-of-...)
                       (pair? (cdr T))
                       (null? (cddr T)))
                  (loop (cadr T) inserted referenced rank #t k))
                 ((and (not escaped?)
                       (pair? (cdr T))
                       (symbol? (cadr T))
                       (same-denotation? (syntactic-lookup env (cadr T))
                                         denotation-of-...))
                  (loop1 T inserted referenced rank escaped? k))
                 (else
                  (loop (car T)
                        inserted
                        referenced
                        rank
                        escaped?
                        (lambda (T1 inserted referenced)
                          (loop (cdr T)
                                inserted
                                referenced
                                rank
                                escaped?
                                (lambda (T2 inserted referenced)
                                  (k (cons T1 T2) inserted referenced))))))))
          ((vector? T)
           (loop (vector->list T)
                 inserted
                 referenced
                 rank
                 escaped?
                 (lambda (T inserted referenced)
                   (k (vector T) inserted referenced))))
          (else (k T inserted referenced))))
  
  (define (loop1 T inserted referenced rank escaped? k)
    (loop (car T)
          inserted
          '()
          (+ rank 1)
          escaped?
          (lambda (T1 inserted referenced1)
            (loop (cddr T)
                  inserted
                  (append referenced1 referenced)
                  rank
                  escaped?
                  (lambda (T2 inserted referenced)
                    (k (cons (make-ellipsis-template
                              T1
                              (filter1 (lambda (var)
                                         (> (patternvar-rank var)
                                            rank))
                                       referenced1))
                             T2)
                       inserted
                       referenced))))))
  
  (loop T
        '()
        '()
        0
        #f
        (lambda (T inserted referenced)
          (list T inserted))))

; The pattern matcher.
;
; Given an input, a pattern, and two syntactic environments,
; returns a pattern variable environment (represented as an alist)
; if the input matches the pattern, otherwise returns #f.

(define empty-pattern-variable-environment
  (list (make-patternvar (string->symbol "") 0)))

(define (m-match F P env-def env-use)
  
  (define (match F P answer rank)
    (cond ((null? P)
           (and (null? F) answer))
          ((pair? P)
           (and (pair? F)
                (let ((answer (match (car F) (car P) answer rank)))
                  (and answer (match (cdr F) (cdr P) answer rank)))))
          ((symbol? P)
           (and (symbol? F)
                (same-denotation? (syntactic-lookup env-def P)
                                  (syntactic-lookup env-use F))
                answer))
          ((patternvar? P)
           (cons (cons P F) answer))
          ((ellipsis-pattern? P)
           (match1 F P answer (+ rank 1)))
          ((vector? P)
           (and (vector? F)
                (match (vector->list F) (vector-ref P 0) answer rank)))
          (else (and (equal? F P) answer))))
  
  (define (match1 F P answer rank)
    (cond ((not (list? F)) #f)
          ((null? F)
           (append (map (lambda (var) (cons var '()))
                        (ellipsis-pattern-vars P))
                   answer))
          (else
           (let* ((P1 (ellipsis-pattern P))
                  (answers (map (lambda (F) (match F P1 answer rank))
                                F)))
             (if (every1? (lambda (answer) answer) answers)
                 (append (map (lambda (var)
                                (cons var
                                      (map (lambda (answer)
                                             (cdr (assq var answer)))
                                           answers)))
                              (ellipsis-pattern-vars P))
                         answer)
                 #f)))))
  
  (match F P empty-pattern-variable-environment 0))

(define (m-rewrite T alist)
  
  (define (rewrite T alist rank)
    (cond ((null? T) '())
          ((pair? T)
           ((if (ellipsis-pattern? (car T))
                append
                cons)
            (rewrite (car T) alist rank)
            (rewrite (cdr T) alist rank)))
          ((symbol? T) (cdr (assq T alist)))
          ((patternvar? T) (cdr (assq T alist)))
          ((ellipsis-template? T)
           (rewrite1 T alist (+ rank 1)))
          ((vector? T)
           (list->vector (rewrite (vector-ref T 0) alist rank)))
          (else T)))
  
  (define (rewrite1 T alist rank)
    (let* ((T1 (ellipsis-template T))
           (vars (ellipsis-template-vars T))
           (rows (map (lambda (var) (cdr (assq var alist)))
                      vars)))
      (map (lambda (alist) (rewrite T1 alist rank))
           (make-columns vars rows alist))))
  
  (define (make-columns vars rows alist)
    (define (loop rows)
      (if (null? (car rows))
          '()
          (cons (append (map (lambda (var row)
                               (cons var (car row)))
                             vars
                             rows)
                        alist)
                (loop (map cdr rows)))))
    (if (or (null? (cdr rows))
            (apply = (map length rows)))
        (loop rows)
        (m-error "Use of macro is not consistent with definition"
                 vars
                 rows)))
  
  (rewrite T alist 0))

; Given a use of a macro, the syntactic environment of the use,
; a continuation that expects a transcribed expression and
; a new environment in which to continue expansion, and a boolean
; that is true if this transcription is for an inline procedure,
; does the right thing.

(define (m-transcribe0 exp env-use k inline?)
  (let* ((m (syntactic-lookup env-use (car exp)))
         (rules (macro-rules m))
         (env-def (macro-env m))
         (F (cdr exp)))
    (define (loop rules)
      (if (null? rules)
          (if inline?
              (k exp env-use)
              (m-error "Use of macro does not match definition" exp))
          (let* ((rule (car rules))
                 (pattern (car rule))
                 (alist (m-match F pattern env-def env-use)))
            (if alist
                (let* ((template (cadr rule))
                       (inserted (caddr rule))
                       (alist2 (rename-vars inserted))
                       (newexp (m-rewrite template (append alist2 alist))))
                  (k newexp
                     (syntactic-alias env-use alist2 env-def)))
                (loop (cdr rules))))))
    (if (procedure? rules)
        (m-transcribe-low-level exp env-use k rules env-def)
        (loop rules))))

(define (m-transcribe exp env-use k)
  (m-transcribe0 exp env-use k #f))

(define (m-transcribe-inline exp env-use k)
  (m-transcribe0 exp env-use k #t))

