; Syntax checker for the beginning student language.
;
; Given a program, represented as a list of definitions
; and expressions, returns four values:
;
; a boolean (#t iff the syntax is okay)
; the first definition or expression that contains an error
;     (or a meaningless value if the syntax is okay)
; a possibly smaller subexpression that contains an error
;     (or a meaningless value if the syntax is okay)
; a string describing the error
;     (or a meaningless value if the syntax is okay)
;
; FIXME: what about duplicate formal parameters?
; FIXME: what about multiple definitions of the same name?

(define (check-beginning-program pgm)
  (define (return okay? def exp msg)
    (values okay? def exp msg))
  (if (null? pgm)
      (return #t (unspecified) (unspecified) (unspecified))
      (call-with-values
       (lambda () (check-beginning-definition-or-expression (car pgm)))
       (lambda (okay? def exp str)
         (if okay?
             (check-beginning-program (cdr pgm))
             (return okay? def exp str))))))

(define (check-beginning-definition-or-expression def/exp)
  (if (and (pair? def/exp)
           (or (eq? 'define (car def/exp))
               (eq? 'define-struct (car def/exp))))
      (check-beginning-definition def/exp)
      (check-beginning-expression def/exp def/exp)))

(define (check-beginning-definition def)
  (define (return okay? def exp msg)
    (values okay? def exp msg))
  (cond ((or (not (pair? def))
             (not (list? def)))
         (return #f def def "bad definition"))
        ((eq? 'define-struct (car def))
         (cond ((not (= 3 (length def)))
                (return #f def def "bad structure definition"))
               ((not (list? (caddr def)))
                (return #f def def "bad field list"))
               ((not (symbol? (cadr def)))
                (return #f def (cadr def) "bad structure name"))
               ((not (for-all symbol? (caddr def)))
                (return #f def (caddr def) "bad field name"))
               (else
                (return #t (unspecified) (unspecified) (unspecified)))))
        ((eq? 'define (car def))
         (cond ((not (= 3 (length def)))
                (return #f def def "bad definition"))
               ((list? (cadr def))
                (let ((protocol (cadr def)))
                  (cond ((not (<= 2 (length protocol)))
                         (return #f def protocol "no formal parameters"))
                        ((not (symbol? (car protocol)))
                         (return #f def (car protocol) "bad procedure name"))
                        ((not (for-all symbol? protocol))
                         (return #f def protocol "bad formal parameter"))
                        (else
                         (check-beginning-expression def (caddr def))))))
               (else
                (let ((exp (caddr def)))
                  (cond ((and (pair? exp) (eq? 'lambda (car exp)))
                         (cond ((not (symbol? (cadr def)))
                                (return #f
                                        def (cadr def) "bad procedure name"))
                               ((not (and (list? exp) (= 3 (length exp))))
                                (return #f def exp "bad lambda syntax"))
                               ((not (for-all symbol? (cadr exp)))
                                (return #f
                                        def (cadr exp) "bad formal parameter"))
                               (else
                                (check-beginning-expression def (caddr exp)))))
                        ((not (symbol? (cadr def)))
                         (return #f def (cadr def) "bad variable name"))
                        (else
                         (check-beginning-expression def (caddr def))))))))
        (else
         (return #f def def "bad definition"))))

(define (check-beginning-expression def exp)
  (define (accept)
    (values #t (unspecified) (unspecified) (unspecified)))
  (define (complain exp msg)
    (values #f def exp msg))
  (cond ((and (pair? exp) (not (list? exp)))
         (complain exp "bad expression syntax"))
        ((and (pair? exp) (eq? 'cond (car exp)))
         (check-beginning-cond-clauses def exp (cdr exp)))
        ((and (pair? exp) (eq? 'if (car exp)))
         (cond ((not (= 4 (length exp)))
                (complain exp "bad if expression"))
               (else
                (check-beginning-expressions def exp (cdr exp)))))
        ((and (pair? exp) (or (eq? 'and (car exp)) (eq? 'or (car exp))))
         (cond ((not (<= 3 (length exp)))
                (complain exp "bad and/or expression"))
               (else
                (check-beginning-expressions def exp (cdr exp)))))
        ((and (pair? exp) (symbol? exp))
         (cond ((<= 2 (length exp))
                (check-beginning-expressions def exp (cdr exp)))
               ((beginning-primop? (car exp))
                (accept))
               (else
                (complain (car exp) "only primops can take no arguments"))))
        ((eq? 'empty exp)
         (accept))
        ((and (symbol? exp) (beginning-name? exp))
         (accept))
        ((and (pair? exp) (eq? 'quote (car exp)))
         (cond ((not (and (= 2 (length exp)) (beginning-name? (cadr exp))))
                (complain exp "bad quoted symbol"))
               (else (accept))))
        ((number? exp)
         (accept))
        ((eq? 'true exp)
         (accept))
        ((eq? 'false exp)
         (accept))
        ((string? exp)
         (accept))
        ((char? exp)
         (accept))
        (else
         (complain exp "bad expression syntax"))))

(define (check-beginning-cond-clauses def exp clauses)
  (define (accept)
    (values #t (unspecified) (unspecified) (unspecified)))
  (define (complain clause msg)
    (values #f def clause msg))
  (define (loop clauses n)
    (cond ((null? clauses)
           (if (> n 0)
               (accept)
               (complain exp "cond expression has no clauses")))
          ((and (list? (car clauses))
                (= 2 (length (car clauses))))
           (call-with-values
            (lambda () (check-beginning-expressions def exp (car clauses)))
            (lambda (okay? def exp msg)
              (if okay?
                  (loop (cdr clauses) (+ n 1))
                  (values okay? def exp msg)))))
          (else
           (complain (car clauses) "bad cond clause"))))
  (loop clauses 0))

(define (check-beginning-expressions def exp0 exps)
  (cond ((null? exps)
         (values #t (unspecified) (unspecified) (unspecified)))
        (else
         (call-with-values
          (lambda () (check-beginning-expression def (car exps)))
          (lambda (okay? def exp msg)
            (if okay?
                (check-beginning-expressions def exp0 (cdr exps))
                (values okay? def exp msg)))))))

; FIXME: relies upon hashtable of primop types

(define (beginning-primop? sym)
  (if (hashtable-ref beginning-primop-types sym #f)
      #t
      #f))

(define (beginning-name? sym)
  (define (name-character? c)
    (and (<= 33 (char->integer c) 126)
         (not (memv c
                    '(#\" #\, #\' #\` #\( #\) #\[ #\] #\{ #\} #\| #\; #\#)))))
  (and (symbol? sym)
       (let ((chars (string->list (symbol->string sym))))
         (for-all name-character? chars))))
