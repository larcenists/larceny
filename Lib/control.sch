;                                    21 March 1990

; CHAPTER.  Control structure macros.

(macro cond
       (lambda (l)
         (optimize space)
         (if (null? (cdr l))
             ''()
             (if (memq (car (car (cdr l))) '(#!true else))
                 (cons 'begin (cdr (car (cdr l))))
                 (if (=? (length (car (cdr l))) 1)
                     (list 'or
                           (car (car (cdr l)))
                           (cons 'cond (cdr (cdr l))))
                     (list 'if
                           (car (car (cdr l)))
                           (cons 'begin (cdr (car (cdr l))))
                           (cons 'cond (cdr (cdr l)))))))))

; Tail-recursive and, implemented as nested if expressions.

(macro and
       (lambda (l)
         (optimize space)
         (if (null? (cdr l))
             '#!true
             (if (null? (cdr (cdr l))); one conjunct
                 (car (cdr l))
                 (list 'if
                       (car (cdr l))
                       (cons 'and (cdr (cdr l))))))))

; Now a special form.
; Tail-recursive or.
;
;(macro or
;    (lambda (l)
;        (if (null? (cdr l))
;            ''#!false
;            (if (null? (cdr (cdr l))); one disjunct
;                (car (cdr l))
;                (list '(lambda (v r) (if v v (r)))
;                      (car (cdr l))
;                      (list 'lambda
;                            '()
;                            (cons 'or (cdr (cdr l)))))))))

(macro case
       (lambda (l)
         (optimize space)
         ((lambda (tag)       ; let is defined later
            ((lambda (body)
               (if (symbol? (cadr l))
                   body
                   (list 'let (list (list tag (cadr l))) body)))
             (cons
              'cond
              (map
               (lambda (x)
                 (cond ((eq? (car x) 'else) x)
                       ((and (pair? (car x)) (=? (length (car x)) 1))
                        (cons (list (if (and (number? (car (car x)))
                                             (not (fixnum? (car (car x)))))
                                        'eqv?
                                        'eq?)
                                    tag
                                    (list 'quote (car (car x))))
                              (cdr x)))
                       ((pair? (car x))
                        (cons (list (if (some? (lambda (x)
                                                 (and (number? x)
                                                      (not (fixnum? x))))
                                               (car x))
                                        'memv
                                        'memq)
                                    tag
                                    (list 'quote (car x)))
                              (cdr x)))
                       (else (cons (list (if (and (number? (car x))
                                                  (not (fixnum? (car x))))
                                             'eqv?
                                             'eq?)
                                         tag
                                         (list 'quote (car x)))
                                   (cdr x)))))
               (cddr l)))))
          (if (symbol? (cadr l)) (cadr l) (gensym)))))

;(load "block-macros.sch")
; Block structure macros:  let, letrec, rec, etc.

(macro let
       (lambda (l)
         (optimize space)
         (if (and (atom? (cadr l))
                  (not (null? (cadr l))))
             (cons (list 'letrec                  ; named let
                         (list (list (cadr l)
                                     (cons 'lambda
                                           (cons (map car (caddr l))
                                                 (cdddr l)))))
                         (cadr l))
                   (map (lambda (x) (cadr x)) (caddr l)))
             (cons (cons 'lambda               ; standard let
                         (cons (map car (cadr l))
                               (cddr l)))
                   (map (lambda (x) (cadr x)) (cadr l))))))

(macro let*
       (lambda (l)
         (optimize space)
         (if (or (null? (cadr l)) (null? (cdr (cadr l))))
             (cons 'let (cdr l))
             (list 'let
                   (list (car (cadr l)))
                   (cons 'let*
                         (cons (cdr (cadr l)) (cddr l)))))))

(macro letrec
       (lambda (l)
         (optimize space)
         (cons (list 'lambda
                     (map car (cadr l))
                     (cons 'begin
                           (map (lambda (x) (cons 'set! x)) (cadr l)))
                     (cons 'let (cons '() (cddr l))))
               (map (lambda (x) '(undefined)) (cadr l)))))

(macro rec
       (lambda (l)
         (optimize space)
         (cons (list 'lambda
                     (list (cadr l))
                     (cons 'set! (cdr l))
                     (cadr l))
               '((undefined)))))

; (load "iteration-macros.sch")

; The notorious do macro.

(macro do
       (let ((oops (lambda (l) 
                     (optimize space)
                     (error "Malformed do expression" l))))
         (lambda (l)
           (optimize space)
           (if (not (proper-list? l)) (oops l))
           (if (<? (length l) 3) (oops l))
           (if (not (proper-list? (cadr l))) (oops l))
           (if (not (and (proper-list? (caddr l)) (pair? (caddr l))))
               (oops l))
           (let
             ((loop (gensym "DO"))
              (bindings (map (lambda (x)
                               (cond ((atom? x) (oops l))
                                     ((atom? (cdr x))
                                      (list (car x) '() (car x)))
                                     ((atom? (cddr x))
                                      (list (car x) (cadr x) (car x)))
                                     (else x)))
                             (cadr l)))
              (test (caddr l)))
             (cons (list 'letrec
                         (list (list loop
                                     (list 'lambda
                                           (map car bindings)
                                           (list 'if
                                                 (car test)
                                                 (cons 'begin (cdr test))
                                                 (list 'begin
                                                       (cons 'begin (cdddr l))
                                                       (cons loop (map caddr bindings)))))))
                         loop)
                   (map cadr bindings))))))

(macro while
       (lambda (l)
         (optimize space)
         (let ((loop (gensym "WHILE")))
           (list (list 'rec
                       loop
                       (list 'lambda
                             '()
                             (list 'if
                                   (cadr l)
                                   (list 'begin
                                         (cons 'begin (cddr l))
                                         (list loop)))))))))

; Mapping functions.

;(define (mapcar f l)
;        (if (null? l) '() (cons (f (car l)) (mapcar f (cdr l)))))
;
;(define (mapc f l)
;        (if (null? l) '#!false (begin (f (car l)) (mapc f (cdr l)))))
;
;(define map mapcar)
;
;(define for-each mapc)

(letrec
  ((map2 (lambda (f l)
           (if (null? l) '() (cons (f (car l)) (map2 f (cdr l))))))
   (for-each2 (lambda (f l)
                (if (null? l)
                    #!false
                    (begin (f (car l))
                           (for-each2 f (cdr l))))))
   (map0 (lambda (f l1 . rest)
           (cond ((null? l1) '())
                 (else (cons (apply f (cons (car l1) (map2 car rest)))
                             (apply map0 (help0 f l1 rest)))))))
   (for-each0 (lambda (f l1 . rest)
                (cond ((null? l1) #!false)
                      (else (apply f (cons (car l1) (map2 car rest)))
                            (apply for-each0 (help0 f l1 rest))))))
   (help0 (lambda (f l1 rest)
            (cons f (cons (cdr l1) (map2 cdr rest)))))
   )
  (set! map
        (lambda (f l1 . rest)
          (if (null? rest)
              (map2 f l1)
              (apply map0 (cons f (cons l1 rest))))))
  (set! mapcar map)
  (set! for-each
        (lambda (f l1 . rest)
          (if (null? rest)
              (for-each2 f l1)
              (apply for-each0 (cons f (cons l1 rest))))))
  (set! mapc for-each)
  #!true)

; delay and force

; (delay exp) => ('#<PROCEDURE make-promise>  (lambda () exp))

(macro delay
       (lambda (l)
         ;was `(',make-promise (lambda () ,(cadr l))))))
         ;now `(make-promise (lambda () ,(cadr l)))
         (list 'make-promise
               (list 'lambda '() (cadr l)))))

(define make-promise
  (lambda (proc)
    (optimize space)
    (let ((already-run? #!false)
          (result #!false))
      (lambda ()
        (if (not already-run?)
            (begin (set! result (proc))
                   (set! already-run? #!true)
                   (set! proc #!false)))
        result))))

(define force
  (lambda (proc)
    (optimize space)
    (proc)))

; Escape procedures.

(define call-with-current-continuation
  (lambda (f)
    (let ((k (creg)))
      (f (lambda (v) (begin (creg-set! k) v))))))
