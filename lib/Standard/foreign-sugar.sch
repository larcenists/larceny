(require 'foreign-stdlib)

;; : [Listof ([Listof Char] -> [Maybe [Listof Char]])]
(define foreign-name-generators '())
(define permanent-foreign-name-generators '())

;; push-foreign-name-generator! :
;;   ([Listof Char] -> [Maybe [Listof Char]]) -> unspecified
(define (install-foreign-name-generator! proc)
  (set! permanent-foreign-name-generators
        (cons proc permanent-foreign-name-generators)))

;; push-foreign-name-generator! :
;;   ([Listof Char] -> [Maybe [Listof Char]]) -> unspecified
(define (push-foreign-name-generator! proc)
  (set! foreign-name-generators 
        (cons proc foreign-name-generators)))

(define (pop-foreign-name-generator!)
  (set! foreign-name-generators (cdr foreign-name-generators)))

(define (foo-bar-baz->foo_bar_baz lst)
  (apply append
         (map (lambda (c) 
                (case c
                  ((#\-) (list #\_))
                  ((#\!) (list))
                  (else  (list c))))
              lst)))

(install-foreign-name-generator! foo-bar-baz->foo_bar_baz)

(define (foo-bar-baz->fooBarBaz lst)
  (call-with-current-continuation 
   (lambda (exit)
     (let loop ((lst lst))
       (cond 
        ((null? lst) '())
        (else
         (case (car lst)
           ((#\-) (if (null? (cdr lst))
                      (exit #f)
                      (cons (char-upcase (cadr lst))
                            (loop (cddr lst)))))
           ((#\!) (loop (cdr lst)))
           (else
            (cons (car lst) (loop (cdr lst)))))))))))

(install-foreign-name-generator! foo-bar-baz->fooBarBaz)
  
(define (foreign-name->strings sym)
  (let* ((str (symbol->string sym))
         (lst (string->list str))  
         (lsts (map (lambda (f) (f lst)) 
                    (append foreign-name-generators
                            permanent-foreign-name-generators)))
         (lsts (let filter ((lsts lsts))
                 (cond ((null? lsts) '())
                       ((car lsts) (cons (car lsts) (filter (cdr lsts))))
                       (else (filter (cdr lsts))))))
         (strs (map list->string lsts)))
    strs))

(define (first-foreign-procedure all-names arg-desc result-desc)
  (let loop ((names all-names))
    (cond ((null? names)
           (error 'first-foreign-procedure 
                  " none of " all-names 
                  " is provided by the current set of ffi libraries."))
          ((foreign-procedure-provided? (car names))
           (foreign-procedure (car names) arg-desc result-desc))
          (else
           (loop (cdr names))))))

;; This layer strips off occurrences of output parameter form (ret X)
;; in the signature and does the necessary boxing and marshalling to
;; return the output parameters via multiple value return.
(define-syntax define-foreign 
  (transformer 
   (lambda (exp ren cmp) ;; exp ::= (define-foreign (NM ARG-TYPE ...) RET-TYPE)
     
     (define (process-args arg-types) 
       (let loop ((i 0) 
                  (l arg-types) 
                  (rets '())
                  (high-args '()) 
                  (types '())
                  (low-args '()))
         (cond ((null? l)
                (values (reverse rets)
                        (reverse high-args)
                        (reverse types)
                        (reverse low-args)))
               (else 
                (if (and (pair? (car l))
                         (cmp (car (car l)) 'ret))
                    (let ((r (gensym "r")))
                      (loop (+ i 1) 
                            (cdr l) 
                            (cons (list (cadar l) r i) rets) 
                            high-args
                            (cons 'void* types)
                            (cons r low-args)))
                    (let ((a (gensym "a")))
                      (loop (+ i 1) (cdr l) 
                            rets
                            (cons (list (car l) a i) high-args)
                            (cons (car l) types)
                            (cons a low-args)
                            )))))))

     (let ((nm        (car (cadr exp)))
           (arg-types (cdr (cadr exp)))
           (ret-type  (caddr exp)))
       (call-with-values (lambda () (process-args arg-types))
         (lambda (rets high-args lowlevel-types low-args)
           `(define ,nm
              (let ((malloc (stdlib/malloc void*-rt)))
                (define-foreign-lowlevel (,nm ,@lowlevel-types) ,ret-type)
                (lambda ,(map cadr high-args)
                  (let (,@(map (lambda (ret) (list (cadr ret) `(malloc 8)))
                               rets))
                    (let ((ret (,nm ,@low-args)))
                      (let (,@(map (lambda (ret) 
                                     (let ((r (cadr ret))
                                           (t (car ret)))
                                       (list r 
                                             `((list-ref (ffi-attribute-entry ',t) 3)
                                               (void*-word-ref ,r 0) ',nm
                                               ))))
                                   rets))

                        ,(if (eq? ret-type 'void)
                             `(values ,@(map cadr rets))
                             `(values ret ,@(map cadr rets)))))))))))))))

;; The simplest way to write this macro involves var-args and apply,
;; but why pay for that overhead when we have an arg count handed to
;; us?  Instead I use expansion-passing-style to generate a set of
;; fresh identifiers to use as placeholder argument names.
(define-syntax define-foreign-lowlevel
  (syntax-rules ()
    ((define-foreign (NAME ARG-TYPES ...) RESULT-TYPE)
     (generate-ids (ARG-TYPES ...) (NAME RESULT-TYPE) 
                   generate-define-foreign-template))))

(define-syntax generate-define-foreign-template 
  (syntax-rules ()
    ((generate-define-foreign-template (ARG-TYPES ...) (IDS ...) (NAME RESULT-TYPE))
     (define NAME
       (let ((proc (first-foreign-procedure (foreign-name->strings 'NAME) 
                                            '(ARG-TYPES ...) 'RESULT-TYPE)))
         ;; The eta-expansion here puts NAME into the procedures's
         ;; documentation slot, yielding nicer backtraces in debugger.
         (define (NAME IDS ...) (proc IDS ...))
         NAME)))))

(define-syntax generate-ids
  (syntax-rules ()
    ((generate-ids (INPUTS ...) OTHER FINISH)
     (generate-ids (INPUTS ...) () () OTHER FINISH))
    ((generate-ids (INPUT INPUTS ...) (COPIES ...) (IDS ...) OTHER FINISH)
     (generate-ids (INPUTS ...) (COPIES ... INPUT) (fresh IDS ...) OTHER FINISH))
    ((generate-ids () (COPIES ...) (IDS ...) OTHER FINISH)
     (FINISH (COPIES ...) (IDS ...) OTHER))))

