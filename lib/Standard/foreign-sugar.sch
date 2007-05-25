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

;; The simplest way to write this macro involves var-args and apply,
;; but why pay for that overhead when we have an arg count handed to
;; us?  Instead I use expansion-passing-style to generate a set of
;; fresh identifiers to use as placeholder argument names.
(define-syntax define-foreign
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

