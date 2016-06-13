;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To simplify interoperability between R7RS and R6RS, this
;;; implementation of define-record-type supports all four of
;;; these specifications of define-record-type:
;;;
;;;     SRFI 9
;;;     ERR5RS
;;;     R6RS      (deprecated)
;;;     R7RS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax define-record-type

  (syntax-rules (fields parent protocol
                 sealed opaque nongenerative
                 parent-rtd)

   ;; R6RS syntax (deprecated)

   ((_ (<name> <constructor> <predicate>) stuff ...)
    (r6rs-define-record-type (<name> <constructor> <predicate>) stuff ...))

   ((_ <name> (fields <spec> ...) stuff ...)
    (r6rs-define-record-type <name> (fields <spec> ...) stuff ...))

   ((_ <name> (parent <pname>) stuff ...)
    (r6rs-define-record-type <name> (parent <pname>) stuff ...))

   ((_ <name> (protocol <exp>) stuff ...)
    (r6rs-define-record-type <name> (protocol <exp>) stuff ...))

   ((_ <name> (sealed <boolean>) stuff ...)
    (r6rs-define-record-type <name> (sealed <boolean>) stuff ...))

   ((_ <name> (opaque <boolean>) stuff ...)
    (r6rs-define-record-type <name> (opaque <boolean>) stuff ...))

   ((_ <name> (nongenerative) stuff ...)
    (r6rs-define-record-type <name> (nongenerative) stuff ...))

   ((_ <name> (nongenerative <uid>) stuff ...)
    (r6rs-define-record-type <name> (nongenerative <uid>) stuff ...))

   ((_ <name> (parent-rtd <rtd> <cd>) stuff ...)
    (r6rs-define-record-type <name> (parent-rtd <rtd> <cd>) stuff ...))

   ((_ <name>)
    (r6rs-define-record-type <name>))

   ;; R7RS, ERR5RS, and SRFI 9 syntax

   ((_ <name> <constructor> <pred> <field> ...)
    (r7rs-define-record-type <name> <constructor> <pred> <field> ...))))

;;; From R7RS 7.3

(define-syntax define-values
  (syntax-rules ()
    ((define-values () expr)
     (define dummy
       (call-with-values
         (lambda () expr)
         (lambda args #f))))
    ((define-values (var) expr)
     (define var expr))
    ((define-values (var0 var1 ... varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr) list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v))
       ...
       (define varn
         (let ((v (cadr var0))) (set! var0 (car var0)) v))))
    ((define-values (var0 var1 ... . varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr) list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v))
       ...
       (define varn
         (let ((v (cdr var0))) (set! var0 (car var0)) v))))
    ((define-values var expr)
     (define var
       (call-with-values (lambda () expr) list)))))

;;; From src/Compiler/usual.sch
;;;
;;; With the SRFI-39 and R7RS semantics, we have to bypass a call to the
;;; conversion procedure, which is done by passing the no-conversion symbol
;;; as an extra argument.  That extra argument is recognized only by real
;;; parameters, so we have to be careful.

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((p1 e1) ...) b1 b2 ...)
     (letrec-syntax 
         ((parameterize-aux
           (... (syntax-rules ()
                  ((parameterize-aux (t ...) ((p0 e0) x ...) body1 body2 ...)
                   (let ((tempE e0)
                         (tempP p0)
                         (first-time? #t))
                     (parameterize-aux ((tempE tempP first-time?) t ...) 
                                       (x ...) 
                                       body1 body2 ...)))
                  ((parameterize-aux ((tE tP first-time?) ...) ()
                    body1 body2 ...)
                   (let-syntax ((swap!
                                 (syntax-rules ()
                                   ((swap! var param)
                                    (let ((tmp var))
                                      (set! var (param))
                                      (param tmp)))
                                   ((swap! var param flag)
                                    (let ((tmp var))
                                      (set! var (param))
                                      (if (parameter? param)
                                          (param tmp flag)
                                          (param tmp)))))))
                     (dynamic-wind
                      (lambda ()
                        (begin
                         (if first-time?
                             (swap! tE tP)
                             (swap! tE tP 'no-conversion))
                         (set! first-time? #f))
                        ...)
                      (lambda ()
                        body1 body2 ...)
                      (lambda ()
                        (swap! tE tP 'no-conversion) ...))))))))
       (parameterize-aux () ((p1 e1) ...) b1 b2 ...)))))

;;; eof
