; Support for variable names in the debugger -- preliminary.
;
; A hack has been inserted in cg-lambda in pass4p1.sch that stores in
; slot 5 of the documentation structure (the "formals" slot) the list of
; variables closed over by a lambda expression.
;
; Using that information, and an interpreter enhancement, we can extract 
; information about the variables closed over by a particular procedure, 
; including their locations.

; Given a procedure, return a structure that maps lexically bound 
; identifiers to locations, and locations to values.

(define (procedure-environment proc)
  (if (interpreted-procedure? proc)
      (interpreted-procedure-environment proc)
      (compiled-procedure-environment proc)))


; Given a procedure, return the formal parameter list of the procedure, if
; available.  This list need not be proper.

(define (procedure-formals p)
  (let ((e (procedure-expression p)))
    (if e
        (listify (cadr e))
        #f)))
   

; Used internally because sometimes we must distinguish.
; FIXME: must deal with documentation structures that are assoc lists.
; The right way to fix it is to export the original procedure-documentation
; as compiled-procedure-documentation, and then use that.

(define (compiled-procedure-variables x)
  (let ((doc (vector-ref (procedure-ref x 1) 0)))
    (if (and doc (vector? doc))
        (vector-ref doc 5)
        #f)))



; Variable information for (compiled) procedures:
;
; (1) Given a frame in use by a non-tail call, print out the values of 
;     variables in a frame.
; (2) Given a closure, print the values of its free non-global variables.
; (3) Given a frame and a set of registers, print out the values of the 
;     variables in the frame.
;
; (2) is easy given the existing procedure info (see above).
; (1) requires more info to be saved, at nontail call points.
; (3) is hard, can we make this easier?  The issue occurs in
;     exception contexts (and down the line, during debugging).

; Useful abstractions to manipulate procedures.

(define proc:reg0 2)

(define (procedure-static-link proc)
  (procedure-loookup proc 0 0))

(define (procedure-lookup proc rib offset)
  (cond ((= rib 0)
         (if (<= (procedure-length p) (+ proc:reg0 offset))
             #f
             (procedure-ref p (+ proc:reg0 offset))))
        ((procedure-static-link proc)
         =>
         (lambda (link)
           (procedure-lookup link (- rib 1) offset)))
        (else
         (error "Procedure-lookup went off the deep end."))))


(define (compiled-procedure-environment p)
  (make-procedure-environment #f #f '()))


; Variable information for (interpreted) procedures:
;
; (1) In a non-tail call, print out the values of variables in a frame.
; (2) Given a closure, print the values of its free non-global variables.
; 
; These amount to the same thing, because for an interpreted procedure,
; the _procedure_ in REG0 in the frame points to an environment that has
; all variables and values.  So create a procedure that returns the
; environment or #f:

; This takes an interpreted procedure PROC and returns its environment:
; the current lexical environment structure together with a map that maps
; variable names to lexical addresses.

(define (interpreted-procedure-environment proc)
  
  (define proc:reg0 2)

  (define (formals p)
    (or (procedure-formals p) '()))
  
  (define (store proc)
    (let ((v (compiled-procedure-variables proc)))
      (if (not v)
          #f
          (let loop ((v v) (i 1))
            (cond ((null? v) #f)
                  ((eq? (unmangle-identifier (car v)) 'env)
                   (procedure-ref proc (+ i proc:reg0)))
                  (else
                   (loop (cdr v) (+ i 1))))))))
  
  (define (environment store args ribno)
    (let ((rib (car store)))
      (cons (map (lambda (v i) (list v ribno i))
                 args
                 (iota1 (length args)))
            (if (null? (cdr store))
                '()
                (environment (cdr store)
                             (formals (vector-ref (cadr store) 0))
                             (+ ribno 1))))))

  (define (make-lookup store)
    (lambda (rib offset)
      (define (lookup store rib)
        (if (zero? rib)
            (vector-ref (car store) offset)
            (lookup (cdr store) (- rib 1))))
      (lookup store rib)))
  
  (define (make-update store)
    (lambda (rib offset value)
      (define (lookup store rib)
        (if (zero? rib)
            (vector-set! (car store) offset value)
            (lookup (cdr store) (- rib 1))))
      (lookup store rib)))
  
  (let ((s (store proc)))
    (if (or (not s) (null? s))
        (make-procedure-environment #f #f '())
        (make-procedure-environment
         (make-lookup s)
         (make-update s)
         (environment s 
                      (formals (vector-ref (car s) 0))
                      0)))))


; A procedure-environment is a structure with three fields:
; - A lookup procedure that takes a rib and an offset and returns the
;   value at that rib and offset; it is an error if the lexical address
;   is not defined.  May be #f.
; - An update procedure that takes a rib, an offset, and a value and
;   stores the value at that rib and offset; it is an error if the lexical 
;   address is not defined.  May be #f.
; - A map of variable names to lexical addresses.  The map is a list
;   of ribs, and a rib is a list of variables, and a variable is a list
;   of variable name, rib address, and offset.  Any address in the map
;   can be passed to the lookup and update procedures.

(define (make-procedure-environment lookup update env)
  (vector 'procedure-environment lookup update env))

(define (procedure-environment.lookup env) (vector-ref env 1))
(define (procedure-environment.update env) (vector-ref env 2))
(define (procedure-environment.env env) (vector-ref env 3))



; This is useful but it's not correct for procedure-variables to 
; apply it to all the variables.

(define (unmangle-identifier x)
    (let* ((s (symbol->string x))
           (l (string-length s)))
      (let loop ((i 0))
        (cond ((= i l) x)
              ((char=? (string-ref s i) #\.)
               (loop (+ i 1)))
              (else
               (let loop ((j (+ i 1)))
                 (cond ((= j l) x)
                       ((char=? (string-ref s j) #\|)
                        (string->symbol (substring s i j)))
                       (else (loop (+ j 1))))))))))

; Utility procedures

(define (listify x)
  (cond ((null? x) x)
        ((pair? x) (cons (car x) (listify (cdr x))))
        (else (list x))))
  
(define (iota1 n)
  (define (iota n) 
    (if (zero? n) 
        '()
        (cons n (iota (- n 1))))) 
  (reverse (iota n)))

; eof

