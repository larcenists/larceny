; Copyright 1991 Lightship Software, Incorporated.

(define (make-label)
  (set! cg-label-counter (+ cg-label-counter 1))
  cg-label-counter)

(define cg-label-counter 1000)

;    an assembly stream into which instructions should be emitted
;    an expression
;    the desired target register ('result, a register number, or '#f)
;    a register environment [cgreg]
;    a stack-frame environment [cgframe]
;      contains size of frame, current top of frame
;    a compile-time environment [cgenv]
;    a flag indicating whether the expression is in tail position

; Assembly streams, into which instructions are emitted by side effect.
; Represented as a pair whose car is a nonempty list whose cdr is a
; possibly empty list of MacScheme machine assembly instructions,
; and whose cdr is the last pair of the car.

(define (make-assembly-stream)
  (let ((stream (list (list 0))))
    (set-cdr! stream (car stream))
    stream))

(define (assembly-stream-code output)
  (if (local-optimizations)
      (filter-basic-blocks (cdar output))
      (cdar output)))

(define (gen! output . instruction)
  (let ((pair (list instruction)))
    (set-cdr! (cdr output) pair)
    (set-cdr! output pair)
    output))

(define (gen-save! output frame)
  (let ((size (cgframe-size-cell frame)))
    (let ((pair (list (cons $save size))))
      (set-cdr! (cdr output) pair)
      (set-cdr! output pair)
      output)))

(define (gen-restore! output frame)
  (let ((pair (list (cons $restore (cgframe-size-cell frame)))))
    (set-cdr! (cdr output) pair)
    (set-cdr! output pair)
    output))

(define (gen-pop! output frame)
  (let ((pair (list (cons $pop (cgframe-size-cell frame)))))
    (set-cdr! (cdr output) pair)
    (set-cdr! output pair)
    output))

; Register environments.
; Represented as an association list whose entries are of the form
;
;    (n . contents)
;
; where contents is one of
;
;    #f  (the register is dead)
;    #t  (the register is live)
;    a variable name
;      (the register is live and contains the value of that variable)
;    a Scheme expression (other than a variable) in standard form
;      (the register is live and contains the value of that expression)
;
; For the moment, the registers must be used as a stack.

(define (cgreg-initial)
  '((0 . #t)))

(define (cgreg-tos regs)
  (car (car regs)))

(define (cgreg-liveregs regs)
  (length regs))

(define (cgreg-live regs r)
  (if (eq? r 'result)
      (cgreg-tos regs)
      (max r (cgreg-tos regs))))

(define (cgreg-push1 regs)
  (cons (cons (+ (cgreg-tos regs) 1) #t) regs))

(define (cgreg-push regs n)
  (if (zero? n)
      regs
      (cgreg-push (cgreg-push1 regs) (- n 1))))

(define (cgreg-vars regs)
  (map (lambda (entry)
         (let ((contents (cdr entry)))
           (cond ((symbol? contents) contents)
                 ((and (pair? contents)
                       (variable? contents))
                  (variable.name contents))
                 (else #t))))
       (reverse regs)))

(define (cgreg-bindregs regs vars)
  (do ((regs regs (cons (cons n (car vars)) regs))
       (vars vars (cdr vars))
       (n (+ (cgreg-tos regs) 1) (+ n 1)))
      ((null? vars) regs)))

(define (cgreg-lookup regs var)
  (cond ((null? regs) #f)
        ((eq? (cdr (car regs)) var)
         (list var 'register (caar regs) '(object)))
        (else (cgreg-lookup (cdr regs) var))))

(define (cgreg-lookup-reg regs reg)
  (let* ((n (length regs)))
    (if (>= reg n)
        #t
        (cdr (list-ref regs (- n reg 1))))))

; Stack-frame environments.
; Represented as a list containing
;    number of highest slot currently allocated
;    size of frame (can be increased by side effect)

(define (cgframe-initial)
  (list -1 -1))

(define (cgframe-top frame)
  (car frame))

(define (cgframe-size-cell frame)
  (cdr frame))

(define (cgframe-size frame)
  (car (cgframe-size-cell frame)))

(define (cgframe-used! frame)
  (if (negative? (cgframe-size frame))
      (set-car! (cgframe-size-cell frame) 0)))

; Returns both a slot number and a new description of the frame.

(define (cgframe-newtemp frame)
  (let ((n (+ (cgframe-top frame) 1))
        (size (cgframe-size-cell frame)))
    (if (> n (car size))
        (set-car! size n))
    (values n (cons n size))))

(define (cgframe-newtemps frame n)
  (if (zero? n)
      (values '() frame)
      (call-with-values
       (lambda () (cgframe-newtemp frame))
       (lambda (t frame)
         (call-with-values
          (lambda () (cgframe-newtemps frame (- n 1)))
          (lambda (temps frame)
            (values (cons t temps) frame)))))))

; Environments.
;
; Each identifier has one of the following kinds of entry.
;
;    (<name> register <number> (object))
;    (<name> procedure <rib> <label> (object))
;    (<name> lexical <rib> <offset> (object))
;    (<name> integrable <arity> <op> <imm> (object))
;    (<name> global (object))
;
; Implementation.
;
; An environment is represented as a list of the form
;
;    ((<entry> ...)                          ; lexical rib
;     ...)
;
; where each <entry> has one of the forms
;
;    (<name> procedure <rib> <label> (object))
;    (<name> lexical <offset> (object))
;    (<name> integrable <arity> <op> <imm>)

(define entry.name car)
(define entry.kind cadr)
(define entry.rib caddr)
(define entry.offset cadddr)
(define entry.label cadddr)
(define entry.regnum caddr)
(define entry.arity caddr)
(define entry.op cadddr)
(define (entry.imm entry) (car (cddddr entry)))

(define (cgenv-initial integrable)
  (list (map (lambda (x)
               (list (car x)
                     'integrable
                     (cadr x)
                     (caddr x)
                     (cadddr x)
                     '((object))))
             integrable)))

(define (cgenv-lookup env id)
  (define (loop ribs m)
    (if (null? ribs)
        (cons id '(global (object)))
        (let ((x (assq id (car ribs))))
          (if x
              (case (cadr x)
                ((lexical) (cons id
                                 (cons (cadr x)
                                       (cons m (cddr x)))))
                ((procedure) (cons id
                                   (cons (cadr x)
                                         (cons m (cddr x)))))
                ((integrable) x)
                (else ???))
              (loop (cdr ribs) (+ m 1))))))
  (loop env 0))

(define (cgenv-extend env vars procs)
  (cons (do ((n 0 (+ n 1))
             (vars vars (cdr vars))
             (rib (map (lambda (id)
                         (list id 'procedure (make-label) '(object)))
                       procs)
                  (cons (list (car vars) 'lexical n '(object)) rib)))
            ((null? vars) rib))
        env))

(define (cgenv-bindprocs env procs)
  (cons (append (map (lambda (id)
                       (list id 'procedure (make-label) '(object)))
                     procs)
                (car env))
        (cdr env)))

(define (var-lookup var regs frame env)
  (or (cgreg-lookup regs var)
      (cgenv-lookup env var)))

; For testing.

(define foo
  (lambda (x)
    (pretty-print
     (pass4 (pass2 (pass1 x)) $usual-integrable-procedures$))))

(define compile
  (lambda (x)
    (pass4 (pass2 (pass1 x)) $usual-integrable-procedures$)))

; Find the smallest number of registers such that
; adding more registers does not affect the code
; generated for x (from 4 to 32 registers).

(define (minregs x)
  (define (defregs R)
    (set! *nregs* R)
    (set! *lastreg* (- *nregs* 1))
    (set! *fullregs* (quotient *nregs* 2)))
  (defregs 32)
  (let ((code (assemble (compile x))))
    (define (binary-search m1 m2)
      (if (= (+ m1 1) m2)
          m2
          (let ((midpt (quotient (+ m1 m2) 2)))
            (defregs midpt)
            (if (equal? code (assemble (compile x)))
                (binary-search m1 midpt)
                (binary-search midpt m2)))))
    (defregs 4)
    (let ((newcode (assemble (compile x))))
      (if (equal? code newcode)
          4
          (binary-search 4 32)))))

; Minimums:
;  browse     10
;  triangle    5
;  traverse   10
;  destruct    6
;  puzzle      8,8,10,7
;  tak         6
;  fft        28   (changing the named lets to macros didn't matter)
