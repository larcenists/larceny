;; Prototype based object system, with "features" like operation
;; reflection, inline documentation (well, soon to be added) and
;; delegation support.

;; This is only for the use within object extension for effecting
;; dispatch.
(define delegate-token (cons 0 0))

;; *Not* named-lambda; in particular, NAME is *not* bound in BODY ...
(define-syntax lambda-with-name
  (syntax-rules ()
    ((_ NAME (ARGS ...) BODY ...)
     (let ((proc (lambda (ARGS ...) BODY ...))) 
       (define (NAME ARGS ...) (proc ARGS ...)) NAME))
    ((_ NAME ARGL       BODY ...)
     (let ((proc (lambda ARGL BODY ...))) 
       (define (NAME . rest) (apply proc rest)) NAME))
    ))

(define-syntax make-root-object
  (syntax-rules ()
    ((root-object self ((OP-NAME . ARGS) BODY ...) ...)
     (letrec ((core-object
               ;; This use of the id 'self' is important (it is
               ;; semantically significant so that we actually
               ;; have true dynamic dispatch when one uses methods
               ;; on self within BODY)
               (lambda (op self)
                 (case op
                   ((OP-NAME) (lambda-with-name OP-NAME ARGS BODY ...))
                   ...
                   ((operations) 
                    (lambda-with-name operations () '(OP-NAME ... operations)))
                   ;; This 'self' is only for error msg documentation
                   (else (error 'self
                                ": unhandled object message " op)))))
              (self 
               ;; This 'self' is only for proc documentation; (it is
               ;; sound to alpha rename w/ sole occurrence below)
               (lambda (op)
                 ;; (display `(handling msg ,op)) (newline)
                 (if (eq? op delegate-token)
                     core-object
                     ;; Here we tie knot marrying dispatch function w/ self.
                     (core-object op self)))))
       self))))

(define-syntax msg-handler
  (syntax-rules ()
    ((msg-handler ((OP-NAME . ARGS) BODY ...) ...)
     (make-root-object self-name ((OP-NAME . ARGS) BODY ...) ...))))

(define-syntax extend-object
  (syntax-rules ()
    ((extend-object super-expr self ((OP-NAME . ARGS) BODY ...) ...)
     (letrec ((super-obj super-expr)
              (core-object 
               (lambda (op self)
                 (case op 
                   ;; See above re: this use of id 'self'
                   ((OP-NAME) (lambda-with-name OP-NAME ARGS BODY ...))
                   ...
                   ((operations) (lambda () (append '(OP-NAME ... operations)
                                                    ((super-obj 'operations)))))
                   (else ((super-obj delegate-token) op self)))))
              (self ;; See above re: this use of id 'self'
               (lambda (op)
                 (if (eq? op delegate-token)
                     core-object
                     ;; Here we tie knot marrying dispatch function w/ self.
                     (core-object op self)))))
       self))))

;; SAMPLE USAGE OF ABOVE OBJECT SYSTEM
(begin 
  (define (make-point x y) 
    (make-root-object 
     point-self
     ((x) x)
     ((y) y) 
     ((move dx dy) (make-point (+ x dx) (+ y dy)))))
  (define some-tests
    (let () 
      (make-point 1 2)                ; => #<procedure point-self>
      ((make-point 1 2) 'x)           ; => #<procedure x>
      (((make-point 1 2) 'x))         ; => 1
      (((make-point 1 2) 'y))         ; => 2
      (((make-point 1 2) 'move) 3 4)  ; => #<procedure point-self>
      (let* ((p1 (make-point 1 2))
             (p2 ((p1 'move) 5 8)))
        ((p2 'x)))                    ; => 6
      ))
  (define (make-colored-point x y col)
    (define (add-color-to-point p)
      (extend-object p 
       colored-self 
       ((color) col) 
       ((move x y) 
        ;; ugly way to get hook on super's method.  :(
        (add-color-to-point ((p 'move) x y)))
       ))
    (add-color-to-point (make-point x y)))
  (define some-more-tests
    (let ()
      (make-colored-point 10 20 'black) ; => #<interpreted-procedure colored-self>
      (let ((cp (make-colored-point 10 20 'black)))
        ((cp 'x)))                      ; => 10
      (let* ((cp1 (make-colored-point 10 20 'black))
             (cp2 ((cp1 'move) 4 7)))
        ((cp2 'color)))                 ; => black
      ))
  )
