; Generators
; 2000-07-20 / lth
;
; MAKE-GENERATOR proc
;   Returns a THUNK with local control state.  When THUNK is called,
;   then PROC is called with a procedure (call it RETURN) of zero or
;   more arguments.  If PROC calls RETURN on some values v then the 
;   v are returned from the call to THUNK.  When THUNK is called again,
;   RETURN returns to its caller.  If PROC returns, its return values
;   are returned from the call to THUNK, and it is an error to call
;   THUNK again.
;
; For example,
;  > (define g (make-generator 
;                (lambda (return)
;                  (for-each return '(1 2 3))
;                  'done)))
;  > (g) => 1
;  > (g) => 2
;  > (g) => 3
;  > (g) => done

(define (make-generator proc)
  (letrec ((next 
            (lambda ()
              (call-with-values
               (lambda () (proc return))
               (lambda results
                 (apply finish results)))))
           (return
            (lambda results
              (call-with-current-continuation
               (lambda (k)
                 (set! next (lambda () (k 'error-in-generator)))
                 (apply finish results)))))
           (finish 
            (lambda results #f)))
    (lambda ()
      (call-with-current-continuation
       (lambda (k)
         (set! finish k)
         (next))))))

(define (generators-for-each f done? . generators)
  (let loop ()
    (let ((xs (map (lambda (x) (x)) generators)))
      (if (not (done? (car xs)))
          (begin (apply f xs)
                 (loop))))))

(define (generators-map f done? . generators)
  (let loop ()
    (let ((xs (map (lambda (x) (x)) generators)))
      (if (not (done? (car xs)))
          (cons (apply f xs)
                (loop))
          '()))))

; Common idiom.
; Avoid using the generators in this case -- this is faster.

(define (generate-list xs sentinel)
  (lambda ()
    (if (null? xs)
        sentinel
        (let ((x (car xs)))
          (set! xs (cdr xs))
          x))))

; eof
