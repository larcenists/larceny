; Test code for lib/defmacro.sch
; 2000-05-21 / lth

(require 'defmacro)

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(defmacro dmtest1 ()
  33)

(or (equal? (dmtest1) 33)
    (fail 'defmacro:1))

(defmacro dmtest2 (a b c)
  `'(,a ,b ,c ,a ,b ,c))

(or (equal? (dmtest2 1 2 3) '(1 2 3 1 2 3))
    (fail 'defmacro:2))

(defmacro dmtest3 (body1 . bodymore)    ; PROG1
  (let ((n (gensym "G")))
    `(let ((,n ,body1))
       (begin ,@bodymore ,n))))

(or (equal? (dmtest3 (+ 1 2) 4 8) 3)
    (fail 'defmacro:3))

(defmacro dmtest4 all
  `(begin ,@all))

(or (equal? (dmtest4 1 2 3) 3)
    (fail 'defmacro:4))
