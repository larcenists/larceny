;;; Test suite for SRFI 51.
;;;
;;; $Id$

(cond-expand (srfi-51))

(define (writeln . xs)
  (for-each display xs)
  (newline))

(define (fail token . more)
  (writeln "Error: test failed: " token)
  #f)

;;; Returns true iff calling thunk causes an error.

(define (mustfail thunk)
  (let ((eh #f))
    (if (call-with-current-continuation
         (lambda (return)
           (dynamic-wind
            (lambda () 
              (set! eh (error-handler))
              (error-handler (lambda args (return #f))))
            (lambda () 
              (thunk)
              #t)
            (lambda () 
              (error-handler eh)))))
        #f
        #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (caller . args) #t)

(define rest-list '(x 1))

(or (equal? (call-with-values (lambda () (rest-values rest-list)) list)
            '(x 1))
    (fail 'rest-values1))

(or (equal? (call-with-values (lambda () (rest-values rest-list 2)) list)
            '(x 1))
    (fail 'rest-values2))

(or (equal? (call-with-values (lambda () (rest-values caller rest-list)) list)
            '(x 1))
    (fail 'rest-values3))

(or (equal? (call-with-values (lambda () (rest-values caller rest-list -3))
                              list)
            '(x 1))
    (fail 'rest-values4))

; error too many defaults (y 3 1) default-list (<= (length default-list) 2)

(or (mustfail (lambda () (rest-values rest-list -2 'y 3 1)))
    (fail 'rest-values5))

; error too many arguments (x 1) rest-list (<= (length rest-list) 1) caller

(or (mustfail (lambda () (rest-values 'caller rest-list 1 '(x y z))))
    (fail 'rest-values6))

; error incorrect argument 1 arg (<procedure string?> arg) <procedure caller>

(or (mustfail (lambda ()
                (rest-values caller rest-list 2
                             (list 'x 'y 'z) (cons "str" string?))))
    (fail 'rest-values7))

; error unmatched argument x arg (member arg (y z))

(or (mustfail (lambda () (rest-values rest-list 2 '(y z) `(100 . ,number?))))
    (fail 'rest-values8))

; error caller: bad argument x arg (member arg (y z))

(or (mustfail (lambda () (rest-values "caller: bad argument"
                                      rest-list 2 '(y z) `(100 . ,number?))))
    (fail 'rest-values9))

; error bad optional argument (x y) option
; (or (boolean? option) (integer? option) (memq option (list + -))) caller

(or (mustfail (lambda () (rest-values 'caller
                                      rest-list
                                      (list 'x 'y) (cons 1 number?))))
    (fail 'rest-values10))

(or (equal? (call-with-values
             (lambda () (rest-values rest-list - 'y 100 "str"))
             list)
            '(x 1 "str"))
    (fail 'rest-values11))

(or (equal? (call-with-values
             (lambda ()
               (rest-values rest-list +
                            `(x y z) `(100 . ,number?) `("str" . ,string?)))
             list)
            '(x 1 "str"))
    (fail 'rest-values12))

(or (equal? (call-with-values
             (lambda ()
              (rest-values rest-list #t `(x y z)
                           `(100 . ,number?) `("str" . ,string?)))
             list)
            '(x 1 "str"))
    (fail 'rest-values13))

(or (equal? (call-with-values
             (lambda ()
               (rest-values rest-list #t `(100 . ,number?)
               `("str" . ,string?) `(x y z)))
             list)
            '(1 "str" x))
    (fail 'rest-values14))

; error bad argument (x) rest-list (null? rest-list)

(or (mustfail (lambda ()
                (rest-values rest-list #t `(100 . ,number?)
                             `("str" . ,string?) `(y z))))
    (fail 'rest-values15))

(or (equal? (call-with-values
             (lambda ()
               (rest-values rest-list #f `(100 . ,number?)
                            `("str" . ,string?) `(y z)))
             list)
            '(1 "str" y x))
    (fail 'rest-values16))

(define str "string")
(define num 2)

; error incorrect argument 2 num (< num 2)

(or (mustfail (lambda () (arg-and num (number? num) (< num 2))))
    (fail 'arg-and1))

; error incorrect argument 2 num (< num 2) <procedure caller>

(or (mustfail (lambda () (arg-and caller num (number? num) (< num 2))))
    (fail 'arg-and2))

; error incorrect argument 2 num (< num 2) caller

(or (mustfail (lambda () (arg-and 'caller num (number? num) (< num 2))))
    (fail 'arg-and3))

; error caller: bad argument 2 num (< num 2)

(or (mustfail (lambda ()
                (arg-and "caller: bad argument" num (number? num) (< num 2))))
    (fail 'arg-and4))

; error caller: bad argument 2 num (< num 2)

(or (mustfail (lambda ()
                (arg-ands (str (string? str) (< (string-length str) 7))
                          ("caller: bad argument" num (number? num)
                           (< num 2)))))
    (fail 'arg-ands1))

; error incorrect argument 2 num (< num 2)

(or (mustfail (lambda ()
                (arg-ands ("caller: bad argument" str (string? str)
                           (< (string-length str) 7))
                          (num (number? num) (< num 2)))))
    (fail 'arg-ands2))

; error incorrect argument 2 num (< num 2) caller

(or (mustfail (lambda ()
                (arg-ands common 'caller
                          (str (string? str) (< (string-length str) 7))
                          (num (number? num) (< num 2)))))
    (fail 'arg-ands3))

; error caller: incorrect argument 2 num (< num 2)

(or (mustfail (lambda ()
                (arg-ands common "caller: bad argument"
                          (str (string? str) (< (string-length str) 7))
                          ("caller: incorrect argument"
                           num (number? num) (< num 2)))))
    (fail 'arg-ands4))

; error false expression (< num 2) caller

(or (mustfail (lambda ()
                (err-and 'caller
                         (string? str)
                         (< (string-length str) 7) (number? num) (< num 2))))
    (fail 'err-and1))

; error num failed test in caller (< num 2)

(or (mustfail (lambda ()
                (err-ands (caller (string? str) (< (string-length str) 7))
                          ("num failed test in caller"
                           (number? num)
                           (< num 2)))))
    (fail 'err-ands1))

'
(define (read-line . p-d)
  ;; p-d should be (<input-port> <symbol>).
  (receive (p d) (rest-values p-d 2
                              (cons (current-input-port) input-port?)
                              (list 'trim 'concat 'split...))
    ...))
'
(define (read-line . p-d)
  (receive (p d) (rest-values p-d -2 (current-input-port) 'trim)
    (arg-ands (p (input-port? p))
              (d (memq d '(trim concat split...))))
    ...))
'
(define (read-line . p-d)
  ;; p-d can be (<input-port> <symbol>) or (<symbol> <input-port>).
  (receive (p d) (rest-values p-d #t
                              (cons (current-input-port) input-port?)
                              (list 'trim 'concat 'split...))
    ...))
'
(define (delete x ls . predicate)
  (let ((pred (rest-values 'delete predicate 1 (list equal? eqv? eq?))))
    ...))
'
(define (delete x ls . predicate)
  (let ((pred (rest-values 'delete predicate -1 equal?)))
    (err-and 'delete (list? ls) (memq pred (list equal? eqv? eq?)))
    ...))
'
(define (substring str . start-end)
  (let ((str-len (arg-and substring str (string? str) (string-length str))))
    (receive (start end) (rest-values substring start-end -2 0 str-len)
      (arg-ands common substring
                (start (integer? start) (<= 0 start str-len))
                (end (integer? end) (<= start end str-len)))
      ...)))
'
(define (procedure-with-sequential-binding-arguments . a-b-c)
  (receive (a b c) (rest-values a-b-c -3 10 #f #f)
    (let* ((b (or b (+ a 10)))
           (c (or c (+ a b))))
      ...)))

(writeln "Done.")
