; Test cases for case-lambda.
;
; A case-lambda of several clauses dispatches on the number of
; arguments to its clauses, which are ordinary procedures (see
; .make-case-lambda in src/Lib/Common/make-case-lambda.sch).

(define (run-case-lambda-tests)

  (define (wrong-number? thunk)
    (call-with-current-continuation
     (lambda (k)
       (call-with-error-handler
        (lambda args (k #t))
        (lambda () (thunk) #f)))))

  ; More arguments than there are hardware registers; the rest are
  ; passed in a list.

  (define many
    (case-lambda
     ((a) (list 'one a))
     ((a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20)
      (list 'twenty a1 a10 a16 a17 a20))
     ((a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 . r)
      (list 'rest a1 a17 a18 r))))

  (define order
    (case-lambda
     ((a . r) (list 'rest a r))
     ((a b) (list 'two a b))
     (() 'none)))

  (define only-rest
    (case-lambda
     ((a b) (list 'two a b))
     (args (cons 'args args))))

  (define (counter)
    (let ((n 0))
      (case-lambda
       (() n)
       ((k) (set! n (+ n k)) n)
       ((k j) (set! n (+ n k j)) n))))

  ; A loop of tail calls through the dispatcher must not grow the stack.

  (define loop
    (case-lambda
     ((n) (loop n 0))
     ((n acc) (if (= n 0) acc (loop (- n 1) (+ acc 1))))))

  (display "Case-lambda") (newline)

  (allof "case-lambda"

   (test "fixed arities"
         (let ((f (case-lambda
                   (() 0)
                   ((a) (list a))
                   ((a b) (list a b))
                   ((a b c) (list a b c))
                   ((a b c d e) (list a b c d e)))))
           (list (f) (f 1) (f 1 2) (f 1 2 3) (f 1 2 3 4 5)))
         '(0 (1) (1 2) (1 2 3) (1 2 3 4 5)))

   (test "rest clauses"
         (let ((f (case-lambda
                   ((a) (list 'one a))
                   ((a b . r) (list 'more a b r)))))
           (list (f 1) (f 1 2) (f 1 2 3 4)))
         '((one 1) (more 1 2 ()) (more 1 2 (3 4))))

   (test "the first clause that accepts the arguments"
         (list (order) (order 1) (order 1 2) (order 1 2 3))
         '(none (rest 1 ()) (rest 1 (2)) (rest 1 (2 3))))

   (test "a clause of only a rest list"
         (list (only-rest) (only-rest 1) (only-rest 1 2) (only-rest 1 2 3))
         '((args) (args 1) (two 1 2) (args 1 2 3)))

   (test "a single clause"
         ((case-lambda ((a b) (+ a b))) 3 4)
         7)

   (test "many arguments"
         (list (many 1)
               (many 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
               (many 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)
               (many 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
               (many 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21))
         '((one 1)
           (twenty 1 10 16 17 20)
           (rest 1 17 18 ())
           (rest 1 17 18 (19))
           (rest 1 17 18 (19 20 21))))

   (test "closures over assigned variables"
         (let ((c (counter)))
           (c 1)
           (c 2 3)
           (list (c) (c 10)))
         '(6 16))

   (test "apply"
         (list (apply order '(1 2))
               (apply many '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))
         '((rest 1 (2)) (twenty 0 9 15 16 19)))

   (test "tail calls"
         (loop 1000000)
         1000000)

   (test "call/cc in a clause"
         (let ((f (case-lambda
                   ((k) (k 'escaped) 'not)
                   ((k x) x))))
           (list (call-with-current-continuation f)
                 (call-with-current-continuation (lambda (k) (f k 2)))))
         '(escaped 2))

   ; The dispatch must leave no false pointer in a register that the
   ; garbage collector scans when a clause allocates.

   (test "allocation in clauses"
         (let ((f (case-lambda
                   ((a) (cons a a))
                   ((a b) (cons a b))
                   ((a b c) (cons b c)))))
           (let loop ((i 0) (acc '()))
             (if (= i 3000000)
                 (length acc)
                 (loop (+ i 1) (if (pair? acc) (f 1 2 '()) (f 1 2 acc))))))
         1)

   (test "procedure?"
         (procedure? (case-lambda ((a) a) ((a b) b)))
         #t)

   (test "procedure-name"
         (let ()
           (define named (case-lambda ((a) a) ((a b) b)))
           (list (procedure-name named)
                 (procedure-name order)
                 (procedure-name string-copy!)))
         '(named order string-copy!))

   (test "no clause accepts the arguments"
         (let ((f (case-lambda ((a) a) ((a b c) c))))
           (list (wrong-number? (lambda () (f)))
                 (wrong-number? (lambda () (f 1 2)))
                 (wrong-number? (lambda () (f 1 2 3 4)))
                 (wrong-number? (lambda () (f 1 2 3)))))
         '(#t #t #t #f))

   (test "eval"
         ((eval '(case-lambda ((a) (list a)) ((a b) (list b a)))
                (interaction-environment))
          1 2)
         '(2 1))

   ; Library procedures with optional arguments, made with case-lambda.

   (test "string-copy, string-copy!"
         (let ((s (lambda () (string-copy "abcdef"))))
           (list (string-copy "abcdef" 2)
                 (string-copy "abcdef" 2 4)
                 (let ((d (s))) (string-copy! d 1 "xyz") d)
                 (let ((d (s))) (string-copy! d 1 "xyz" 1) d)
                 (let ((d (s))) (string-copy! d 1 "xyz" 1 2) d)
                 (let ((d (s))) (string-copy! d 2 d 0 4) d)
                 (let ((d (s))) (string-copy! d 0 d 2) d)
                 (wrong-number? (lambda () (string-copy "abc" 0 1 2)))))
         '("cdef" "cd" "axyzef" "ayzdef" "aycdef" "ababcd" "cdefef" #t))

   (test "vector-copy, vector-copy!"
         (let ((v (lambda () (vector-copy '#(a b c d e f)))))
           (list (vector-copy '#(a b c d e f) 2)
                 (vector-copy '#(a b c d e f) 2 4)
                 (let ((d (v))) (vector-copy! d 1 '#(x y z)) d)
                 (let ((d (v))) (vector-copy! d 1 '#(x y z) 1) d)
                 (let ((d (v))) (vector-copy! d 1 '#(x y z) 1 2) d)
                 (let ((d (v))) (vector-copy! d 2 d 0 4) d)
                 (wrong-number? (lambda () (vector-copy 'a)))))
         '(#(c d e f) #(c d) #(a x y z e f) #(a y z d e f) #(a y c d e f)
           #(a b a b c d) #t))

   (test "bytevector-copy, r7rs:bytevector-copy!"
         (let ((b (lambda () (u8-list->bytevector '(0 1 2 3 4 5)))))
           (map bytevector->u8-list
                (list (bytevector-copy (b) 2)
                      (bytevector-copy (b) 2 4)
                      (let ((d (b)))
                        (r7rs:bytevector-copy! d 1 (b))
                        d)
                      (let ((d (b)))
                        (r7rs:bytevector-copy! d 1 (b) 3)
                        d)
                      (let ((d (b)))
                        (r7rs:bytevector-copy! d 1 (b) 3 4)
                        d))))
         '((2 3 4 5) (2 3) (0 0 1 2 3 4) (0 3 4 5 4 5) (0 3 2 3 4 5)))

   (test "output procedures with and without a port"
         (let* ((q (open-output-string))
                (r (open-output-string))
                (out (lambda (p)
                       (display "d" p) (write "w" p) (newline p)
                       (write-string "abcdef" p 2) (write-string "abcdef" p 1 3)
                       (write-shared '(1) p) (write-simple 'x p)
                       (display-simple #\c p) (flush-output-port p)))
                (out0 (lambda ()
                        (display "d") (write "w") (newline)
                        (write-string "abcdef") (write-shared '(1))
                        (write-simple 'x) (display-simple #\c)
                        (flush-output-port))))
           (out q)
           (parameterize ((current-output-port r)) (out0))
           (list (get-output-string q) (get-output-string r)))
         '("d\"w\"\ncdefbc(1)xc" "d\"w\"\nabcdef(1)xc"))

   (test "input procedures with and without a port"
         (let ((p (open-input-string "ab\ncdefg"))
               (q (open-input-string "xy\nz")))
           (list (peek-char p) (char-ready? p) (read-line p)
                 (read-string 2 p)
                 (parameterize ((current-input-port q))
                   (list (peek-char) (char-ready?) (read-line)
                         (read-string 1)))))
         '(#\a #t "ab" "cd" (#\x #t "xy" "z")))

   (test "binary port procedures with and without a port"
         (let ((in (lambda ()
                     (open-input-bytevector (u8-list->bytevector
                                             '(1 2 3 4 5 6 7 8)))))
               (b (make-bytevector 4 0))
               (out (open-output-bytevector)))
           (list (let ((p (in)))
                   (list (read-u8 p) (peek-u8 p) (u8-ready? p)
                         (bytevector->u8-list (read-bytevector 2 p))
                         (read-bytevector! b p 1 3)
                         (bytevector->u8-list b)
                         (read-bytevector! b p)
                         (bytevector->u8-list b)))
                 (parameterize ((current-input-port (in)))
                   (list (read-u8) (peek-u8) (u8-ready?)
                         (bytevector->u8-list (read-bytevector 1))
                         (read-bytevector! b)))
                 (begin
                   (write-u8 9 out)
                   (write-bytevector (u8-list->bytevector '(1 2 3)) out 1)
                   (write-bytevector (u8-list->bytevector '(1 2 3)) out 0 1)
                   (parameterize ((current-output-port out))
                     (write-u8 7)
                     (write-bytevector (u8-list->bytevector '(5))))
                   (bytevector->u8-list (get-output-bytevector out)))))
         '((1 2 #t (2 3) 2 (0 4 5 0) 3 (6 7 8 0))
           (1 2 #t (2) 4)
           (9 2 3 1 7 5)))

   (test "put-string, put-bytevector"
         (list (call-with-string-output-port
                (lambda (p)
                  (put-string p "abcdef")
                  (put-string p "abcdef" 4)
                  (put-string p "abcdef" 1 2)))
               (call-with-values
                (lambda () (open-bytevector-output-port))
                (lambda (p get)
                  (let ((b (u8-list->bytevector '(1 2 3 4))))
                    (put-bytevector p b)
                    (put-bytevector p b 3)
                    (put-bytevector p b 1 2)
                    (bytevector->u8-list (get))))))
         '("abcdefefbc" (1 2 3 4 4 2 3)))

   (test "list procedures"
         (list (make-list 2 'x) (length (make-list 3))
               (member 2.0 '(1 2 3) =) (member "b" '("a" "b"))
               (assoc 2.0 '((1 . a) (2 . b)) =) (assoc "b" '(("b" . 1))))
         '((x x) 3 (2 3) ("b") (2 . b) ("b" . 1)))

   (test "string and vector conversions"
         (list (vector->list '#(a b c d)) (vector->list '#(a b c d) 2)
               (vector->list '#(a b c d) 1 3)
               (vector->string '#(#\a #\b #\c) 1)
               (string->vector "abc" 1 2)
               (string->list "abcd" 2) (string->list "abcd" 1 2)
               (let ((v (make-vector 4 0))) (vector-fill! v 1 2) v)
               (let ((v (make-vector 4 0))) (vector-fill! v 1 1 3) v)
               (let ((s (make-string 4 #\-))) (string-fill! s #\x 3) s)
               (let ((s (make-string 4 #\-))) (string-fill! s #\x) s))
         '((a b c d) (c d) (b c) "bc" #(#\b) (#\c #\d) (#\b)
           #(0 0 1 1) #(0 1 1 0) "---x" "xxxx"))

   (test "string->utf8, utf8->string"
         (list (bytevector->u8-list (string->utf8 "abc" 1))
               (bytevector->u8-list (string->utf8 "abc" 1 2))
               (utf8->string (string->utf8 "abc") 1)
               (utf8->string (string->utf8 "abc") 0 2))
         '((98 99) (98) "bc" "ab"))

   (test "number procedures"
         (list (number->string 255 16) (number->string 7)
               (string->number "ff" 16) (string->number "17")
               (atan 1 1) (atan 0)
               (log 8 2) (wrong-number? (lambda () (log 8 'x))))
         (list "ff" "7" 255 17 (atan 1.0 1.0) 0.0 (/ (log 8) (log 2)) #t))

   (test "vector-sort, vector-sort!"
         (list (vector-sort < '#(3 1 2)) (vector-sort < '#(3 1 2 0) 1)
               (vector-sort < '#(3 1 2 0) 1 3)
               (let ((v (vector 3 1 2 0))) (vector-sort! < v) v)
               (let ((v (vector 3 1 2 0))) (vector-sort! < v 1) v)
               (let ((v (vector 3 1 2 0))) (vector-sort! < v 0 2) v))
         '(#(1 2 3) #(0 1 2) #(1 2) #(0 1 2 3) #(3 0 1 2) #(1 3 2 0)))))

; eof
