; Hashtable of primop types for the beginning student language.
;
; FIXME: type processing isn't implemented yet.
; FIXME: value processing isn't implemented yet.
; FIXME: a few primops aren't implemented yet.



(define beginning-primop-types
  (make-hashtable symbol-hash eq?))

(define beginning-primop-procs
  (make-hashtable symbol-hash eq?))

; FIXME

(define (primop-lookup op)
  (hashtable-ref beginning-primop-procs op op))

(define (beginning-primop-value/type: name val type)
  (hashtable-set! beginning-primop-types
                  name
                  type)
  (hashtable-set! beginning-primop-procs
                  name
                  (make-beginning-primop name val)))


(beginning-primop-value/type: '* * '(num num num ... -> num))
(beginning-primop-value/type: '+ + '(num num num ... -> num))
(beginning-primop-value/type: '- - '(num num ... -> num))
(beginning-primop-value/type: '/ / '(num num num ... -> num))
(beginning-primop-value/type: '< < '(real real real ... -> boolean))
(beginning-primop-value/type: '<= <= '(real real real ... -> boolean))
(beginning-primop-value/type: '= = '(num num num ... -> boolean))
(beginning-primop-value/type: '> > '(real real real ... -> boolean))
(beginning-primop-value/type: '>= >= '(real real ... -> boolean))
(beginning-primop-value/type: 'abs abs '(real -> real))
(beginning-primop-value/type: 'acos acos '(num -> num))
(beginning-primop-value/type: 'add1
                              (lambda (z) (+ z 1))
                              '(number -> number))
(beginning-primop-value/type: 'angle angle '(num -> real))
(beginning-primop-value/type: 'asin asin '(num -> num))
(beginning-primop-value/type: 'atan atan '(num -> num))
(beginning-primop-value/type: 'ceiling ceiling '(real -> int))
(beginning-primop-value/type: 'complex? complex? '(any -> bool))
(beginning-primop-value/type: 'conjugate
                              (lambda (z)
                                (make-rectangular (real-part z)
                                                  (- (imag-part z))))
                              '(num -> num))
(beginning-primop-value/type: 'cos cos '(num -> num))
;(beginning-primop-value/type: 'cosh cosh '(num -> num))
(beginning-primop-value/type: 'current-seconds current-seconds '(-> int))
(beginning-primop-value/type: 'denominator denominator '(rat -> int))
(beginning-primop-value/type: 'e
                              (exp 1)
                              'real)
(beginning-primop-value/type: 'even? even? '(integer -> bool))
(beginning-primop-value/type: 'exact->inexact exact->inexact '(num -> num))
(beginning-primop-value/type: 'exact? exact? '(num -> bool))
(beginning-primop-value/type: 'exp exp '(num -> num))
(beginning-primop-value/type: 'expt expt '(num num -> num))
(beginning-primop-value/type: 'floor floor '(real -> int))
(beginning-primop-value/type: 'gcd gcd '(int int ... -> int))
(beginning-primop-value/type: 'imag-part imag-part '(num -> real))
(beginning-primop-value/type: 'inexact->exact inexact->exact '(num -> num))
(beginning-primop-value/type: 'inexact? inexact? '(num -> bool))
(beginning-primop-value/type: 'integer->char integer->char '(int -> char))
(beginning-primop-value/type: 'integer? integer? '(any -> bool))
(beginning-primop-value/type: 'lcm lcm '(int int ... -> int))
(beginning-primop-value/type: 'log log '(num -> num))
(beginning-primop-value/type: 'magnitude magnitude '(num -> real))
(beginning-primop-value/type: 'make-polar make-polar '(real real -> num))
(beginning-primop-value/type: 'max max '(real real ... -> real))
(beginning-primop-value/type: 'min min '(real real ... -> real))
(beginning-primop-value/type: 'modulo modulo '(int int -> int))
(beginning-primop-value/type: 'negative? negative? '(number -> bool))
(beginning-primop-value/type: 'number->string number->string '(num -> string))
(beginning-primop-value/type: 'number? number? '(any -> boolean))
(beginning-primop-value/type: 'numerator numerator '(rat -> int))
(beginning-primop-value/type: 'odd? odd? '(integer -> bool))
(beginning-primop-value/type: 'pi
                              (acos -1)
                              'real)
(beginning-primop-value/type: 'positive? positive? '(number -> bool))
(beginning-primop-value/type: 'quotient quotient '(int int -> int))
(beginning-primop-value/type: 'random
                              random
                              '(int -> int))
(beginning-primop-value/type: 'rational? rational? '(any -> bool))
(beginning-primop-value/type: 'real-part real-part '(num -> real))
(beginning-primop-value/type: 'real? real? '(any -> bool))
(beginning-primop-value/type: 'remainder remainder '(int int -> int))
(beginning-primop-value/type: 'round round '(real -> int))
(beginning-primop-value/type: 'sgn
                              (lambda (x)
                                (define (adjust n)
                                  (if (exact? x)
                                      n
                                      (exact->inexact n)))
                                (cond ((< x 0)
                                       (adjust -1))
                                      ((= x 0)
                                       (adjust 0))
                                      ((> x 0)
                                       (adjust +1))
                                      (else
                                       x))) ;FIXME
                              '(real -> (union 1 #i1.0 0 #i0.0 -1 #i-1.0)))
(beginning-primop-value/type: 'sin sin '(num -> num))
;(beginning-primop-value/type: 'sinh sinh '(num -> num))
(beginning-primop-value/type: 'sqr
                              (lambda (z) (* z z))
                              '(num -> num))
(beginning-primop-value/type: 'sqrt sqrt '(num -> num))
(beginning-primop-value/type: 'sub1
                              (lambda (z) (- z 1))
                              '(number -> number))
(beginning-primop-value/type: 'tan tan '(num -> num))
(beginning-primop-value/type: 'zero? zero? '(number -> bool))

(beginning-primop-value/type: 'boolean=? boolean=? '(boolean boolean -> boolean))
(beginning-primop-value/type: 'boolean? boolean? '(any -> boolean))
(beginning-primop-value/type: 'not not '(boolean -> boolean))

(beginning-primop-value/type: 'symbol->string symbol->string '(symbol -> string))
(beginning-primop-value/type: 'symbol=? symbol=? '(symbol symbol -> boolean))
(beginning-primop-value/type: 'symbol? symbol? '(any -> boolean))

(beginning-primop-value/type: 'append append '((listof any) ... -> (listof any)))
(beginning-primop-value/type: 'assq assq '(X (listof (cons X Y)) -> (union false (cons X Y))))
(beginning-primop-value/type: 'caaar caaar '((cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> W))
(beginning-primop-value/type: 'caadr caadr '((cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> (listof Z)))
(beginning-primop-value/type: 'caar caar '((cons (cons Z (listof Y)) (listof X)) -> Z))
(beginning-primop-value/type: 'cadar cadar '((cons (cons W (cons Z (listof Y))) (listof X)) -> Z))
(beginning-primop-value/type: 'cadddr cadddr '((listof Y) -> Y))
(beginning-primop-value/type: 'caddr caddr '((cons W (cons Z (cons Y (listof X)))) -> Y))
(beginning-primop-value/type: 'cadr cadr '((cons Z (cons Y (listof X))) -> Y))
(beginning-primop-value/type: 'car car '((cons Y (listof X)) -> Y))
(beginning-primop-value/type: 'cdaar cdaar '((cons (cons (cons W (listof Z)) (listof Y)) (listof X)) -> (listof Z)))
(beginning-primop-value/type: 'cdadr cdadr '((cons W (cons (cons Z (listof Y)) (listof X))) -> (listof Y)))
(beginning-primop-value/type: 'cdar cdar '((cons (cons Z (listof Y)) (listof X)) -> (listof Y)))
(beginning-primop-value/type: 'cddar cddar '((cons (cons W (cons Z (listof Y))) (listof X)) -> (listof Y)))
(beginning-primop-value/type: 'cdddr cdddr '((cons W (cons Z (cons Y (listof X)))) -> (listof X)))
(beginning-primop-value/type: 'cddr cddr '((cons Z (cons Y (listof X))) -> (listof X)))
(beginning-primop-value/type: 'cdr cdr '((cons Y (listof X)) -> (listof X)))
(beginning-primop-value/type: 'cons cons '(X (listof X) -> (listof X)))
(beginning-primop-value/type: 'cons?
                              pair?
                              '(any -> boolean))
(beginning-primop-value/type: 'eighth
                              (lambda (y) (list-ref y 7))
                              '((listof Y) -> Y))
(beginning-primop-value/type: 'empty?
                              null?
                              '(any -> boolean))
(beginning-primop-value/type: 'fifth
                              (lambda (y) (list-ref y 4))
                              '((listof Y) -> Y))
(beginning-primop-value/type: 'first
                              car
                              '((cons Y (listof X)) -> Y))
(beginning-primop-value/type: 'fourth
                              cadddr
                              '((listof Y) -> Y))
(beginning-primop-value/type: 'length length '(list -> number))
;(beginning-primop-value/type: 'list list '(any ... (listof any) -> (listof any)))
(beginning-primop-value/type: 'list list '(any ... -> (listof any)))
(beginning-primop-value/type: 'list* list* '(any ... (listof any) -> (listof any)))
(beginning-primop-value/type: 'list-ref list-ref '((listof X) natural-number -> X))
(beginning-primop-value/type: 'member member '(any list -> (union false list)))
(beginning-primop-value/type: 'memq memq '(any list -> (union false list)))
(beginning-primop-value/type: 'memv memv '(any list -> (union false list)))
(beginning-primop-value/type: 'null
                              (list)
                              'empty)
(beginning-primop-value/type: 'null? null? '(any -> boolean))
(beginning-primop-value/type: 'pair? pair? '(any -> boolean))
(beginning-primop-value/type: 'rest
                              cdr
                              '((cons Y (listof X)) -> (listof X)))
(beginning-primop-value/type: 'reverse reverse '(list -> list))
(beginning-primop-value/type: 'second
                              cadr
                              '((cons Z (cons Y (listof X))) -> Y))
(beginning-primop-value/type: 'seventh
                              (lambda (y) (list-ref y 6))
                              '((listof Y) -> Y))
(beginning-primop-value/type: 'sixth
                              (lambda (y) (list-ref y 5))
                              '((listof Y) -> Y))
(beginning-primop-value/type: 'third
                              caddr
                              '((cons W (cons Z (cons Y (listof X)))) -> Y))

(beginning-primop-value/type: 'make-posn
                              make-posn
                              '(number number -> posn))
(beginning-primop-value/type: 'posn-x
                              posn-x
                              '(posn -> number))
(beginning-primop-value/type: 'posn-y
                              posn-y
                              '(posn -> number))
(beginning-primop-value/type: 'posn?
                              posn?
                              '(anything -> boolean))

(beginning-primop-value/type: 'char->integer char->integer '(char -> integer))
(beginning-primop-value/type: 'char-alphabetic? char-alphabetic? '(char -> boolean))
(beginning-primop-value/type: 'char-ci<=? char-ci<=? '(char char ... -> boolean))
(beginning-primop-value/type: 'char-ci<? char-ci<? '(char char ... -> boolean))
(beginning-primop-value/type: 'char-ci=? char-ci=? '(char char ... -> boolean))
(beginning-primop-value/type: 'char-ci>=? char-ci>=? '(char char ... -> boolean))
(beginning-primop-value/type: 'char-ci>? char-ci>? '(char char ... -> boolean))
(beginning-primop-value/type: 'char-downcase char-downcase '(char -> char))
(beginning-primop-value/type: 'char-lower-case? char-lower-case? '(char -> boolean))
(beginning-primop-value/type: 'char-numeric? char-numeric? '(char -> boolean))
(beginning-primop-value/type: 'char-upcase char-upcase '(char -> char))
(beginning-primop-value/type: 'char-upper-case? char-upper-case? '(char -> boolean))
(beginning-primop-value/type: 'char-whitespace? char-whitespace? '(char -> boolean))
(beginning-primop-value/type: 'char<=? char<=? '(char char ... -> boolean))
(beginning-primop-value/type: 'char<? char<? '(char char ... -> boolean))
(beginning-primop-value/type: 'char=? char=? '(char char ... -> boolean))
(beginning-primop-value/type: 'char>=? char>=? '(char char ... -> boolean))
(beginning-primop-value/type: 'char>? char>? '(char char ... -> boolean))
(beginning-primop-value/type: 'char? char? '(any -> boolean))

;(beginning-primop-value/type: 'format format '(string any ... -> string))
(beginning-primop-value/type: 'list->string list->string '((listof char) -> string))
(beginning-primop-value/type: 'make-string make-string '(nat char -> string))
(beginning-primop-value/type: 'string string '(char ... -> string))
(beginning-primop-value/type: 'string->list string->list '(string -> (listof char)))
(beginning-primop-value/type: 'string->number string->number '(string -> (union number false)))
(beginning-primop-value/type: 'string->symbol string->symbol '(string -> symbol))
(beginning-primop-value/type: 'string-append string-append '(string ... -> string))
(beginning-primop-value/type: 'string-ci<=? string-ci<=? '(string string ... -> boolean))
(beginning-primop-value/type: 'string-ci<? string-ci<? '(string string ... -> boolean))
(beginning-primop-value/type: 'string-ci=? string-ci=? '(string string ... -> boolean))
(beginning-primop-value/type: 'string-ci>=? string-ci>=? '(string string ... -> boolean))
(beginning-primop-value/type: 'string-ci>? string-ci>? '(string string ... -> boolean))
(beginning-primop-value/type: 'string-copy string-copy '(string -> string))
(beginning-primop-value/type: 'string-length string-length '(string -> nat))
(beginning-primop-value/type: 'string-ref string-ref '(string nat -> char))
(beginning-primop-value/type: 'string<=? string<=? '(string string ... -> boolean))
(beginning-primop-value/type: 'string<? string<? '(string string ... -> boolean))
(beginning-primop-value/type: 'string=? string=? '(string string ... -> boolean))
(beginning-primop-value/type: 'string>=? string>=? '(string string ... -> boolean))
(beginning-primop-value/type: 'string>? string>? '(string string ... -> boolean))
(beginning-primop-value/type: 'string? string? '(any -> boolean))
(beginning-primop-value/type: 'substring substring '(string nat nat -> string))

;(beginning-primop-value/type: 'image=? image=? '(image image -> boolean))
;(beginning-primop-value/type: 'image? image? '(any -> boolean))

;(beginning-primop-value/type: '=~ '(real real non-negative-real -> boolean))
(beginning-primop-value/type: 'eof
                              (eof-object)
                              'eof)
(beginning-primop-value/type: 'eof-object? eof-object? '(any -> boolean))
(beginning-primop-value/type: 'eq? eq? '(any any -> boolean))
(beginning-primop-value/type: 'equal? equal? '(any any -> boolean))
;(beginning-primop-value/type: 'equal~? equal~? '(any any non-negative-real -> boolean))
(beginning-primop-value/type: 'eqv? eqv? '(any any -> boolean))
(beginning-primop-value/type: 'error error '(symbol string -> void))
(beginning-primop-value/type: 'exit exit '(-> void))
(beginning-primop-value/type: 'identity
                              (lambda (obj) obj)
                              '(any -> any))
(beginning-primop-value/type: 'struct?
                              record?
                              '(any -> boolean))
