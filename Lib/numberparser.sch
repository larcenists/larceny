; Copyright Lightship Software.
;
; $Id: numberparser.sch,v 1.5 1992/08/04 18:27:32 lth Exp $
;
;                                      16 April 1990
;
; A parser for numeric constants in MacScheme.
; Designed to be called by the reader.
; Captures a procedure named bellerophon, which should implement
; Algorithm Bellerophon for reading floating point numbers perfectly.
;
; MacScheme supports neither complex numbers nor the complex
; number syntax.
;
; Number syntax for MacScheme
; 
; <number>  -->  <num 2>  |  <num 8>  |  <num 10>  |  <num 16>
; 
; The following rules for <num R>, <complex R>, <real R>, <ureal R>,
; <uinteger R>, and <prefix R> should be replicated for R = 2, 8, 10,
; and 16.  There are no rules for <decimal 2>, <decimal 8>, and
; <decimal 16>, which means that numbers containing decimal points
; or exponents must be in decimal radix.
; 
; <num R>  -->  <prefix R> <sign> <ureal R>
; <ureal R>  -->  <uinteger R>
;     |  <uinteger R> / <uinteger R>
;     |  <decimal R>
; <decimal 10>  -->  <uinteger 10> <suffix>
;     |  . <digit 10>+ #* <suffix>
;     |  <digit 10>+ . <digit 10>* #* <suffix>
;     |  <digit 10>+ #* . #* <suffix>
; <uinteger R>  -->  <digit R>+ #*
; <prefix R>  -->  <radix R> <exactness>
;     |  <exactness> <radix R>
; 
; <suffix>  -->  <empty>
;     |  <exponent marker> <sign> <digit>+
; <exponent marker>  -->  e  |  s  |  f  |  d  |  l
; <sign>  -->  <empty>  |  +  |  -
; <exactness>  -->  <empty>  |  #i  |  #e
; <radix 2>  -->  #b
; <radix 8>  -->  #o
; <radix 10>  -->  <empty>  |  #d
; <radix 16>  -->  #x
; <digit 2>  -->  0  |  1
; <digit 8>  -->  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7
; <digit 10>  -->  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9
; <digit 16>  -->  <digit 10>  |  a  |  b  |  c  |  d  |  e  |  f

; (decimal-digit? c)  ==>  (and (< 47 (char->integer c))
;                               (> 58 (char->integer c)))
;
; (decimal-digit-value c)  ==>  (- (char->integer c) 48)

; No macros in larceny (yet)
;
;(macro decimal-digit?
;       (lambda (l)
;         (list 'and
;               (list '< '47 (list 'char->integer (cadr l)))
;               (list '> '58 (list 'char->integer (cadr l))))))

;(macro decimal-value
;       (lambda (l)
;         (list '- (list 'char->integer (cadr l)) '48)))

(define (bellerophon . s)
  (error "Bellerophon not included."))

(define string->number)

; The input to the parser is a list of characters to be parsed.
; Its output is a number, or #f.

(define parse-number
  
  (let ((bellerophon bellerophon))
    
    (define (decimal-digit? c)
      (and (char>=? c #\0) (char<=? c #\9)))

    (define (decimal-value c)
      (- (char->integer c) (char->integer #\0)))

    (define (parse-number input)
;      (optimize speed)
      (let ((c (car input)))
        (cond ((decimal-digit? c)
               (parse-decimal (cdr input) (decimal-value c) 1))
              ((char=? c #\-)
               (parse-ureal (cdr input) #f 10 -1))
              ((char=? c #\+)
               (parse-ureal (cdr input) #f 10 1))
              ((char=? c #\.)
               (parse-ureal input 'i 10 1))
              ((char=? c #\#)
               (parse-prefix input #f #f))
              (else #f))))
    
    ; input = list of characters remaining to parse
    ; exactness = the symbol e if #e has been read
    ;             the symbol i if #i has been read
    ;             otherwise #f
    ; radix = 2, 8, 10, 16 or #f if no explicit radix prefix
    ;   has yet been read
    
    (define (parse-prefix input exactness radix)
;      (optimize space)
      (cond ((null? input) #f)
            ((char=? (car input) #\#)
             (cond ((null? (cdr input)) #f)
                   (else (let ((c (char-downcase (cadr input))))
                           (case c
                             ((#\e #\i)
                              (if exactness
                                  #f
                                  (parse-prefix (cddr input)
                                                (if (char=? c #\e) 'e 'i)
                                                radix)))
                             ((#\b #\o #\d #\x)
                              (if radix
                                  #f
                                  (parse-prefix
                                   (cddr input)
                                   exactness
                                   (cdr (assq c '((#\b . 2)
                                                  (#\o . 8)
                                                  (#\d . 10)
                                                  (#\x . 16)))))))
                             (else #f))))))
            (else (parse-sign input exactness (if radix radix 10)))))
    
    ; The prefix has been consumed, but nothing else.
    ; e is exactness prefix: e, i, or #f if no explicit prefix
    ; r is the radix: 2, 8, 10, or 16
    
    (define (parse-sign input e r)
;      (optimize space)
      (cond ((null? input) #f)
            ((char=? (car input) #\+)
             (parse-ureal (cdr input) e r 1))
            ((char=? (car input) #\-)
             (parse-ureal (cdr input) e r -1))
            (else (parse-ureal input e r 1))))
    
    ; The prefix and sign have been consumed.
    ; exactness = e, i, or #f if there is no explicit exactness.
    ; radix = 2, 8, 10, or 16.
    ; sign = 1 or -1.
    ;
    ; The numeric value of the number parsed is
    ;   (/ (* numerator (expt 10 exponent)) denominator)
    
    (define (parse-ureal input exactness radix sign)
;      (optimize space)
      (cond ((null? input) #f)
            ((and (= radix 10)
                  (radix-digit? (car input) 10) (not exactness))
             (parse-decimal (cdr input)
                            (decimal-value (car input))
                            sign))
            ((radix-digit? (car input) radix)
             (q1 (cdr input)
                 exactness
                 radix
                 sign
                 (radix-digit-value (car input) radix)))
            ((and (= radix 10)
                  (char=? (car input) #\.)
                  (not (null? (cdr input)))
                  (radix-digit? (cadr input) 10))
             (q3 (cdr input) (or exactness 'i) sign 0 0))
            (else #f)))
    
    ; At least one digit has been consumed.
    ; This is an accepting state.
    ;
    ; MacScheme deliberately accepts numbers like 3#4 in order to save
    ; code space.
    
    (define (q1 input e r s m)
;      (optimize space)
      (if (null? input)
          (create-number (or e 'e) s m 1 0)
          (let ((c (char-downcase (car input))))
            (cond ((radix-digit? c r)
                   (q1 (cdr input)
                       e r s (+ (* r m) (radix-digit-value c r))))
                  ((char=? c #\#)
                   ; should call q2 here
                   (q1 (cdr input) (or e 'i) r s (* r m)))
                  ((char=? c #\/)
                   (q7 (cdr input) e r s m))
                  ((not (= r 10)) #f)
                  ((char=? c #\.)
                   (q3 (cdr input) (or e 'i) s m 0))
                  ((exponent-marker? c)
                   (q5 (cdr input) (or e 'i) s m 0))
                  (else #f)))))
    
    ; The parse-decimal procedure is a version of q1 for use when there is
    ; no explicit exactness prefix and the radix is 10 (e = #f, r = 10).
    ; Since it takes fewer arguments and doesn't have to call char-downcase,
    ; it runs quicker.  I have also permuted its arguments so the compiler
    ; will keep m in a hardware register.
    ; Speed matters here because this is by far the most common case.
    
    (define (parse-decimal input m s)
;      (optimize speed)
      (if (null? input)
          (create-number 'e s m 1 0)
          (let ((c (car input)))
            (cond ((decimal-digit? c)
                   (parse-decimal (cdr input)
                                  (+ (* 10 m) (decimal-value c))
                                  s))
                  ((char=? c #\#)
                   ; should call q2 here
                   (q1 (cdr input) 'i 10 s (* 10 m)))
                  ((char=? c #\/)
                   (q7 (cdr input) #f 10 s m))
                  ((char=? c #\.)
                   (q3 (cdr input) 'i s m 0))
                  ((exponent-marker? (char-downcase c))
                   (q5 (cdr input) 'i s m 0))
                  (else #f)))))
    
    ; The radix is 10, a decimal point has been consumed,
    ; and either a digit has been consumed or a digit is the next character.
    ; The value read so far is (* m (expt 10 o)).
    ; This is an accepting state.
    ;
    ; MacScheme deliberately accepts 3.#4 in order to save code space.
    
    (define (q3 input e s m o)
;      (optimize space)
      (if (null? input)
          (create-number e s m 1 o)
          (let ((c (char-downcase (car input))))
            (cond ((radix-digit? c 10)
                   (q3 (cdr input)
                       e s (+ (* 10 m) (decimal-value c)) (- o 1)))
                  ((char=? c #\#)
                   ; should call q4 here
                   (q3 (cdr input) e s (* 10 m) (- o 1)))
                  ((exponent-marker? c)
                   (q5 (cdr input) (or e 'i) s m o))
                  (else #f)))))
    
    ; The radix is 10 and an exponent marker has been consumed.
    ; The value read so far is (* m (expt 10 o)).
    
    (define (q5 input e s m o)
;      (optimize space)
      (if (null? input)
          #f
          (let ((c (car input)))
            (cond ((and (or (char=? c #\+) (char=? c #\-))
                        (not (null? (cdr input))))
                   (let ((d (cadr input)))
                     (if (radix-digit? d 10)
                         (q6 (cddr input)
                             e
                             s
                             m
                             o
                             (if (char=? c #\-) -1 1)
                             (decimal-value d))
                         #f)))
                  ((radix-digit? c 10)
                   (q6 (cdr input) e s m o 1 (decimal-value c)))
                  (else #f)))))
    
    ; The radix is 10 and an exponent marker, the exponent sign (if any),
    ; and the first digit of the exponent have been consumed.
    ; This is an accepting state.
    
    (define (q6 input e s m o esign exp)
;      (optimize space)
      (if (null? input)
          (create-number e s m 1 (+ o (* esign exp)))
          (let ((c (car input)))
            (if (radix-digit? c 10)
                (q6 (cdr input)
                    e s m o esign (+ (* 10 exp) (decimal-value c)))
                #f))))
    
    ; Here we are parsing the denominator of a ratio.
    ; e = e, i, or #f if no exactness has been specified or inferred.
    ; r is the radix
    ; s is the sign
    ; m is the numerator
    
    (define (q7 input e r s m)
;      (optimize space)
      (if (null? input)
          #f
          (let ((c (car input)))
            (cond ((radix-digit? c r)
                   (q8 (cdr input) e r s m (radix-digit-value c r)))
                  (else #f)))))
    
    ; A digit has been read while parsing the denominator of a ratio.
    ; n is the denominator read so far.
    ; This is an accepting state.
    ;
    ; MacScheme accepts 3/4#5 to save code space.
    
    (define (q8 input e r s m n)
;      (optimize space)
      (if (null? input)
          (create-number (or e 'e) s m n 0)
          (let ((c (car input)))
            (cond ((radix-digit? c r)
                   (q8 (cdr input)
                       e r s m (+ (* r n) (radix-digit-value c r))))
                  ((char=? c #\#)
                   ; should call q9 here
                   (q8 (cdr input) (or e 'i) r s m (* r n)))
                  (else #f)))))
    
    (define (exponent-marker? c)
;      (optimize space)
      (memq c '(#\e #\s #\f #\d #\l)))
    
    (define (radix-digit? c r)
;      (optimize space)
      (if (eq? r 16)
          (or (decimal-digit? c)
              (let ((c (char-downcase c)))
                (and (char<=? #\a c) (char<=? c #\f))))
          (and (char<=? #\0 c)
               (char<=? c (integer->char (+ (char->integer #\0) r))))))
    
    (define (radix-digit-value c r)
;      (optimize space)
      (cond ((not (eq? r 16))
             (- (char->integer c) (char->integer #\0)))
            ((char<=? c #\9)
             (radix-digit-value c 10))
            (else (+ 10 (- (char->integer (char-downcase c))
                           (char->integer #\a))))))
    
    ;----------------------------------------------------------------
    ;
    ; The arguments to create-number contain all the information needed to
    ; create a real number of the correct sign, magnitude, and exactness.
    ;
    ;   sign        = 1 or -1
    ;   exactness   = a symbol, e or i
    ;   numerator   = an exact integer
    ;   denominator = an exact integer
    ;   exponent    = an exact integer
    
    (define (create-number exactness sign numerator denominator exponent)
;      (optimize space)
      (cond ((not (eq? denominator 1))
             (coerce-exactness exactness (/ (* sign numerator) denominator)))
            ((eq? exactness 'i)
             (* sign (bellerophon numerator exponent)))
            ((zero? exponent)
             (* sign numerator))
            ((negative? exponent)
             (/ (* sign numerator)
                (expt 10 (- exponent))))
            (else (* sign numerator (expt 10 exponent)))))
    
    (define (coerce-exactness exactness x)
;      (optimize space)
      (cond ((eq? exactness 'i)
             (if (inexact? x) x (exact->inexact x)))
            ((exact? x) x)
            ((integer? x) (inexact->exact x))
            (else x)))
    
    ;(set! string->number
    ;      (lambda (string . rest)
    ;        (optimize space)
    ;        (let ((input (string->list string)))
    ;          (cond ((null? rest)
    ;                 (parse-number input))
    ;                ((null? (cdr rest))
    ;                 (parse-prefix input (car rest) #f))
    ;                (else (parse-sign input (car rest) (cadr rest)))))))
    
    (set! string->number
          (lambda (string . rest)
;            (optimize space)
            (let ((input (string->list string)))
              (cond ((null? rest)
                     (parse-number input))
                    ((null? (cdr rest))
                     (parse-prefix input #f (car rest)))
                    (else (error "Too many arguments to string->number"))))))
    
    parse-number))

(define decimal-digit?)
(define decimal-value)
