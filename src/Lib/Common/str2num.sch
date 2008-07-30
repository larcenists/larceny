; Copyright Lightship Software, Incorporated.
;
; $Id$
;
; A parser for numeric constants in MacScheme,
; upgraded to accept the union of R6RS and ERR5RS syntax.
;
; The differences between R6RS and ERR5RS syntax are:
;
;     ERR5RS does not allow the imaginary part of a
;         rectangular notation to be an infinity or NaN
;
;     ERR5RS does not allow mantissa widths
;
;     ERR5RS allows trailing decimal digits to be #
;
;     ERR5RS allows 3+4I.  Despite an erratum, it is
;         still unclear whether the R6RS allows that.
;         It is also unclear whether the R6RS allows
;         +NaN.0 or -InF.0.
;
; ERR5RS also allows extensions, but the R6RS doesn't.
;
; Uses a procedure named bellerophon, which should implement
; Algorithm Bellerophon for reading floating point numbers perfectly.
;
; The following syntax is from the R6RS, without ERR5RS
; extensions.
;
; <number> -> <num 2> | <num 8>
;          | <num 10> | <num 16>
; <num R> -> <prefix R> <complex R>
; <complex R> -> <real R> | <real R> @ <real R>
;          | <real R> + <ureal R> i | <real R> - <ureal R> i
;          | <real R> + <naninf> i | <real R> - <naninf> i
;          | <real R> + i | <real R> - i
;          | + <ureal R> i | - <ureal R> i
;          | + <naninf> i | - <naninf> i
;          | + i | - i
; <real R> -> <sign> <ureal R>
;          | + <naninf> | - <naninf>
; <naninf> -> nan.0 | inf.0
; <ureal R> -> <uinteger R>
;          | <uinteger R> / <uinteger R>
;          | <decimal R> <mantissa width>
; <decimal 10> -> <uinteger 10> <suffix>
;          | . <digit 10>+ <suffix>
;          | <digit 10>+ . <digit 10>* <suffix>
;          | <digit 10>+ . <suffix>
; <uinteger R> -> <digit R>+
; <prefix R> -> <radix R> <exactness>
;          | <exactness> <radix R>
; 
; <suffix> -> <empty>
;          | <exponent marker> <sign> <digit 10>+
; <exponent marker> -> e | E | s | S | f | F
;          | d | D | l | L
; <mantissa width> -> <empty>
;          | | <digit 10>+
; <sign> -> <empty> | + | -
; <exactness> -> <empty>
;          | #i| #I | #e| #E
; <radix 2> -> #b| #B
; <radix 8> -> #o| #O
; <radix 10> -> <empty> | #d | #D
; <radix 16> -> #x| #X
; <digit 2> -> 0 | 1
; <digit 8> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
; <digit 10> -> <digit>
; <digit 16> -> <hex digit>

($$trace "str2num")

; String->number takes a number or a number and a radix.
; Its output is a number, or #f.

; Parse-number takes a list of characters to be parsed.
; Its output is a number, or #f.

(define (string->number s . rest)
  
  (let ((radix (cond ((null? rest) #f)
                     ((null? (cdr rest)) (car rest))
                     (else (assertion-violation 'string->number
                                                "too many arguments"
                                                (cons s rest)))))
        (n (string-length s)))

    ;; This is a procedure because at the time we wish to call make-flonum,
    ;; generic arithmetic is not yet fully operational.

    (define (flonum:nan) (make-flonum 0 1 1024))
    
    ;; This is a procedure because flonum:nan is.

    (define (flonum:inf) 1e500)

    (define (decimal-digit? c)
      (char<=? #\0 c #\9))

    (define (decimal-value c)
      (- (char->integer c) (char->integer #\0)))

    (define (parse-number)
      (if (= n 0)
          #f
          (case (string-ref s 0)
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+)
            (parse-complex 0 #f (or radix 10)))
           ((#\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)
            (case radix
             ((16) (parse-complex 0 #f 16))
             (else #f)))
           ((#\.)
            (case radix
             ((#f 10) (parse-complex 0 #f 10))
             (else #f)))
           ((#\#)
            (parse-prefix 0 #f #f))
           (else #f))))
    
    ;; exactness = the symbol e if #e has been read
    ;;             the symbol i if #i has been read
    ;;             otherwise #f
    ;; radix = 2, 8, 10, 16 or #f if no explicit radix prefix read yet
    
    (define (parse-prefix i exactness radix)
      (if (>= i n)
          #f
          (case (string-ref s i)
           ((#\#)
            (let ((i (+ i 1)))
              (if (>= i n)
                  #f
                  (case (string-ref s i)
                   ((#\e #\E)
                    (if exactness #f (parse-prefix (+ i 1) 'e radix)))
                   ((#\i #\I)
                    (if exactness #f (parse-prefix (+ i 1) 'i radix)))
                   ((#\b #\B)
                    (if radix #f (parse-prefix (+ i 1) exactness 2)))
                   ((#\o #\O)
                    (if radix #f (parse-prefix (+ i 1) exactness 8)))
                   ((#\d #\D)
                    (if radix #f (parse-prefix (+ i 1) exactness 10)))
                   ((#\x #\X)
                    (if radix #f (parse-prefix (+ i 1) exactness 16)))
                   (else #f)))))
           (else (parse-complex i exactness (or radix 10))))))

    ;; Prefix has been consumed, but not anything else.
    ;; exactness = the symbol e if #e has been read
    ;;             the symbol i if #i has been read
    ;;             otherwise #f
    ;; radix = 2, 8, 10, or 16

    (define (parse-complex i exactness radix)
      (if (>= i n)
          #f
          (case (string-ref s i)
           ((#\-)
            (parse-ucomplex (+ i 1) exactness radix -1))
           ((#\+)
            (parse-ucomplex (+ i 1) exactness radix 1))
           (else
            (call-with-values
             (lambda () (parse-ureal i exactness radix 1))
             (lambda (areal i)
               (and areal
                    (parse-imaginary i exactness radix areal))))))))

    ;; An explicit sign has just been read.
    ;; sign = 1 or -1

    (define (parse-ucomplex i exactness radix sign)
      (if (>= i n)
          #f
          (case (string-ref s i)
           ((#\i #\I)               ; FIXME: does the R6RS allow #\I here?
            (cond ((= (+ i 1) n)
                   (coerce-exactness exactness -i))
                  ((and (<= (+ i 5) n)
                        (char=? #\n (string-ref s (+ i 1)))
                        (char=? #\f (string-ref s (+ i 2)))
                        (char=? #\. (string-ref s (+ i 3)))
                        (char=? #\0 (string-ref s (+ i 4))))
                   (parse-imaginary2 (+ i 5)
                                     exactness radix
                                     (* sign (flonum:inf))))
                  (else #f)))
           ((#\n)
            (cond ((and (<= (+ i 5) n)
                        (char=? #\a (string-ref s (+ i 1)))
                        (char=? #\n (string-ref s (+ i 2)))
                        (char=? #\. (string-ref s (+ i 3)))
                        (char=? #\0 (string-ref s (+ i 4))))
                                                       ; FIXME
                   (parse-imaginary2 (+ i 5)
                                     exactness radix
                                     (* sign (flonum:nan))))
                  (else #f)))
           (else
            (call-with-values
             (lambda () (parse-ureal i exactness radix sign))
             (lambda (areal i)
               (and areal
                    (parse-imaginary2 i exactness radix areal))))))))

    ;; An unsigned real part has just been read.
    ;; Note that 45i is not allowed, but +45i is.

    (define (parse-imaginary i exactness radix areal)
      (if (>= i n)
          (coerce-exactness exactness areal)
          (case (string-ref s i)
           ((#\+)
            (parse-uimaginary (+ i 1) exactness radix areal 1))
           ((#\-)
            (parse-uimaginary (+ i 1) exactness radix areal -1))
           ((#\@)
            (parse-angle (+ i 1) exactness radix areal))
           (else #f))))

    ;; An explicitly signed real part has just been read.
    ;; parse-imaginary2 is like parse-imaginary except it
    ;; allows i as a terminating character.

    (define (parse-imaginary2 i exactness radix areal)
      (if (>= i n)
          (parse-imaginary i exactness radix areal)
          (case (string-ref s i)
           ((#\i #\I)               ; FIXME
            (if (= (+ i 1) n)
                (coerce-exactness exactness
                                  (make-rectangular 0 areal))
                #f))
           (else
            (parse-imaginary i exactness radix areal)))))

    ;; The real part and an explicit sign have just been read.

    (define (parse-uimaginary i exactness radix areal sign)
      (if (>= i n)
          #f
          (case (string-ref s i)
           ((#\i #\I)                  ; FIXME
            (cond ((= (+ i 1) n)
                   (coerce-exactness exactness
                                     (make-rectangular areal sign)))
                  ((and (<= (+ i 6) n)
                        (char=? #\n (string-ref s (+ i 1)))
                        (char=? #\f (string-ref s (+ i 2)))
                        (char=? #\. (string-ref s (+ i 3)))
                        (char=? #\0 (string-ref s (+ i 4)))
                        (char=? #\i (string-ref s (+ i 5))))
                   (coerce-exactness exactness
                                     (make-rectangular areal
                                                       (* sign (flonum:inf)))))
                  (else #f)))
           ((#\n)
            (cond ((and (<= (+ i 6) n)
                        (char=? #\a (string-ref s (+ i 1)))
                        (char=? #\n (string-ref s (+ i 2)))
                        (char=? #\. (string-ref s (+ i 3)))
                        (char=? #\0 (string-ref s (+ i 4)))
                        (char=? #\i (string-ref s (+ i 5))))
                   (coerce-exactness exactness
                                     (make-rectangular areal
                                                       ; FIXME
                                                       (* sign (flonum:nan)))))
                  (else #f)))
           (else
            (call-with-values
             (lambda () (parse-ureal i exactness radix sign))
             (lambda (imag i)
               (and imag
                    (= (+ i 1) n)
                    (char=? #\i (char-downcase (string-ref s i)))
                    (coerce-exactness exactness
                                      (make-rectangular areal
                                                        imag)))))))))

    ;; The real part and an @-sign have just been read.

    (define (parse-angle i exactness radix areal)
      (if (>= i n)
          #f
          (case (string-ref s i)
           ((#\+)
            (parse-angle2 (+ i 1) exactness radix areal 1))
           ((#\-)
            (parse-angle2 (+ i 1) exactness radix areal -1))
           ((#\i #\I #\n #\N)
            #f)
           (else
            (parse-angle2 i exactness radix areal 1)))))

    ;; The real part and an @-sign have just been read.
    ;; Either an explicit sign has just been read, or
    ;; lookahead has determined that there is no sign.

    (define (parse-angle2 i exactness radix areal sign)
      (if (>= i n)
          #f
          (case (string-ref s i)
           ((#\i)
           (cond ((and (= (+ i 5) n)
                       (char=? #\n (string-ref s (+ i 1)))
                       (char=? #\f (string-ref s (+ i 2)))
                       (char=? #\. (string-ref s (+ i 3)))
                       (char=? #\0 (string-ref s (+ i 4))))
                  (coerce-exactness exactness
                                    (make-polar areal (* sign (flonum:inf)))))
                 (else #f)))
          ((#\n)
           (cond ((and (= (+ i 5) n)
                       (char=? #\a (string-ref s (+ i 1)))
                       (char=? #\n (string-ref s (+ i 2)))
                       (char=? #\. (string-ref s (+ i 3)))
                       (char=? #\0 (string-ref s (+ i 4))))
                                                      ; FIXME
                  (coerce-exactness exactness
                                    (make-polar areal (* sign (flonum:nan)))))
                 (else #f)))
           (else
            (call-with-values
             (lambda () (parse-ureal i exactness radix sign))
             (lambda (angle i)
               (and angle
                    (= i n)
                    (coerce-exactness exactness
                                      (make-polar areal
                                                  angle)))))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; parse-ureal
    ;;
    ;; This procedure is called during parsing of the real part,
    ;; imaginary part, and angle.
    ;;
    ;; exactness = the symbol e if #e has been read
    ;;             the symbol i if #i has been read
    ;;             otherwise #f
    ;; radix = 2, 8, 10, or 16
    ;; sign = 1 or -1
    ;;
    ;; Returns two values:
    ;;     #f or a real value parsed from s
    ;;     index of the first unconsumed character in s
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (parse-ureal i exactness radix sign)
      (if (= radix 10)
          (parse-ureal10 i exactness sign)
          (call-with-values
           (lambda () (parse-uinteger i radix sign))
           (lambda (numerator i)
             (cond ((not numerator)
                    (values #f i))
                   ((and (< i n)
                         (char=? #\/ (string-ref s i)))
                    (call-with-values
                     (lambda () (parse-uinteger (+ i 1) radix sign))
                     (lambda (denominator i)
                       (values (and denominator
                                    (create-number exactness
                                                   sign
                                                   numerator
                                                   denominator
                                                   0))
                               i))))
                   (else
                    (values (create-number exactness sign numerator 1 0)
                            i)))))))

    (define (parse-uinteger i radix sign)
      (define (loop i k)
        (if (>= i n)
            (values k i)
            (let* ((c (string-ref s i)))
              (case c
               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                (let ((j (decimal-value c)))
                  (if (< j radix)
                      (loop (+ i 1) (+ (* radix k) j))
                      (values k i))))
               ((#\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)
                (if (= radix 16)
                    (let ((j (+ 10 (- (char->integer (char-downcase c))
                                      (char->integer #\a)))))
                      (loop (+ i 1) (+ (* radix k) j)))
                    (values k i)))
               (else
                (values k i))))))
      (if (>= i n)
          (values #f i)
          (let ((c (string-ref s i)))
            (case radix
             ((2)
              (if (char<=? #\0 c #\1)
                  (loop i 0)
                  (values #f i)))
             ((8)
              (if (char<=? #\0 c #\7)
                  (loop i 0)
                  (values #f i)))
             ((10)
              (if (char<=? #\0 c #\9)
                  (loop i 0)
                  (values #f i)))
             ((16)
              (if (or (char<=? #\0 c #\9)
                      (char<=? #\a (char-downcase c) #\f))
                  (loop i 0)
                  (values #f i)))
             (else
              (values #f i))))))

    (define (parse-ureal10 i e sign)
      (if (>= i n)
          (values #f i)
          (let ((c (string-ref s i)))
            (cond ((char=? c #\.)
                   (let ((i (+ i 1)))
                     (if (and (< i n)
                              (decimal-digit? (string-ref s i)))
                         (parse-scientific-fraction i
                                                    (or e 'i)
                                                    sign 0 0 #f)
                         (values #f i))))
                  ((decimal-digit? c)
                   (parse-ureal10a (+ i 1) e sign (decimal-value c)))
                  (else
                   (values #f i))))))

    ;; A nonempty sequence of decimal digits has been read,
    ;; and their value is (* sign k).

    (define (parse-ureal10a i e sign k)
      (if (>= i n)
          (values (create-number e sign k 1 0) i)
          (let ((c (string-ref s i)))
            (if (decimal-digit? c)
                (parse-ureal10a (+ i 1)
                                e sign
                                (+ (* 10 k) (decimal-value c)))
                (parse-ureal10part2 i e sign k #f)))))

    (define (parse-ureal10part2 i e sign k sharps-only?)
      (if (>= i n)
          (values (create-number e sign k 1 0) i)
          (case (string-ref s i)
           ((#\#)
            (parse-ureal10part2 (+ i 1) (or e 'i) sign (* 10 k) #t))
           ((#\.)
            (parse-scientific-fraction (+ i 1)
                                       (or e 'i) sign k 0 sharps-only?))
           ((#\i #\I #\+ #\- #\@)                     ; FIXME
            (values (create-number e sign k 1 0) i))
           ((#\/)
            (call-with-values
             (lambda () (parse-uinteger (+ i 1) 10 1))
             (lambda (denominator i)
               (values (and denominator
                            (create-number e sign k denominator 0))
                       i))))
           ((#\d #\e #\f #\l #\s #\D #\E #\F #\L #\S)
            (let ((precision (char-downcase (string-ref s i))))
              (parse-exponent (+ i 1) (or e 'i) sign k 0 precision)))
           ((#\|)
            (parse-mantissawidth (+ i 1) (or e 'i) sign k 0 #f))
           (else
            (values (create-number e sign k 1 0) i)))))

    ;; A decimal point has been read, and the value of what
    ;; has been read so far is (* sign p (expt 10 exponent)).
    ;;
    ;; e = the symbol e or the symbol i
    ;; sign = 1 or -1
    ;; p = an exact integer >= 0
    ;; exponent = an exact integer
    ;; sharps-only? = a boolean

    (define (parse-scientific-fraction i e sign p exponent sharps-only?)
      (if (>= i n)
          (values (create-number e sign p 1 exponent) i)
          (let ((c (string-ref s i)))
            (case c
             ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
              (if sharps-only?
                  (values (create-number e sign p 1 exponent) i)
                  (let ((p (+ (* 10 p) (decimal-value c))))
                    (parse-scientific-fraction
                     (+ i 1) e sign p (- exponent 1) sharps-only?))))
             ((#\#)
              (parse-scientific-fraction
               (+ i 1) e sign (* 10 p) (- exponent 1) #t))
             ((#\d #\e #\f #\l #\s #\D #\E #\F #\L #\S)
              (let ((precision (char-downcase (string-ref s i))))
                (parse-exponent (+ i 1) (or e 'i) sign p exponent precision)))
             ((#\|)
              (parse-mantissawidth (+ i 1) (or e 'i) sign p exponent #f))
             (else
              (values (create-number e sign p 1 exponent) i))))))

    ;; An exponent marker has been read, and the value of what
    ;; has been read so far is (* sign p (expt 10 exponent)).
    ;;
    ;; e = the symbol e or the symbol i
    ;; sign = 1 or -1
    ;; p = an exact integer >= 0
    ;; exponent = an exact integer
    ;; precision = #f or #\d or #\e or #\f or #\l or #\s
    ;;
    ;; FIXME: what happens to the precision when we create a number?

    (define (parse-exponent i e sign p exponent precision)
      (define (loop i k ksign)
        (if (>= i n)
            (values (create-number e sign p 1 (+ exponent (* ksign k)))
                    i)
            (let ((c (string-ref s i)))
              (cond ((decimal-digit? c)
                     (loop (+ i 1)
                           (+ (* 10 k) (decimal-value c))
                           ksign))
                    ((char=? c #\|)
                     (let ((exponent (+ exponent (* ksign k))))
                       (parse-mantissawidth
                        (+ i 1) e sign p exponent precision)))
                    (else
                     (values (create-number e sign
                                            p 1 (+ exponent (* ksign k)))
                             i))))))
      (if (>= i n)
          (values #f i)
          (case (string-ref s i)
           ((#\+ #\-)
            (let* ((sign (string-ref s i))
                   (i (+ i 1)))
              (if (and (< i n)
                       (decimal-digit? (string-ref s i)))
                  (loop i 0 (if (char=? sign #\+) 1 -1))
                  (values #f i))))
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (loop i 0 1))
           (else
            (values #f i)))))

    ;; A vertical bar has just been read, and the value of what
    ;; has been read so far is (* sign p (expt 10 exponent)).
    ;;
    ;; e = the symbol e or the symbol i
    ;; sign = 1 or -1
    ;; p = an exact integer >= 0
    ;; exponent = an exact integer
    ;; precision = #f or #\d or #\e or #\f or #\l or #\s
    ;;
    ;; FIXME: what happens to the precision when we create a number?

    (define (parse-mantissawidth i e sign p exponent precision)
      (define (loop i k)
        (if (>= i n)
            (values (create-number e sign p 1 exponent)
                    i)
            (let ((c (string-ref s i)))
              (cond ((decimal-digit? c)
                     (loop (+ i 1)
                           (+ (* 10 k) (decimal-value c))))
                    (else
                     ; FIXME: the mantissa width is k, but we ignore it
                     (values (create-number e sign p 1 exponent)
                             i))))))
      (if (>= i n)
          (values #f i)
          (case (string-ref s i)
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (loop i 0))
           (else
            (values #f i)))))

    ; FIXME: these two procedures are no longer used.
    ; Maybe they should be.
    
    (define (radix-digit? c r)
      (if (eq? r 16)
          (or (decimal-digit? c)
              (let ((c (char-downcase c)))
                (and (char<=? #\a c) (char<=? c #\f))))
          (and (char<=? #\0 c)
               (char<? c (integer->char (+ (char->integer #\0) r))))))
    
    (define (radix-digit-value c r)
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
    ;   exactness   = a symbol, e or i
    ;   sign        = 1 or -1
    ;   numerator   = an exact integer >= 0
    ;   denominator = an exact integer >= 0
    ;   exponent    = an exact integer
    
    (define (create-number exactness sign numerator denominator exponent)
      (cond ((not (eq? denominator 1))
             ; exponent must be 0
             (if (eq? denominator 0)
                 (if (eq? exactness 'i)
                     (/ (exact->inexact (* sign numerator))
                        (exact->inexact 0))
                     #f)
                 (coerce-exactness exactness
                                   (/ (* sign numerator) denominator))))
            ((eq? exactness 'i)
             (* sign (bellerophon numerator exponent)))
            ((zero? exponent)
             (* sign numerator))
            ((negative? exponent)
             (/ (* sign numerator)
                (expt 10 (- exponent))))
            (else (* sign numerator (expt 10 exponent)))))
    
    ; Given an exactness (e or i or #f) and an exact number x,
    ; coerces x to the specified exactness.  #f means e.

    (define (coerce-exactness exactness x)
      (cond ((eq? exactness 'i)
             (if (inexact? x) x (exact->inexact x)))
            (else x)))

    (if (not (or (not radix)
                 (memv radix '(2 8 10 16))))
        (assertion-violation 'string->number "illegal radix" radix))

    (parse-number)))

; eof
