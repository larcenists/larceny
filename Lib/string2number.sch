; Copyright (c) 1989 Lightship Software, Incorporated
;
; Permission to copy this software, to use this software for any lawful
; purpose, and to redistribute this software is granted subject to the
; restriction that any copy made of this software must include this
; copyright notice in full.  Lightship Software has made no warranty
; or representation of any kind, either express or implied, including
; but not limited to implied warranties of merchantability or fitness
; for any particular purpose.
;
; If you improve or extend this software, please make your improvement
; or extension available to Lightship Software and to the Scheme
; community in general.

; A portable implementation of STRING->NUMBER as described in R4RS.
;
; Some of the comments assume an unlimited range of exact integers.
; Without them it should still work in some sense, but roundoff errors
; would be greater and the handling of exactness might be improper.
; This program should also work without complex numbers so long as
; it isn't asked to parse any of them.

(define string->number
  
  (let ()
    
    (define (string->number s . rest)
      (cond ((zero? (string-length s)) #f)
            ((null? rest)
             (parse-prefix (string->list s) #f #f 10))
            ((null? (cdr rest))
             (let ((radix (car rest)))
               (if (memv (car rest) '(2 8 10 16))
                   (parse-prefix (string->list s) #f #f (car rest))
                   (error "Incorrect radix argument to string->number" radix))))
            (else (error "Wrong number of arguments to string->number"))))
    
    ; input = list of characters remaining to parse
    ; exactness = the symbol e if #e has been read
    ;             the symbol i if #i has been read
    ;             otherwise #f
    ; radix = 2, 8, 10, 16 or #f if no explicit radix prefix has yet been read
    ; default-radix = 2, 8, 10, or 16 (used only if radix is #f)
    
    (define (parse-prefix input exactness radix default-radix)
      (if (null? input)
          #f
          (case (car input)
            ((#\#) (if (null? (cdr input))
                       #f
                       (let ((c (char-downcase (cadr input))))
                         (case c
                           ((#\e #\i)
                            (if exactness
                                #f
                                (parse-prefix (cddr input)
                                              (if (char=? c #\e) 'e 'i)
                                              radix
                                              default-radix)))
                           ((#\b #\o #\d #\x)
                            (if radix
                                #f
                                (parse-prefix (cddr input)
                                              exactness
                                              (cdr (assv c '((#\b . 2)
                                                             (#\o . 8)
                                                             (#\d . 10)
                                                             (#\x . 16))))
                                              default-radix)))
                           (else #f)))))
            (else (parse-sign input
                              exactness
                              (if radix radix default-radix))))))
    
    ; The prefix has been consumed, but nothing else.
    ; e is exactness prefix: e, i, or #f if no explicit prefix
    ; r is the radix: 2, 8, 10, or 16
    
    (define (parse-sign input e r)
      (if (null? input)
          #f
          (case (car input)
            ((#\+) (parse-sign-follower (cdr input) e r 1))
            ((#\-) (parse-sign-follower (cdr input) e r -1))
            (else (parse-sign-follower input e r #f)))))
    
    ; The prefix and (implicit or explicit) sign have been consumed.
    ; sign = 1, -1, or #f if the sign is implicit.
    
    (define (parse-sign-follower input e r sign)
      (if (null? input)
          #f
          (if (and sign
                   (char=? (char-downcase (car input)) #\i)
                   (null? (cdr input)))
              (coerce-exactness e (make-rectangular 0 sign))
              (let ((x (parse-ureal input e r)))
                (if x
                    (let ((input (remaining-input x)))
                      (if (null? input)
                          (* (or sign 1) (create-real x))
                          (let ((c (car input)))
                            (case c
                              ((#\@)
                               (parse-angle (cdr input) e r (or sign 1) x))
                              ((#\+ #\-)
                               (parse-imaginary
                                (cdr input) e r (or sign 1) x (if (char=? c #\-) -1 1)))
                              ((#\i #\I)
                               (if (and sign (null? (cdr input)))
                                   (coerce-exactness
                                    e
                                    (make-rectangular
                                     0
                                     (* (or sign 1) (create-real x))))
                                   #f))
                              (else #f))))))))))
    
    (define (parse-angle input e r signx x)
      (let ((c (car input)))
        (let ((input (if (or (char=? c #\+) (char=? c #\-))
                         (cdr input)
                         input)))
          (let ((signy (if (char=? c #\-) -1 1))
                (y (parse-ureal input e r)))
            (and y
                 (null? (remaining-input y))
                 (coerce-exactness e
                                   (make-polar
                                    (* signx (create-real x))
                                    (* signy (create-real y)))))))))
    
    (define (parse-imaginary input e r signx x signy)
      (if (null? input)
          #f
          (cond ((char=? (char-downcase (car input)) #\i)
                 (if (null? (cdr input))
                     (coerce-exactness e (make-rectangular
                                          (* signx (create-real x))
                                          signy))
                     #f))
                (else (let ((y (parse-ureal input e r)))
                        (if y
                            (let ((input (remaining-input y)))
                              (cond ((null? input) #f)
                                    ((and (char=? (char-downcase (car input)) #\i)
                                          (null? (cdr input)))
                                     (coerce-exactness e
                                                       (make-rectangular
                                                        (* signx (create-real x))
                                                        (* signy (create-real y)))))
                                    (else #f)))
                            #f))))))
    
    ; We are about to parse a <ureal r>, though we don't know whether
    ; we're parsing the real or the imaginary part (or the angle).
    ; exactness = e, i, or #f if there is no explicit exactness.
    ;
    ; Returns #f if unsuccessful.
    ; Otherwise returns a list of the following:
    ;   input:       a list of the remaining, unparsed characters
    ;   exactness:   e, i, or #f
    ;   numerator:   an exact integer
    ;   denominator: an exact integer
    ;   exponent:    an exact integer
    ;   precision:   #\e, #\s, #\f, #\d, #\l
    ;
    ; The numeric value of the number parsed is
    ;   (/ (* numerator (expt 10 exponent)) denominator)
    
    (define (parse-ureal input exactness radix)
      (if (null? input)
          #f
          (let ((c (car input)))
            (cond ((radix-digit? c radix)
                   (q1 (cdr input) exactness radix (radix-digit-value c radix)))
                  ((and (= radix 10)
                        (char=? c #\.)
                        (not (null? (cdr input)))
                        (radix-digit? (cadr input) 10))
                   (q3 (cdr input) (or exactness 'i) 0 0))
                  (else #f)))))
    
    ; At least one digit has been consumed.
    ; This is an accepting state.
    
    (define (q1 input e r m)
      (if (null? input)
          (make-return-values input e m 1 0 #\e)
          (let ((c (char-downcase (car input))))
            (cond ((radix-digit? c r)
                   (q1 (cdr input) e r (+ (* r m) (radix-digit-value c r))))
                  ((char=? c #\#)
                   (q2 (cdr input) (or e 'i) r (* r m)))
                  ((char=? c #\/)
                   (q7 (cdr input) e r m))
                  ((not (= r 10)) (make-return-values input e m 1 0 #\e))
                  ((char=? c #\.)
                   (q3 (cdr input) (or e 'i) m 0))
                  ((exponent-marker? c)
                   (q5 (cdr input) (or e 'i) m 0 c))
                  (else (make-return-values input e m 1 0 #\e))))))
    
    ; At least one don't care digit has been consumed.
    ; This is an accepting state.
    
    (define (q2 input e r m)
      (if (null? input)
          (make-return-values input e m 1 0 #\e)
          (let ((c (char-downcase (car input))))
            (cond ((char=? c #\#)
                   (q2 (cdr input) e r (* r m)))
                  ((char=? c #\/)
                   (q7 (cdr input) e r m))
                  ((not (= r 10)) (make-return-values input e m 1 0 #\e))
                  ((char=? c #\.)
                   (q3 (cdr input) (or e 'i) m 0))
                  ((exponent-marker? c)
                   (q5 (cdr input) (or e 'i) m 0 c))
                  (else (make-return-values input e m 1 0 #\e))))))
    
    ; The radix is 10, a decimal point has been consumed,
    ; and either a digit has been consumed or a digit is the next character.
    ; The value read so far is (* m (expt 10 o)).
    ; This is an accepting state.
    
    (define (q3 input e m o)
      (if (null? input)
          (make-return-values input e m 1 o #\e)
          (let ((c (char-downcase (car input))))
            (cond ((radix-digit? c 10)
                   (q3 (cdr input) e (+ (* 10 m) (radix-digit-value c 10)) (- o 1)))
                  ((char=? c #\#)
                   (q4 (cdr input) e (* 10 m) (- o 1)))
                  ((exponent-marker? c)
                   (q5 (cdr input) (or e 'i) m o c))
                  (else (make-return-values input e m 1 o #\e))))))
    
    ; The radix is 10 and a digit, a decimal point, and a don't care digit
    ; have been consumed.
    ; The value read so far is (* m (expt 10 o)).
    ; This is an accepting state.
    
    (define (q4 input e m o)
      (if (null? input)
          (make-return-values input e m 1 o #\e)
          (let ((c (char-downcase (car input))))
            (cond ((char=? c #\#)
                   (q4 (cdr input) e (* 10 m) (- o 1)))
                  ((exponent-marker? c)
                   (q5 (cdr input) e m o c))
                  (else (make-return-values input e m 1 o #\e))))))
    
    ; The radix is 10 and an exponent marker has been consumed.
    ; precision = #\e, #\s, #\f, #\d, or #\l
    ; The value read so far is (* m (expt 10 o)).
    
    (define (q5 input e m o precision)
      (if (null? input)
          #f
          (let ((c (car input)))
            (cond ((and (or (char=? c #\+) (char=? c #\-))
                        (not (null? (cdr input))))
                   (let ((d (cadr input)))
                     (if (radix-digit? d 10)
                         (q6 (cddr input)
                             e
                             m
                             o
                             precision
                             (if (char=? c #\-) -1 1)
                             (radix-digit-value d 10))
                         #f)))
                  ((radix-digit? c 10)
                   (q6 (cdr input) e m o precision 1 (radix-digit-value c 10)))
                  (else #f)))))
    
    ; The radix is 10 and an exponent marker, the exponent sign (if any),
    ; and the first digit of the exponent have been consumed.
    ; This is an accepting state.
    
    (define (q6 input e m o p esign exp)
      (if (null? input)
          (make-return-values input e m 1 (+ o (* esign exp)) p)
          (let ((c (car input)))
            (if (radix-digit? c 10)
                (q6 (cdr input) e m o p esign (+ (* 10 exp) (radix-digit-value c 10)))
                (make-return-values input e m 1 (+ o (* esign exp)) p)))))
    
    ; Here we are parsing the denominator of a ratio.
    ; e = e, i, or #f if no exactness has been specified or inferred.
    ; r is the radix
    ; m is the numerator
    
    (define (q7 input e r m)
      (if (null? input)
          #f
          (let ((c (car input)))
            (cond ((radix-digit? c r)
                   (q8 (cdr input) e r m (radix-digit-value c r)))
                  (else #f)))))
    
    ; A digit has been read while parsing the denominator of a ratio.
    ; n is the denominator read so far.
    ; This is an accepting state.
    
    (define (q8 input e r m n)
      (if (null? input)
          (make-return-values input e m n 0 #\e)
          (let ((c (car input)))
            (cond ((radix-digit? c r)
                   (q8 (cdr input) e r m (+ (* r n) (radix-digit-value c r))))
                  ((char=? c #\#)
                   (q9 (cdr input) (or e 'i) r m (* r n)))
                  (else (make-return-values input e m n 0 #\e))))))
    
    ; A don't care digit has been read in the denominator of a ratio.
    ; This is an accepting state.
    
    (define (q9 input e r m n)
      (if (null? input)
          (make-return-values input e m n 0 #\e)
          (let ((c (car input)))
            (cond ((char=? c #\#)
                   (q9 (cdr input) e r m (* r n)))
                  (else (make-return-values input e m n 0 #\e))))))
    
    (define (exponent-marker? c)
      (memv c '(#\e #\s #\f #\d #\l)))
    
    ; The following procedures could be made more efficient by assuming
    ; a specific character code.
    
    (define (radix-digit? c r)
      (assv (char-downcase c) (cdr (assv r radix-digit-table))))
    
    (define (radix-digit-value c r)
      (cadr (assv (char-downcase c) (cdr (assv r radix-digit-table)))))
    
    (define radix-digit-table
      '((2 (#\0 0) (#\1 1))
        (8 (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7))
        (10 (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7)
            (#\8 8) (#\9 9))
        (16 (#\0 0) (#\1 1) (#\2 2) (#\3 3) (#\4 4) (#\5 5) (#\6 6) (#\7 7) (#\8 8)
            (#\9 9) (#\a 10) (#\b 11) (#\c 12) (#\d 13) (#\e 14) (#\f 15))))
    
    ; I wish Scheme had multiple return values.
    ; The parse-ureal procedure needs to return 6 things.
    
    (define make-return-values list)
    
    (define remaining-input car)
    (define number-description-exactness cadr)
    (define number-description-numerator caddr)
    (define number-description-denominator cadddr)
    (define (number-description-exponent x)
      (car (cddddr x)))
    (define (number-description-precision x)
      (cadr (cddddr x)))
    
    ;----------------------------------------------------------------
    ;
    ; The argument to create-real is a list containing all the information
    ; needed to create a nonnegative real number of the correct magnitude,
    ; precision, and exactness.
    ;
    ;   exactness   = e, i, or #f
    ;   numerator   = an exact integer
    ;   denominator = an exact integer
    ;   exponent    = an exact integer
    ;   precision   = a character: #\e, #\s, #\f, #\d, #\l
    ;
    ; If the exactness is #f, then the result ought to be exact but there
    ; was no explicit exactness prefix.  Pragmatically, this may mean
    ; that if the number cannot be represented as an exact number then it
    ; is better to return an approximately correct inexact number than to
    ; report a violation of an implementation restriction.  At any rate,
    ; that's how COERCE-EXACTNESS interprets it.
    ;
    ; Scheme provides no portable way to use the precision information.
    
    (define (create-real x)
      (let ((exactness (number-description-exactness x))
            (numerator (number-description-numerator x))
            (denominator (number-description-denominator x))
            (exponent (number-description-exponent x))
            (precision (number-description-precision x)))
        (let ((v (if (negative? exponent)
                     (/ numerator (* (expt 10 (- exponent)) denominator))
                     (/ (* numerator (expt 10 exponent)) denominator))))
          (coerce-exactness exactness v))))
    
    ; In a sufficiently good implementation, the error case should never
    ; be executed, and x should always be exact even when exactness is #f.
    
    (define (coerce-exactness exactness x)
      (cond ((and (eq? exactness 'i) (exact? x))
             (exact->inexact x))
            ((and (eq? exactness 'e) (inexact? x))
             (error "Exact constant is not representable" x))
            (else x)))
    
    string->number))


