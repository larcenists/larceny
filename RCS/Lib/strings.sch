; Copyright Lightship Software.
;
; $Id: strings.sch,v 1.1 92/02/10 03:17:21 lth Exp Locker: lth $
;
; Library procedures for characters, strings, and bytevectors.
;
; Some of these procedures have been modified for larceny. In particular,
; there is some use of the integrables bytevector-like-<whatever> to
; operate on both strings and bytevectors.
;
; In principle, these can be defined in macscheme thusly:
;
;  (define (bytevector-like-ref x i)
;    (bytevector-ref (->bytevector x) i))
;
; and similarly for other operations.

; Upper- and lower-case predicates and conversions
; for both characters and strings.

(define char-ci=?
  (lambda (x y)
    (char=? (char-downcase x) (char-downcase y))))

(define char-ci<?
  (lambda (x y)
    (char<? (char-downcase x) (char-downcase y))))

(define char-ci>?
  (lambda (x y)
    (char>? (char-downcase x) (char-downcase y))))

(define char-ci<=?
  (lambda (x y)
    (char<=? (char-downcase x) (char-downcase y))))

(define char-ci>=?
  (lambda (x y)
    (char>=? (char-downcase x) (char-downcase y))))

(define char-alphabetic?
  (lambda (x)
    (or (and (char<=? #\a x)
             (char<=? x #\z))
        (and (char<=? #\A x)
             (char<=? x #\Z)))))

(define char-numeric?
  (lambda (x)
    (and (char<=? #\0 x)
         (char<=? x #\9))))

(define char-whitespace?
  (lambda (x)
    (or (char=? x #\space)
        (char=? x #\newline)
        (char=? x (integer->char 10))      ; line feed
        (char=? x (integer->char 9))       ; tab
        (char=? x (integer->char 12)))))   ; form feed

(define char-upper-case?
  (lambda (x)
    (and (char<=? #\A x)
         (char<=? x #\Z))))

(define char-lower-case?
  (lambda (x)
    (and (char<=? #\a x)
         (char<=? x #\z))))

(define char-upcase
  (lambda (x)
    (if (char-lower-case? x)
        (integer->char (+ (char->integer x)
                          (- (char->integer #\A)
                             (char->integer #\a))))
        x)))

(define char-downcase
  (lambda (x)
    (if (char-upper-case? x)
        (integer->char (+ (char->integer x)
                          (- (char->integer #\a)
                             (char->integer #\A))))
        x)))

(define string-ci=?
  (letrec ((loop
             (lambda (s1 s2 i)
               (cond ((< i 0) #t)
                     ((char=? (let ((c (string-ref s1 i)))
                                (if (and (char<=? #\A c)
                                         (char<=? c #\Z))
                                    (integer->char (+ (char->integer c)
                                                      (- (char->integer #\a)
                                                         (char->integer #\A))))
                                    c))
                              (let ((c (string-ref s2 i)))
                                (if (and (char<=? #\A c)
                                         (char<=? c #\Z))
                                    (integer->char (+ (char->integer c)
                                                      (- (char->integer #\a)
                                                         (char->integer #\A))))
                                    c)))
                      (loop s1 s2 (- i 1)))
                     (else #f)))))
    (lambda (s1 s2)
      (if (= (string-length s1) (string-length s2))
          (loop s1 s2 (- (string-length s1) 1))
          #f))))

(define string-ci<?
  (letrec ((loop
             (lambda (s1 s2 i n)
               (cond ((= i n)
                      (< (string-length s1) (string-length s2)))
                     ((char<? (char-downcase (string-ref s1 i))
                              (char-downcase (string-ref s2 i)))
                      #t)
                     ((char>? (char-downcase (string-ref s1 i))
                              (char-downcase (string-ref s2 i)))
                      #f)
                     (else (loop s1 s2 (+ i 1) n))))))
    (lambda (s1 s2)
      (loop s1 s2 0 (min (string-length s1) (string-length s2))))))

(define string-ci>?
  (lambda (x y)
    (string-ci<? y x)))

(define string-ci<=?
  (lambda (x y)
    (not (string-ci>? x y))))

(define string-ci>=?
  (lambda (x y)
    (not (string-ci<? x y))))

(define string-copy
  (lambda (x)
    (string-append x "")))

(define string
  (lambda chars
    (list->string chars)))

(define string-append
  (lambda args
    (list->string (apply append (map string->list args)))))
 
(define substring
  (let ((tag sys$tag.string-typetag))
    (lambda (s m n)
      (do ((x s)
	   (y (make-bytevector (- n m)))
	   (i m (+ i 1))
	   (j 0 (+ j 1)))
	  ((>= i n) (begin 
		       (typetag-set! y tag)
		       y))
	(bytevector-like-set! y j (bytevector-like-ref x i))))))
 
(define string-fill!
  (lambda (s c)
    (substring-fill! s 0 (string-length s) c)))
 
(define substring-fill!
  (lambda (s start end c)
    (do ((i start (+ i 1)))
        ((>= i end) s)
        (string-set! s i c))))

; Make-string is in Scheme for simplicity.
; This does *not* need to be varargs because the macro expansion pass
; gives us a default if it is not given by the programmer.

(define (make-string n init)
  (let ((s (make-bytevector n)))
    (bytevector-fill! s (char->integer init))
    (typetag-set! s sys$tag.string-typetag)
    s))

; Ditto for string-set!; this should really be integrable.

(define (string-set! s i x)
  (bytevector-like-set! s i (char->integer x)))

(define list->string
  (letrec ((loop
             (lambda (s i l)
               (if (not (null? l))
                   (begin (string-set! s i (car l))
                          (loop s (+ i 1) (cdr l)))
                   s))))
    (lambda (l)
      (loop (make-string (length l)) 0 l))))

(define string->list
  (letrec ((loop
             (lambda (bv i l)
               (if (< i 0)
                   l
                   (loop bv (- i 1) (cons (string-ref bv i) l))))))
    (lambda (bv)
      (loop bv (- (string-length bv) 1) '()))))

(define list->bytevector
  (letrec ((loop
             (lambda (bv i l)
               (if (not (null? l))
                   (begin (bytevector-set! bv i (car l))
                          (loop bv (+ i 1) (cdr l)))
                   bv))))
    (lambda (l)
      (loop (make-bytevector (length l)) 0 l))))
 
(define bytevector->list
  (letrec ((loop
             (lambda (bv i l)
               (if (< i 0)
                   l
                   (loop bv (- i 1) (cons (bytevector-ref bv i) l))))))
    (lambda (bv)
      (loop bv (- (bytevector-length bv) 1) '()))))

(define (string=? a b)
  (string-compare zero? 'string=? a b))

(define (string<? a b)
  (string-compare negative? 'string<? a b))

(define (string<=? a b)
  (string-compare (lambda (x) (<= x 0)) 'string<=? a b))

(define (string>? a b)
  (string-compare positive? 'string>? a b))

(define (string>=? a b)
  (string-compare (lambda (x) (>= x 0)) 'string>=? a b))

(define (string-compare pred name a b)
  (if (not (and (string? a) (string? b)))
      (error name "Operands must be strings.")
      (pred (bytevector-like-compare a b))))

(define bytevector-equal?
  (lambda (bv1 bv2)
    (if (not (and (bytevector? bv1) (bytevector? bv2)))
	(error 'bytevector-equal? "Not a bytevector.")
	(zero? (bytevector-like-compare bv1 bv2)))))

; Compare bytevector-like objects and return a code for how they compare:
; -1 if the former is less than the latter; 0 if they are equal; and
; 1 if the former is greater than the latter.

(define (bytevector-like-compare bv1 bv2)
  (let ((limit (min (bytevector-like-length bv1) 
		    (bytevector-like-length bv2))))
    (let loop ((i 0))
      (cond ((= i limit)
	     (- (bytevector-like-length bv1) (bytevector-like-length bv2)))
	    (else
	     (let ((x (- (bytevector-like-ref bv1 i)
			 (bytevector-like-ref bv2 i))))
	       (if (not (zero? x))
		   x
		   (loop (+ i 1)))))))))
