; Larceny library: characters, strings, and bytevectors.
; Parts of this code Copyright Lightship Software.
;
; $Id: string.sch,v 1.1 1995/08/03 00:18:21 lth Exp lth $
;
; FIXME: 
;  - many character procedures should be table driven.
;  - see FIXMEs in the code for other issues.

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
    (or (char=? x #\space)                 ; space
        (char=? x #\newline)               ; UNIX: a line feed
        (char=? x (integer->char 13))      ; return
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

; Ugly, but contains no procedure calls.

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

; FIXME: slow. Use sys$bvl-copy-into! when available.
(define string-append
  (lambda args
    (list->string (apply append (map string->list args)))))
 
; FIXME: slow. Use sys$bvl-copy-into! when available.
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
    (if (and (string? s) (char? c))
	(bytevector-fill! s (char->integer c))
	(error "string-fill!: bad operands: " s c))))
 
; FIXME: should there be a bytevector-like-subfill! primop?
(define substring-fill!
  (lambda (s start end c)
    (do ((i start (+ i 1)))
        ((>= i end) s)
        (string-set! s i c))))

; Make-string is in Scheme for simplicity.
; This does *not* need to be varargs because the macro expansion pass
; gives us a default if it is not given by the programmer.
;
; FIXME: is that done also if benchmark-mode is not on?

(define (make-string n init)
  (let ((s (make-bytevector n)))
    (bytevector-fill! s (char->integer init))
    (typetag-set! s sys$tag.string-typetag)
    s))

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
  (= (string-compare 'string=? a b) 0))

(define string-equal? string=?)       ; for backward compatibility

(define (string<? a b)
  (< (string-compare 'string<? a b) 0))

(define (string<=? a b)
  (<= (string-compare 'string<=? a b) 0))

(define (string>? a b)
  (> (string-compare 'string>? a b) 0))

(define (string>=? a b)
  (>= (string-compare 'string>=? a b) 0))

(define (string-compare name a b)
  (if (not (and (string? a) (string? b)))
      (error name "Operands must be strings.")
      (sys$bvlcmp a b)))

(define (bytevector-equal? b1 b2)
  (if (not (bytevector? b1))
      (error "bytevector-equal?: not a bytevector: " b1))
  (if (not (bytevector? b2))
      (error "bytevector-equal?: not a bytevector: " b2))
  (zero? (sys$bvlcmp bv1 bv2)))

(define (bytevector-copy b)
  (if (not (bytevector? b))
      (error "bytevector-copy: not a bytevector: " b))
  (bytevector-like-copy b))

(define (bytevector-like-equal? b1 b2)
  (if (not (bytevector-like? b1))
      (error "bytevector-like-equal?: not a bytevector-like: " b1))
  (if (not (bytevector-like? b2))
      (error "bytevector-like-equal?: not a bytevector-like: " b2))
  (zero? (sys$bvlcmp b1 b2)))

; Needs to use sys$bvl-copy-into when available.

(define (bytevector-like-copy b)
  (if (not (bytevector-like? b))
      (error "bytevector-like-copy: not a bytevector-like: " b))
  (let* ((l (bytevector-like-length b))
	 (n (make-bytevector l)))
    (do ((i (- l 1) (- i 1)))
	((< i 0) (typetag-set! n (typetag b)) n)
      (bytevector-like-set! n i (bytevector-like-ref b i)))))

; OBSOLETE -- comment out.
;
; Compare bytevector-like objects and return a code for how they compare:
; -1 if the former is less than the latter; 0 if they are equal; and
; 1 if the former is greater than the latter.
;
; This should probably be a primop hooking into a millicode proc.

(define (bytevector-like-compare bv1 bv2)
  (display "WARNING: calling obsolete bytevector-like-compare.")
  (newline)
  (let* ((la    (bytevector-like-length bv1))
	 (lb    (bytevector-like-length bv2))
	 (limit (if (< la lb) la lb)))
    (letrec ((loop 
	      (lambda (i)
		(if (= i limit)
		    (- (bytevector-like-length bv1)
		       (bytevector-like-length bv2))
		    (let ((x (- (bytevector-like-ref bv1 i)
				(bytevector-like-ref bv2 i))))
		      (if (not (zero? x))
			  x
			  (loop (+ i 1))))))))
      (loop 0))))

; eof
