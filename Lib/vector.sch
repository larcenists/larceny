; Copyright Lightship Software
;
;                                       20 March 1990
; CHAPTER.  Byte vectors.
 
(define list->bytevector
  (letrec ((loop
             (lambda (bv i l)
               (if (not (null? l))
                   (begin (bytevector-set! bv i (car l))
                          (loop bv (1+ i) (cdr l)))
                   bv))))
    (lambda (l)
      (loop (make-bytevector (length l)) 0 l))))
 
(define bytevector->list
  (letrec ((loop
             (lambda (bv i l)
               (if (<? i 0)
                   l
                   (loop bv (1- i) (cons (bytevector-ref bv i) l))))))
    (lambda (bv)
      (loop bv (1- (bytevector-length bv)) '()))))
 
; (load "strings.sch")
; CHAPTER.  Strings.

(define-varying char->integer
  (lambda (l f)
    (cond ((and (= (length l) 2) (char? (cadr l)))
           (char->integer (cadr l)))
          (else l)))
  (lambda (c)
    (char->integer c)))

(define-varying integer->char
  (lambda (l f)
    (cond ((and (= (length l) 2) (fixnum? (cadr l)))
           (integer->char (cadr l)))
          (else l)))
  (lambda (c)
    (integer->char c)))
 
(define ascii
  (lambda (s)
    (string-ref s 0)))
 
(define-inline ascii
  (lambda (l f)
    (optimize space)
    (if (some f '(string-ref))
        l
        (if (string? (cadr l))
            (string-ref (cadr l) 0)
            l))))

(define char=?
  (lambda (x y)
    (eq? (char->integer x) (char->integer y))))
 
(define-inline char=?
  (lambda (l f)
    (optimize space)
    (if (some f '(eq? char->integer))
        l
        (list 'eq?
              (list 'char->integer (cadr l))
              (list 'char->integer (caddr l))))))
 
(define char<?
  (lambda (x y)
    (< (char->integer x) (char->integer y))))
 
(define-inline char<?
  (lambda (l f)
    (optimize space)
    (if (some f '(< char->integer))
        l
        (list '<
              (list 'char->integer (cadr l))
              (list 'char->integer (caddr l))))))
 
(define char>?
  (lambda (x y)
    (> (char->integer x) (char->integer y))))
 
(define-inline char>?
  (lambda (l f)
    (optimize space)
    (if (some f '(> char->integer))
        l
        (list '>
              (list 'char->integer (cadr l))
              (list 'char->integer (caddr l))))))
 
(define char<=?
  (lambda (x y)
    (<= (char->integer x) (char->integer y))))
 
(define-inline char<=?
  (lambda (l f)
    (optimize space)
    (if (some f '(<= char->integer))
        l
        (list '<=
              (list 'char->integer (cadr l))
              (list 'char->integer (caddr l))))))
 
(define char>=?
  (lambda (x y)
    (>= (char->integer x) (char->integer y))))
 
(define-inline char>=?
  (lambda (l f)
    (optimize space)
    (if (some f '(>= char->integer))
        l
        (list '>=
              (list 'char->integer (cadr l))
              (list 'char->integer (caddr l))))))
 
(define list->string
  (letrec ((loop
             (lambda (s i l)
               (if (not (null? l))
                   (begin (string-set! s i (car l))
                          (loop s (1+ i) (cdr l)))
                   s))))
    (lambda (l)
      (loop (make-string (length l)) 0 l))))
 
(define string->list
  (lambda (s)
    (map integer->char (bytevector->list (->bytevector s)))))
 
(define make-string
  (lambda (n . c)
    (let ((c (if (null? c) #f (car c)))
          (s (->string (make-bytevector n))))
      (if c (string-fill! s c))
      s)))
 
; Now a byte code.
;(define string-length
;  (lambda (s)
;    (bytevector-length (->bytevector s))))
 
; Now a byte code.
;(define string-ref
;  (lambda (s i)
;    (bytevector-ref (->bytevector s) i)))
 
(define string<?
  (letrec ((loop
             (lambda (bv1 bv2 i n)
               (cond ((=? i n)
                      (<? (bytevector-length bv1) (bytevector-length bv2)))
                     ((<? (bytevector-ref bv1 i) (bytevector-ref bv2 i))
                      #!true)
                     ((>? (bytevector-ref bv1 i) (bytevector-ref bv2 i))
                      #!false)
                     (else (loop bv1 bv2 (1+ i) n))))))
    (lambda (s1 s2)
      (set! s1 (->bytevector s1))
      (set! s2 (->bytevector s2))
      (loop s1 s2 0 (min (bytevector-length s1) (bytevector-length s2))))))
 
(define string-less? string<?)       ; for backward compatibility
 
(define string>? (lambda (x y) (string<? y x)))
 
(define string<=? (lambda (x y) (not (string>? x y))))
 
(define string>=? (lambda (x y) (not (string<? x y))))
 
(define string-null? (lambda (x) (string=? x "")))
 
(define string-append
  (lambda args
    (list->string (apply append (map string->list args)))))
 
(define substring
  (lambda (s m n)
    (do ((x (->bytevector s))
         (y (make-bytevector (- n m)))
         (i m (1+ i))
         (j 0 (1+ j)))
        ((>=? i n) (->string y))
        (bytevector-set! y j (bytevector-ref x i)))))
 
(define string-fill!
  (lambda (s c)
    (substring-fill! s 0 (string-length s) c)))
 
(define substring-fill!
  (lambda (s start end c)
    (do ((i start (1+ i)))
        ((>=? i end) s)
        (string-set! s i c))))
 
;(load "vectors.sch")
; CHAPTER.  Vectors.
 
(define-varying make-vector
  (lambda (l f)
    (cond ((=? (length l) 3) l)
          ((=? (length l) 2) (list 'make-vector (cadr l) ''()))
          (else #!false)))
  (lambda (n . x)
    (make-vector n (if (null? x) '() (car x)))))
 
(define vector
  (lambda l
    (list->vector l)))
 
(define list->vector
  (letrec ((loop
             (lambda (v i l)
               (if (not (null? l))
                   (begin (vector-set! v i (car l))
                          (loop v (1+ i) (cdr l)))
                   v))))
    (lambda (l)
      (loop (make-vector (length l) '()) 0 l))))
 
(define vector->list
  (letrec ((loop
             (lambda (v i l)
               (if (<? i 0)
                   l
                   (loop v (1- i) (cons (vector-ref v i) l))))))
    (lambda (v)
      (loop v (1- (vector-length v)) '()))))
 
(define vector-fill!
  (lambda (v x)
    (do ((i 0 (1+ i))
         (n (vector-length v)))
        ((=? i n) v)
        (vector-set! v i x))))
