; Copyright Lightship Software, Incorporated.
;
; $Id$
;
; Larceny library --  characters, strings, and bytevectors.
;
; Should there be a bytevector-like-subfill! primop to use here?

($$trace "string")

; The character table that follows, and the procedures that use it,
; are obsolete and will soon be replaced by Unicode tables and
; procedures.

; The character set, collating order, and so on can be redefined by
; changing this table.
;
; The table has a byte for each character, with the following value
; assignments:
; - Uppercase alphabetic: value 1
; - Lowercase alphabetic: value 2
; - Numeric:              value 4
; - Whitespace:           value 8

(define *char-table* '#())


; ISO Latin 1 character set.

(define (make-iso-latin-1-table)

  (define tbl (make-bytevector 256))

  (bytevector-fill! tbl 0)

  (do ((i (char->integer #\A) (+ i 1)))
      ((> i (char->integer #\Z)))
    (bytevector-set! tbl i 1))

  (do ((i (char->integer #\a) (+ i 1)))
      ((> i (char->integer #\z)))
    (bytevector-set! tbl i 2))

  (do ((i (char->integer #\À) (+ i 1)))
      ((> i (char->integer #\Ö)))
    (bytevector-set! tbl i 1))

  (do ((i (char->integer #\Ø) (+ i 1)))
      ((> i (char->integer #\ß)))
    (bytevector-set! tbl i 1))

  (do ((i (char->integer #\à) (+ i 1)))
      ((> i (char->integer #\ö)))
    (bytevector-set! tbl i 2))

  (do ((i (char->integer #\ø) (+ i 1)))
      ((> i (char->integer #\ÿ)))
    (bytevector-set! tbl i 2))

  (do ((i (char->integer #\0) (+ i 1)))
      ((> i (char->integer #\9)))
    (bytevector-set! tbl i 4))

  (let ((f (lambda (x)
	     (let ((i (char->integer x)))
	       (bytevector-set! tbl i 8)))))
    (f #\space)				;
    (f #\newline)			; Unix: LF  (code 10)
    (f (integer->char 13))		; CR
    (f (integer->char 9))		; TAB
    (f (integer->char 12))		; Form feed
    )

  tbl)

(set! *char-table* (make-iso-latin-1-table))


(define (string-ci=? s1 s2)

  (define (loop i)
    (cond ((< i 0))
	  ((char-ci=? (string-ref s1 i) (string-ref s2 i))
	   (loop (- i 1)))
	  (else #f)))

  (if (= (string-length s1) (string-length s2))
      (loop (- (string-length s1) 1))
      #f))

(define (string-ci<? s1 s2)

  (define (loop i limit)
    (cond ((= i limit)
	   (< (string-length s1) (string-length s2)))
	  ((char-ci<? (string-ref s1 i) (string-ref s2 i))
	   #t)
	  ((char-ci>? (string-ref s1 i) (string-ref s2 i))
	   #f)
	  (else
	   (loop (+ i 1) limit))))

  (loop 0 (min (string-length s1) (string-length s2))))

(define string-ci>?
  (lambda (x y)
    (string-ci<? y x)))

(define string-ci<=?
  (lambda (x y)
    (not (string-ci>? x y))))

(define string-ci>=?
  (lambda (x y)
    (not (string-ci<? x y))))

; Copies (substring x i j) into y starting at k.
; Used only within this file.
; Performs no checking.
; Assumes x and y are distinct strings.

(define string-copy-into!
  (lambda (x i j y k)
    (do ((i i (+ i 1))
         (k k (+ k 1)))
        ((>= i j))
      (string-set! y k (string-ref x i)))))

(define string-copy
  (lambda (x)
    (let* ((length (string-length x))
           (y (make-string length)))
      (string-copy-into! x 0 length y 0)
      y)))

(define string
  (lambda chars
    (list->string chars)))

(define (concatenate-strings string-list)
  (define (concatenate-strings1 position tail)
    (cond ((pair? tail)
           (let* ((this-string  (car tail))
                  (length (string-length this-string))
                  (result-string
                   (concatenate-strings1 (+ position length) (cdr tail))))
             (string-copy-into! this-string 0 length
                                result-string position)
             result-string))
          ((null? tail) (make-string position))
          (else (error "concatenate-strings: improper list") #t)))
  (concatenate-strings1 0 string-list))

(define (string-append . args)
  (concatenate-strings args))

(define (substring s m n)
  (let ((length (string-length s)))
    (if (and (fixnum? m)
             (fixnum? n)
             (fx<= 0 m)
             (fx<= m n)
             (fx<= n length))
        (let ((y (make-string (- n m))))
          (string-copy-into! s m n y 0)
          y)
        (error "substring: bad operands: " s " " m " " n))))

(define string-fill!
  (lambda (s c)
    (substring-fill! s 0 (string-length s) c)))

(define substring-fill!
  (lambda (s start end c)
    (do ((i start (+ i 1)))
        ((>= i end) s)
        (string-set! s i c))))

; Make-string is now a primitive; see primops.sch.

;(define (make-string n . rest)
;  (let ((init (char->integer (if (null? rest) #\space (car rest))))
;	(s    (make-bytevector n)))
;    (bytevector-fill! s init)
;    (typetag-set! s sys$tag.string-typetag)
;    s))

(define list->string
  (letrec ((loop
             (lambda (s i l)
               (if (pair? l)
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

;;; String hash based on
;;;
;;; @inproceedings{ ramakrishna97performance,
;;;     author = "M. V. Ramakrishna and Justin Zobel",
;;;     title = "Performance in Practice of String Hashing Functions",
;;;     booktitle = "Database Systems for Advanced Applications",
;;;     pages = "215-224",
;;;     year = "1997",
;;;     url = "citeseer.ist.psu.edu/article/ramakrishna97performance.html" }

;;; Note, the stepping function is this:
;;;   hash_n+1 <- (fxlogxor hash_n (+ (shift-left hash_n 5)
;;;                                 (shift-right hash_n 2)
;;;                                 (string-ref string index)))
;;;

;;; The end result is a > 25% speedup in hashing, and a better
;;; distribution of hash values.  (Hashing a set of words from a
;;; dictionary showed fewer empty buckets, more buckets with exactly
;;; one entry and fewer buckets with three or more entries.)

; Returns a value in the range 0 .. 2^16-1 (a fixnum in Larceny).

; FIXME:  This code must be kept in sync with the definition of
; twobit-symbol-hash in Compiler/pass2if.sch.
; Any change to this code must be made there also, and vice versa.

(define (string-hash string)

  (define (string-hash-step code byte)
    (fxlogxor code
            ;; Avoid consing fixnums
            (let ((l (fxlsh (fxlogand code #x01FF) 5))
                  ;; R must be less than (+ (fxrshl #x01FF 2) 256)
                  (r (+ (fxrshl code 2) byte)))
              (if (> (- 16383 l) (- r 1))
                  (+ l r)
                  (+ (- l (- 16384 128)) (- r 128))))))

  (define (string-hash-loop string limit i code)
    (if (= i limit)
        code
        (string-hash-loop
         string limit (+ i 1)
         (string-hash-step code
                           (fxlogand #x00FF
                                     (char->integer (string-ref string i)))))))

  (let ((n (string-length string)))
    (string-hash-loop string n 0 (fxlogxor n #x1aa5))))

;;; This version (commented out) trades space for speed.  The problem
;;; is that fixnums take 16 bytes *each*, so keeping a shift table may
;;; be too expensive space-wise.
;;;
;;; The speed limiting factor (under dotnet) is not memory access,
;;; but number of primitive operations per step.  Thus we precompute
;;;  (+ (shift-left hash 5) (shift-right hash 2)) for the possible
;;; hash codes and just fetch them from a table.
;;;
;;; Additionally, we want the hash code to be in the range [0 2^16).
;;; To avoid a masking step, we limit the table entries to
;;; [0 (2^16 - 256)) so that adding in a byte from the string always
;;; leaves us with at most 16 bits.
'(define string-hash
  (let ((shift-table (make-vector 65536 0)))

    (define (string-hash-loop string limit i code)
      (if (= i limit)
          code
          (string-hash-loop
           string limit (+ i 1)
           (fxlogxor code
                   (+ (vector-ref shift-table code)
                      (bytevector-like-ref string i))))))

    (define (string-hash-internal string)
      (let ((n (string-length string)))
        (string-hash-loop string n 0 (fxlogxor n #x5aa5))))

    ;; C is zero every fourth time
    ;; This computation is funky to avoid straying into bignum land,
    ;; even briefly.
    (do ((c    0 (if (= c 3) 0 (+ c 1)))
         (sti  0 (+ sti 1))
         (stip 0 (if (= c 3)
                     (if (>= stip (- 65536 256 33))
                         (- stip (- 65536 256 33))
                         (+ stip 33))
                     (if (>= stip (- 65536 256 32))
                         (- stip (- 65536 256 32))
                         (+ stip 32)))))
        ((>= sti 65536))
      (vector-set! shift-table sti stip))

    string-hash-internal))

;(define (string-hash string)
;  (define (loop s i h)
;    (if (< i 0)
;	h
;	(loop s
;	      (- i 1)
;	      (fxlogand 65535 (+ (char->integer (string-ref s i)) h h h)))))
;  (let ((n (string-length string)))
;    (loop string (- n 1) n)))

(define (%string-downcase! src dest)
  (do ((i (- (string-length src) 1) (- i 1)))
      ((< i 0) dest)
    (let ((x (char->integer (string-ref src i))))
      (string-set! dest i (integer->char
                           (if (= 1 (bytevector-ref *char-table* x))
                               (+ x 32)
                               x))))))

(define (string-downcase! string)
  (%string-downcase! string string))

(define (string-downcase string)
  (%string-downcase! string (make-string (string-length string))))

(define (%string-upcase! src dest)
  (do ((i (- (string-length src) 1) (- i 1)))
      ((< i 0) dest)
    (let ((x (char->integer (string-ref src i))))
      (string-set! dest i (integer->char 
                           (if (= 2 (bytevector-ref *char-table* x))
                               (- x 32)
                               x))))))

(define (string-upcase! string)
  (%string-upcase! string string))

(define (string-upcase string)
  (%string-upcase! string (make-string (string-length string))))

(define list->bytevector
  (letrec ((loop
             (lambda (bv i l)
               (if (pair? l)
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
      (begin (error name ": Operands must be strings: " a " " b)
	     #t)
      (bytevector-like-compare a b)))


(define (bytevector-equal? a b)
  (if (not (bytevector? a))
      (error "bytevector-equal?: not a bytevector: " a))
  (if (not (bytevector? b))
      (error "bytevector-equal?: not a bytevector: " b))
  (zero? (bytevector-like-compare a b)))


(define (bytevector-copy b)
  (if (not (bytevector? b))
      (error "bytevector-copy: not a bytevector: " b))
  (bytevector-like-copy b))


(define (bytevector-like-equal? b1 b2)
  (zero? (bytevector-like-compare b1 b2)))


(define (bytevector-like-copy b)
  (let ((v (make-bytevector (bytevector-like-length b))))
    (typetag-set! v (typetag b))
    (bytevector-like-copy-into! b 0 (bytevector-like-length b) v 0)))


(define (bytevector-like-copy-into! src from lim dest to)
  (do ((i from (+ i 1))
       (j to   (+ j 1)))
      ((= i lim) dest)
    (bytevector-like-set! dest j (bytevector-like-ref src i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Unicode.
;
; Unicode characters are fully implemented,
; but Unicode strings are not.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; These ustring operations are for testing operations on Unicode
; string.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Although these definitions are temporary, we will probably keep
; the representation used here.  These operations will become a
; lot faster when the back ends generate native code for them.
;
; UPDATE: make-ustring is now representation-agnostic, and all the
; other procedures are primops.

;(define (make-ustring n . rest)
;  (let ((fill (if (null? rest) #\space (car rest))))
;    (if (not (char? fill))
;        (error "Bad fill argument to make-string: " fill))
;    (let* ((s (make-bytevector (* 4 n))))
;      ; FIXME: it's inefficient to make two passes,
;      ; but we have to preserve the ustring invariant that
;      ; every element of the string is a boxed immediate.
;      (do ((n (* 4 n))
;           (i 0 (+ i 1)))
;          ((= i n) s)
;        (bytevector-set! s i 0))      
;      (typetag-set! s sys$tag.ustring-typetag)
;      (do ((i 0 (+ i 1)))
;          ((= i n) s)
;        (ustring-set! s i fill)))))

; FIXME: I have no idea why the auto-generated definitions in
; layouts.sch aren't visible to this file.
;
;(define $imm.character 38)
;
;(define (ustring? x)
;  (or (string? x) ; FIXME: should go away
;      (and (bytevector-like? x)
;           (eq? (typetag x) sys$tag.ustring-typetag))))
;
; (define (make-ustring n . rest)
;   (let ((fill (if (null? rest) #\space (car rest))))
;     (if (not (char? fill))
;         (error "Bad fill argument to make-string: " fill))
;     (let* ((n (* 4 n))
;            (s (make-bytevector n))
;            (sv (char->integer fill))
;            (b0 (fxrshl sv 16))
;            (b1 (fxlogand (fxrshl sv 8) 255))
;            (b2 (fxlogand sv 255))
;            (b3 $imm.character)
;            (offsets (case (cdr (assq 'arch-endianness (system-features)))
;                       ((big)    (list 0 1 2 3))
;                       ((little) (list 3 2 1 0))))
;            (i0 (list-ref offsets 0))
;            (i1 (list-ref offsets 1))
;            (i2 (list-ref offsets 2))
;            (i3 (list-ref offsets 3)))
;       (do ((i 0 (+ i 4)))
;           ((= i n) (typetag-set! s sys$tag.ustring-typetag) s)
;         (bytevector-set! s (+ i i0) b0)
;         (bytevector-set! s (+ i i1) b1)
;         (bytevector-set! s (+ i i2) b2)
;         (bytevector-set! s (+ i i3) b3)))))

;; NB the implementations below assume a big-endian architecture, 
;; while the make-ustring implementation above is
;; endianness-dependent.

;(define (ustring-length s)
;  (cond ((string? s)
;         (string-length s))
;        ((ustring? s)
;         (fxrsha (bytevector-like-length s) 2))
;        (else
;         (error "Bad argument to string-length: " s))))

;(define (ustring-ref s i)
;  (cond ((string? s)
;         (string-ref s i))
;        ((ustring? s)
;         (let* ((n (fxlsh i 2))
;                (b0 (bytevector-like-ref s n))
;                (b1 (bytevector-like-ref s (+ n 1)))
;                (b2 (bytevector-like-ref s (+ n 2)))
;                (b3 (bytevector-like-ref s (+ n 3))))
;           (if (not (eq? b3 $imm.character))
;               (error "Bug in ustring operations: " b3))
;           (integer->char
;            (fxlogior (fxlogior (fxlsh b0 16) (fxlsh b1 8)) b2))))
;        (else
;         (error "Bad argument to string-ref: " s))))

;(define (ustring-set! s i c)
;  (cond ((string? s)
;         (error "Immutable string passed to string-set!: " s))
;        ((ustring? s)
;         (let* ((n (fxlsh i 2))
;                (sv (char->integer c))
;                (b0 (fxrshl sv 16))
;                (b1 (fxlogand (fxrshl sv 8) 255))
;                (b2 (fxlogand sv 255))
;                (b3 $imm.character))
;           (bytevector-like-set! s n b0)
;           (bytevector-like-set! s (+ n 1) b1)
;           (bytevector-like-set! s (+ n 2) b2)
;           (bytevector-like-set! s (+ n 3) $imm.character)))
;        (else
;         (error "Bad argument to string-set!: " s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Procedures that operate on characters.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Case-insensitive comparisons.

(define (char-ci=? c1 c2)
  (char=? (char-foldcase c1) (char-foldcase c2)))

(define (char-ci<? c1 c2)
  (char<? (char-foldcase c1) (char-foldcase c2)))

(define (char-ci>? c1 c2)
  (char>? (char-foldcase c1) (char-foldcase c2)))

(define (char-ci<=? c1 c2)
  (char<=? (char-foldcase c1) (char-foldcase c2)))

(define (char-ci>=? c1 c2)
  (char>=? (char-foldcase c1) (char-foldcase c2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Simple (character-to-character) case conversions.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-upcase c)
  (let ((n (char->integer c)))
    (cond ((<= #x61 n #x7a)
           (integer->char (- n #x20)))
          ((< n #xb5)
           c)
          ((<= #xe0 n #xfe)
           (integer->char (- n #x20)))
          ((= n #xb5)
           (integer->char #x039c))
          ((< n #xff)
           c)
          (else
           (let ((probe
                  (if (<= n #xffff)
                      (binary-search-16bit n simple-upcase-chars-16bit)
                      (binary-search-of-vector
                       n simple-upcase-chars-morebits))))
             (if probe
                 (integer->char
                  (+ n (vector-ref simple-case-adjustments
                                   (bytevector-ref simple-upcase-adjustments
                                                 probe))))
                 c))))))

(define (char-downcase c)
  (let ((n (char->integer c)))
    (cond ((<= #x41 n #x5a)
           (integer->char (+ n #x20)))
          ((< n #xc0)
           c)
          ((<= n #xde)
           (integer->char (+ n #x20)))
          ((< n #xff)
           c)
          (else
           (let ((probe
                  (if (<= n #xffff)
                      (binary-search-16bit n simple-downcase-chars-16bit)
                      (binary-search-of-vector
                       n simple-downcase-chars-morebits))))
             (if probe
                 (integer->char
                  (- n (vector-ref simple-case-adjustments
                                   (bytevector-ref simple-downcase-adjustments
                                                 probe))))
                 c))))))

; FIXME:
; Hard-coding the exceptional code points into this procedure
; means this procedure will have to be revisited whenever the
; Unicode database is changed.

(define (char-titlecase c)
  (let ((n (char->integer c)))
    (cond ((< n #x01c4)
           (char-upcase c))
          ((> n #x01f3)
           (char-upcase c))
          (else
           (case n
             ((#x01c4 #x01c5 #x01c6)
              (integer->char #x01c5))
             ((#x01c7 #x01c8 #x01c9)
              (integer->char #x01c8))
             ((#x01ca #x01cb #x01cc)
              (integer->char #x01cb))
             ((#x01f1 #x01f2 #x01f3)
              (integer->char #x01f2))
             (else
              (char-upcase c)))))))

; ASCII characters are treated specially, making them about
; twice as fast as the general case.

(define (char-foldcase c)
  (let ((cp (char->integer c)))
    (if (< cp #xb5)
        (char-downcase c)
        (case (char->integer c)
         ((#x130 #x131) c)
         (else
          (char-downcase (char-upcase c)))))))

; Given a character, returns its Unicode general category.
; The tables used to implement this procedure occupy about 12440 bytes.
; 4408 of those bytes could be saved by splitting the large vector
; into a bytes object for the 16-bit scalar values and using a
; general vector only for the scalar values greater than 65535.

(define (char-general-category c)
  (let ((n (char->integer c)))
    (vector-ref
     vector-of-general-category-symbols
     (if (< n (bytevector-length
               general-category-indices-for-common-characters))
         (bytevector-ref general-category-indices-for-common-characters n)
         (bytevector-ref general-category-indices-for-all-characters
                       (binary-search-of-vector
                        n
                        vector-of-code-points-with-same-category))))))

; Given a character, returns true if and only if the character
; is alphabetic, numeric, whitespace, upper-case, lower-case,
; or title-case respectively.

(define (char-alphabetic? c)
  (if (memq (char-general-category c)
            '(Lu Ll Lt Lm Lo))
      #t
      #f))

(define (char-numeric? c)
  (eq? (char-general-category c)
       'Nd))

(define (char-whitespace? c)
  (let ((k (char->integer c)))
    (if (< k 256)
        (case k
         ((#x09 #x0a #x0b #x0c #x0d #x20 #xa0) #t)
         (else #f))
        (if (memq (char-general-category c)
                  '(Zs Zl Zp))
            #t
            #f))))

(define (char-upper-case? c)
  (eq? 'Lu (char-general-category c)))

(define (char-lower-case? c)
  (eq? 'Ll (char-general-category c)))

(define (char-title-case? c)
  (eq? 'Lt (char-general-category c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Help procedures and tables (not part of R6RS)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given an exact integer key and a vector of exact integers
; in strictly increasing order, returns the largest i such
; that element i of the vector is less than or equal to key,
; or -1 if key is less than every element of the vector.

(define (binary-search-of-vector key vec)

  ; Loop invariants:
  ; 0 <= i < j <= (vector-length vec)
  ; vec[i] <= key
  ; if j < (vector-length vec), then key < vec[j]

  (define (loop i j)
    (let ((mid (quotient (+ i j) 2)))
      (cond ((= i mid)
             mid)
            ((<= (vector-ref vec mid) key)
             (loop mid j))
            (else
             (loop i mid)))))

  (let ((hi (vector-length vec)))
    (if (or (= hi 0) (< key (vector-ref vec 0)))
        -1
        (loop 0 hi))))

; Given an exact integer key and a bytevector of 16-bit unsigned
; big-endian integers in strictly increasing order,
; returns i such that elements (* 2 i) and (+ (* 2 i) 1)
; are equal to the key, or returns #f if the key is not found.

(define (binary-search-16bit key bytes)

  (define (bytes-ref16 bytes i)
    (let ((i2 (+ i i)))
      (+ (* 256 (bytevector-ref bytes i2))
         (bytevector-ref bytes (+ i2 1)))))

  (define (loop i j)
    (let ((mid (quotient (+ i j) 2)))
      (cond ((= i mid)
             (if (= (bytes-ref16 bytes mid) key)
                 mid
                 #f))
            ((<= (bytes-ref16 bytes mid) key)
             (loop mid j))
            (else
             (loop i mid)))))

  (let ((hi (quotient (bytevector-length bytes) 2)))
    (if (or (= hi 0) (< key (bytes-ref16 bytes 0)))
        #f
        (loop 0 hi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Unicode general properties.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The symbols that represent Unicode general properties.
; There are 30 of these.
; This table occupies about 128 bytes, not counting
; the space occupied by the symbols themselves.

(define vector-of-general-category-symbols
  '#(
     ; Letter: uppercase, lowercase, titlecase, modifier, other
     Lu Ll Lt Lm Lo

     ; Mark: nonspacing, spacing combining, enclosing
     Mn Mc Me

     ; Number: decimal digit, letter, other
     Nd Nl No

     ; Punctuation: connector, dash, open, close,
     ;     initial quote, final quote, other
     Pc Pd Ps Pe Pi Pf Po

     ; Symbol: math, currency, modifier, other
     Sm Sc Sk So

     ; Separator: space, line, paragraph
     Zs Zl Zp

     ; Other: control, format, surrogate, private use, not assigned
     Cc Cf Cs Co Cn))

; Given a symbol that appears in the vector above,
; returns its index within the vector.
; Used only for initialization, so it needn't be fast.

(define (general-category-symbol->index sym)
  (let ((n (vector-length vector-of-general-category-symbols)))
    (do ((i 0 (+ i 1)))
        ((or (= i n)
             (eq? sym (vector-ref vector-of-general-category-symbols i)))
         (if (= i n)
             (error "Unrecognized Unicode general category" sym)
             i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The following tables were generated from
; UnicodeData.txt and SpecialCasing.txt.
; Use parseUCD.sch to regenerate these tables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following vector contains the general category for
; characters whose Unicode scalar value is less than 256.
;
; This table contains 256 entries.

(define general-category-indices-for-common-characters
  (list->bytevector
   (map
    general-category-symbol->index
    '(
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Zs Po Po Po Sc Po Po Po Ps Pe Po Sm Po Pd Po Po 
       Nd Nd Nd Nd Nd Nd Nd Nd Nd Nd Po Po Sm Sm Sm Po 
       Po Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu 
       Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Ps Po Pe Sk Pc 
       Sk Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll 
       Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ps Sm Pe Sm Cc 
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc Cc 
       Zs Po Sc Sc Sc Sc So So Sk So Ll Pi Sm Cf So Sk 
       So Sm No No Sk Ll So Po Sk No Ll Pf No No No Po 
       Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu Lu 
       Lu Lu Lu Lu Lu Lu Lu Sm Lu Lu Lu Lu Lu Lu Lu Ll 
       Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll Ll 
       Ll Ll Ll Ll Ll Ll Ll Sm Ll Ll Ll Ll Ll Ll Ll Ll ))))

; The following array of bytes, together with the vector below it,
; implements an indirect mapping from all Unicode scalar values to
; indices into the above vector.
;
; This table contains 2408 entries.

(define general-category-indices-for-all-characters
  (list->bytevector
   (map
    general-category-symbol->index
    '(
      Cc Zs Po Sc Po Ps Pe Po Sm Po Pd Po Nd Po Sm Po 
      Lu Ps Po Pe Sk Pc Sk Ll Ps Sm Pe Sm Cc Zs Po Sc 
      So Sk So Ll Pi Sm Cf So Sk So Sm No Sk Ll So Po 
      Sk No Ll Pf No Po Lu Sm Lu Ll Sm Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lo Lu Ll Lo 
      Lu Lt Ll Lu Lt Ll Lu Lt Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Lt Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Cn Ll Lm Sk Lm Sk Lm Sk Lm Sk Mn Cn Sk Cn Lm 
      Cn Po Cn Sk Lu Po Lu Cn Lu Cn Lu Ll Lu Cn Lu Ll 
      Cn Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Sm Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll So Mn Cn Me Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Cn 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Cn Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Cn Lu Cn Lm Po 
      Cn Ll Cn Po Pd Cn Mn Cn Mn Po Mn Po Mn Po Mn Po 
      Mn Cn Lo Cn Lo Po Cn Cf Cn Sc Po So Mn Cn Po Cn 
      Po Cn Lo Cn Lm Lo Mn Cn Nd Po Lo Mn Lo Po Lo Mn 
      Cf Me Mn Lm Mn So Mn Lo Nd Lo So Lo Po Cn Cf Lo 
      Mn Lo Mn Cn Lo Cn Lo Mn Lo Cn Mn Mc Lo Cn Mn Lo 
      Mc Mn Mc Mn Cn Lo Mn Cn Lo Mn Po Nd Po Cn Lo Cn 
      Mn Mc Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn 
      Lo Mc Mn Cn Mc Cn Mc Mn Lo Cn Mc Cn Lo Cn Lo Mn 
      Cn Nd Lo Sc No So Cn Mn Mc Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Mn Cn Mc Mn Cn Mn Cn Mn 
      Cn Lo Cn Lo Cn Nd Mn Lo Cn Mn Mc Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc Mn Cn Mn Mc Cn 
      Mc Mn Cn Lo Cn Lo Mn Cn Nd Cn Sc Cn Mn Mc Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc Mn Mc 
      Mn Cn Mc Cn Mc Mn Cn Mn Mc Cn Lo Cn Lo Cn Nd So 
      Lo Cn Mn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Mc Mn Mc Cn Mc Cn Mc Mn Cn 
      Mc Cn Nd No So Sc So Cn Mc Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Mn Mc Cn Mn Cn Mn Cn Mn Cn Lo Cn Nd 
      Cn Mc Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Mn Lo Mc 
      Mn Mc Cn Mn Mc Cn Mc Mn Cn Mc Cn Lo Cn Lo Cn Nd 
      Cn Mc Cn Lo Cn Lo Cn Lo Cn Lo Cn Mc Mn Cn Mc Cn 
      Mc Mn Cn Mc Cn Lo Cn Nd Cn Mc Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Mn Cn Mc Mn Cn Mn Cn Mc Cn Mc Po 
      Cn Lo Mn Lo Mn Cn Sc Lo Lm Mn Po Nd Po Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Mn Lo Mn Cn Mn Lo Cn Lo Cn Lm Cn 
      Mn Cn Nd Cn Lo Cn Lo So Po So Mn So Nd No So Mn 
      So Mn So Mn Ps Pe Ps Pe Mc Lo Cn Lo Cn Mn Mc Mn 
      Po Mn Lo Cn Mn Cn Mn Cn So Mn So Cn So Po Cn Lo 
      Cn Lo Cn Lo Cn Mc Mn Mc Mn Cn Mn Mc Mn Cn Nd Po 
      Lo Mc Mn Cn Lu Cn Lo Po Lm Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn 
      Mn So Po No Cn Lo So Cn Lo Cn Lo Po Lo Cn Zs Lo 
      Ps Pe Cn Lo Po Nl Cn Lo Cn Lo Mn Cn Lo Mn Po Cn 
      Lo Mn Cn Lo Cn Lo Cn Mn Cn Lo Cf Mc Mn Mc Mn Mc 
      Mn Po Lm Po Sc Lo Mn Cn Nd Cn No Cn Po Pd Po Mn 
      Zs Cn Nd Cn Lo Lm Lo Cn Lo Mn Cn Lo Cn Mn Mc Mn 
      Mc Cn Mc Mn Mc Mn Cn So Cn Po Nd Lo Cn Lo Cn Lo 
      Cn Mc Lo Mc Cn Nd Cn Po So Lo Mn Mc Cn Po Cn Ll 
      Lm Ll Lm Ll Lm Mn Cn Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Cn Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll 
      Lu Ll Lu Ll Lu Ll Lu Ll Cn Ll Lu Ll Cn Lu Cn Ll 
      Lu Ll Lu Ll Cn Lu Cn Ll Cn Lu Cn Lu Cn Lu Cn Lu 
      Ll Lu Ll Cn Ll Lt Ll Lt Ll Lt Ll Cn Ll Lu Lt Sk 
      Ll Sk Ll Cn Ll Lu Lt Sk Ll Cn Ll Lu Cn Sk Ll Lu 
      Sk Cn Ll Cn Ll Lu Lt Sk Cn Zs Cf Pd Po Pi Pf Ps 
      Pi Pf Ps Pi Po Zl Zp Cf Zs Po Pi Pf Po Pc Po Sm 
      Ps Pe Po Sm Po Pc Po Zs Cf Cn Cf No Ll Cn No Sm 
      Ps Pe Ll No Sm Ps Pe Cn Lm Cn Sc Cn Mn Me Mn Me 
      Mn Cn So Lu So Lu So Ll Lu Ll Lu Ll So Lu So Lu 
      So Lu So Lu So Lu So Lu So Ll Lu So Lu Ll Lo Ll 
      So Ll Lu Sm Lu Ll So Sm So Cn No Nl Cn Sm So Sm 
      So Sm So Sm So Sm So Sm So Sm So Sm So Sm So Sm 
      So Sm So Sm So Ps Pe So Sm So Sm Ps Pe Po So Cn 
      So Cn So Cn No So No So Sm So Sm So Sm So Sm So 
      Cn So Cn So Cn So Cn So Cn So Cn So Cn So Cn So 
      Cn So Cn So Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe 
      Ps Pe No So Cn So Cn So Cn Sm Ps Pe Cn Sm Ps Pe 
      Ps Pe Ps Pe Cn Sm So Sm Ps Pe Ps Pe Ps Pe Ps Pe 
      Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Sm Ps 
      Pe Ps Pe Sm Ps Pe Sm So Cn Lu Cn Ll Cn Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu 
      Ll So Cn Po No Po Ll Cn Lo Cn Lm Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Po Pi 
      Pf Pi Pf Po Pi Pf Po Pi Pf Po Pd Cn Pi Pf Cn So 
      Cn So Cn So Cn So Cn Zs Po So Lm Lo Nl Ps Pe Ps 
      Pe Ps Pe Ps Pe Ps Pe So Ps Pe Ps Pe Ps Pe Ps Pe 
      Pd Ps Pe So Nl Mn Pd Lm So Nl Lm Lo Po So Cn Lo 
      Cn Mn Sk Lm Lo Pd Lo Po Lm Lo Cn Lo Cn Lo Cn So 
      No So Lo Cn So Cn Lo So Cn No So Cn So No So No 
      So No So Cn So Lo Cn So Lo Cn Lo Lm Lo Cn So Cn 
      Sk Cn Lo Mc Lo Mn Lo Mn Lo Mc Mn Mc So Cn Lo Cn 
      Cs Co Lo Cn Lo Cn Lo Cn Ll Cn Ll Cn Lo Mn Lo Sm 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Lo Ps Pe Cn 
      Lo Cn Lo Cn Lo Sc So Cn Mn Po Ps Pe Po Cn Mn Cn 
      Po Pd Pc Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps Pe Ps 
      Pe Ps Pe Po Ps Pe Po Pc Po Cn Po Pd Ps Pe Ps Pe 
      Ps Pe Po Sm Pd Sm Cn Po Sc Po Cn Lo Cn Lo Cn Cf 
      Cn Po Sc Po Ps Pe Po Sm Po Pd Po Nd Po Sm Po Lu 
      Ps Po Pe Sk Pc Sk Ll Ps Sm Pe Sm Ps Pe Po Ps Pe 
      Po Lo Lm Lo Lm Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Sc 
      Sm Sk So Sc Cn So Sm So Cn Cf So Cn Lo Cn Lo Cn 
      Lo Cn Lo Cn Lo Cn Lo Cn Lo Cn Po So Cn No Cn So 
      Nl No So No Cn Lo Cn No Cn Lo Nl Cn Lo Cn Po Lo 
      Cn Lo So Nl Cn Lu Ll Lo Cn Nd Cn Lo Cn Lo Cn Lo 
      Cn Lo Cn Lo Cn Lo Cn Lo Mn Cn Mn Cn Mn Lo Cn Lo 
      Cn Lo Cn Mn Cn Mn No Cn Po Cn So Cn So Cn So Mc 
      Mn So Mc Cf Mn So Mn So Mn So Cn So Mn So Cn So 
      Cn Lu Ll Lu Ll Cn Ll Lu Ll Lu Cn Lu Cn Lu Cn Lu 
      Cn Lu Cn Lu Ll Cn Ll Cn Ll Cn Ll Lu Ll Lu Cn Lu 
      Cn Lu Cn Lu Cn Ll Lu Cn Lu Cn Lu Cn Lu Cn Lu Cn 
      Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Lu Ll Cn Lu Sm 
      Ll Sm Ll Lu Sm Ll Sm Ll Lu Sm Ll Sm Ll Lu Sm Ll 
      Sm Ll Lu Sm Ll Sm Ll Cn Nd Cn Lo Cn Lo Cn Cf Cn 
      Cf Cn Mn Cn Co Cn Co Cn ))))

; The following vector of exact integers represents the
; Unicode scalar values whose Unicode general category
; is different from the Unicode scalar value immediately
; less than it.
;
; This table contains 2408 entries.

(define vector-of-code-points-with-same-category
  '#(
     #x0 #x20 #x21 #x24 #x25 #x28 #x29 #x2a 
     #x2b #x2c #x2d #x2e #x30 #x3a #x3c #x3f 
     #x41 #x5b #x5c #x5d #x5e #x5f #x60 #x61 
     #x7b #x7c #x7d #x7e #x7f #xa0 #xa1 #xa2 
     #xa6 #xa8 #xa9 #xaa #xab #xac #xad #xae 
     #xaf #xb0 #xb1 #xb2 #xb4 #xb5 #xb6 #xb7 
     #xb8 #xb9 #xba #xbb #xbc #xbf #xc0 #xd7 
     #xd8 #xdf #xf7 #xf8 #x100 #x101 #x102 #x103 
     #x104 #x105 #x106 #x107 #x108 #x109 #x10a #x10b 
     #x10c #x10d #x10e #x10f #x110 #x111 #x112 #x113 
     #x114 #x115 #x116 #x117 #x118 #x119 #x11a #x11b 
     #x11c #x11d #x11e #x11f #x120 #x121 #x122 #x123 
     #x124 #x125 #x126 #x127 #x128 #x129 #x12a #x12b 
     #x12c #x12d #x12e #x12f #x130 #x131 #x132 #x133 
     #x134 #x135 #x136 #x137 #x139 #x13a #x13b #x13c 
     #x13d #x13e #x13f #x140 #x141 #x142 #x143 #x144 
     #x145 #x146 #x147 #x148 #x14a #x14b #x14c #x14d 
     #x14e #x14f #x150 #x151 #x152 #x153 #x154 #x155 
     #x156 #x157 #x158 #x159 #x15a #x15b #x15c #x15d 
     #x15e #x15f #x160 #x161 #x162 #x163 #x164 #x165 
     #x166 #x167 #x168 #x169 #x16a #x16b #x16c #x16d 
     #x16e #x16f #x170 #x171 #x172 #x173 #x174 #x175 
     #x176 #x177 #x178 #x17a #x17b #x17c #x17d #x17e 
     #x181 #x183 #x184 #x185 #x186 #x188 #x189 #x18c 
     #x18e #x192 #x193 #x195 #x196 #x199 #x19c #x19e 
     #x19f #x1a1 #x1a2 #x1a3 #x1a4 #x1a5 #x1a6 #x1a8 
     #x1a9 #x1aa #x1ac #x1ad #x1ae #x1b0 #x1b1 #x1b4 
     #x1b5 #x1b6 #x1b7 #x1b9 #x1bb #x1bc #x1bd #x1c0 
     #x1c4 #x1c5 #x1c6 #x1c7 #x1c8 #x1c9 #x1ca #x1cb 
     #x1cc #x1cd #x1ce #x1cf #x1d0 #x1d1 #x1d2 #x1d3 
     #x1d4 #x1d5 #x1d6 #x1d7 #x1d8 #x1d9 #x1da #x1db 
     #x1dc #x1de #x1df #x1e0 #x1e1 #x1e2 #x1e3 #x1e4 
     #x1e5 #x1e6 #x1e7 #x1e8 #x1e9 #x1ea #x1eb #x1ec 
     #x1ed #x1ee #x1ef #x1f1 #x1f2 #x1f3 #x1f4 #x1f5 
     #x1f6 #x1f9 #x1fa #x1fb #x1fc #x1fd #x1fe #x1ff 
     #x200 #x201 #x202 #x203 #x204 #x205 #x206 #x207 
     #x208 #x209 #x20a #x20b #x20c #x20d #x20e #x20f 
     #x210 #x211 #x212 #x213 #x214 #x215 #x216 #x217 
     #x218 #x219 #x21a #x21b #x21c #x21d #x21e #x21f 
     #x220 #x221 #x222 #x223 #x224 #x225 #x226 #x227 
     #x228 #x229 #x22a #x22b #x22c #x22d #x22e #x22f 
     #x230 #x231 #x232 #x233 #x23a #x23c #x23d #x23f 
     #x241 #x242 #x250 #x2b0 #x2c2 #x2c6 #x2d2 #x2e0 
     #x2e5 #x2ee #x2ef #x300 #x370 #x374 #x376 #x37a 
     #x37b #x37e #x37f #x384 #x386 #x387 #x388 #x38b 
     #x38c #x38d #x38e #x390 #x391 #x3a2 #x3a3 #x3ac 
     #x3cf #x3d0 #x3d2 #x3d5 #x3d8 #x3d9 #x3da #x3db 
     #x3dc #x3dd #x3de #x3df #x3e0 #x3e1 #x3e2 #x3e3 
     #x3e4 #x3e5 #x3e6 #x3e7 #x3e8 #x3e9 #x3ea #x3eb 
     #x3ec #x3ed #x3ee #x3ef #x3f4 #x3f5 #x3f6 #x3f7 
     #x3f8 #x3f9 #x3fb #x3fd #x430 #x460 #x461 #x462 
     #x463 #x464 #x465 #x466 #x467 #x468 #x469 #x46a 
     #x46b #x46c #x46d #x46e #x46f #x470 #x471 #x472 
     #x473 #x474 #x475 #x476 #x477 #x478 #x479 #x47a 
     #x47b #x47c #x47d #x47e #x47f #x480 #x481 #x482 
     #x483 #x487 #x488 #x48a #x48b #x48c #x48d #x48e 
     #x48f #x490 #x491 #x492 #x493 #x494 #x495 #x496 
     #x497 #x498 #x499 #x49a #x49b #x49c #x49d #x49e 
     #x49f #x4a0 #x4a1 #x4a2 #x4a3 #x4a4 #x4a5 #x4a6 
     #x4a7 #x4a8 #x4a9 #x4aa #x4ab #x4ac #x4ad #x4ae 
     #x4af #x4b0 #x4b1 #x4b2 #x4b3 #x4b4 #x4b5 #x4b6 
     #x4b7 #x4b8 #x4b9 #x4ba #x4bb #x4bc #x4bd #x4be 
     #x4bf #x4c0 #x4c2 #x4c3 #x4c4 #x4c5 #x4c6 #x4c7 
     #x4c8 #x4c9 #x4ca #x4cb #x4cc #x4cd #x4ce #x4cf 
     #x4d0 #x4d1 #x4d2 #x4d3 #x4d4 #x4d5 #x4d6 #x4d7 
     #x4d8 #x4d9 #x4da #x4db #x4dc #x4dd #x4de #x4df 
     #x4e0 #x4e1 #x4e2 #x4e3 #x4e4 #x4e5 #x4e6 #x4e7 
     #x4e8 #x4e9 #x4ea #x4eb #x4ec #x4ed #x4ee #x4ef 
     #x4f0 #x4f1 #x4f2 #x4f3 #x4f4 #x4f5 #x4f6 #x4f7 
     #x4f8 #x4f9 #x4fa #x500 #x501 #x502 #x503 #x504 
     #x505 #x506 #x507 #x508 #x509 #x50a #x50b #x50c 
     #x50d #x50e #x50f #x510 #x531 #x557 #x559 #x55a 
     #x560 #x561 #x588 #x589 #x58a #x58b #x591 #x5ba 
     #x5bb #x5be #x5bf #x5c0 #x5c1 #x5c3 #x5c4 #x5c6 
     #x5c7 #x5c8 #x5d0 #x5eb #x5f0 #x5f3 #x5f5 #x600 
     #x604 #x60b #x60c #x60e #x610 #x616 #x61b #x61c 
     #x61e #x620 #x621 #x63b #x640 #x641 #x64b #x65f 
     #x660 #x66a #x66e #x670 #x671 #x6d4 #x6d5 #x6d6 
     #x6dd #x6de #x6df #x6e5 #x6e7 #x6e9 #x6ea #x6ee 
     #x6f0 #x6fa #x6fd #x6ff #x700 #x70e #x70f #x710 
     #x711 #x712 #x730 #x74b #x74d #x76e #x780 #x7a6 
     #x7b1 #x7b2 #x901 #x903 #x904 #x93a #x93c #x93d 
     #x93e #x941 #x949 #x94d #x94e #x950 #x951 #x955 
     #x958 #x962 #x964 #x966 #x970 #x971 #x97d #x97e 
     #x981 #x982 #x984 #x985 #x98d #x98f #x991 #x993 
     #x9a9 #x9aa #x9b1 #x9b2 #x9b3 #x9b6 #x9ba #x9bc 
     #x9bd #x9be #x9c1 #x9c5 #x9c7 #x9c9 #x9cb #x9cd 
     #x9ce #x9cf #x9d7 #x9d8 #x9dc #x9de #x9df #x9e2 
     #x9e4 #x9e6 #x9f0 #x9f2 #x9f4 #x9fa #x9fb #xa01 
     #xa03 #xa04 #xa05 #xa0b #xa0f #xa11 #xa13 #xa29 
     #xa2a #xa31 #xa32 #xa34 #xa35 #xa37 #xa38 #xa3a 
     #xa3c #xa3d #xa3e #xa41 #xa43 #xa47 #xa49 #xa4b 
     #xa4e #xa59 #xa5d #xa5e #xa5f #xa66 #xa70 #xa72 
     #xa75 #xa81 #xa83 #xa84 #xa85 #xa8e #xa8f #xa92 
     #xa93 #xaa9 #xaaa #xab1 #xab2 #xab4 #xab5 #xaba 
     #xabc #xabd #xabe #xac1 #xac6 #xac7 #xac9 #xaca 
     #xacb #xacd #xace #xad0 #xad1 #xae0 #xae2 #xae4 
     #xae6 #xaf0 #xaf1 #xaf2 #xb01 #xb02 #xb04 #xb05 
     #xb0d #xb0f #xb11 #xb13 #xb29 #xb2a #xb31 #xb32 
     #xb34 #xb35 #xb3a #xb3c #xb3d #xb3e #xb3f #xb40 
     #xb41 #xb44 #xb47 #xb49 #xb4b #xb4d #xb4e #xb56 
     #xb57 #xb58 #xb5c #xb5e #xb5f #xb62 #xb66 #xb70 
     #xb71 #xb72 #xb82 #xb83 #xb84 #xb85 #xb8b #xb8e 
     #xb91 #xb92 #xb96 #xb99 #xb9b #xb9c #xb9d #xb9e 
     #xba0 #xba3 #xba5 #xba8 #xbab #xbae #xbba #xbbe 
     #xbc0 #xbc1 #xbc3 #xbc6 #xbc9 #xbca #xbcd #xbce 
     #xbd7 #xbd8 #xbe6 #xbf0 #xbf3 #xbf9 #xbfa #xbfb 
     #xc01 #xc04 #xc05 #xc0d #xc0e #xc11 #xc12 #xc29 
     #xc2a #xc34 #xc35 #xc3a #xc3e #xc41 #xc45 #xc46 
     #xc49 #xc4a #xc4e #xc55 #xc57 #xc60 #xc62 #xc66 
     #xc70 #xc82 #xc84 #xc85 #xc8d #xc8e #xc91 #xc92 
     #xca9 #xcaa #xcb4 #xcb5 #xcba #xcbc #xcbd #xcbe 
     #xcbf #xcc0 #xcc5 #xcc6 #xcc7 #xcc9 #xcca #xccc 
     #xcce #xcd5 #xcd7 #xcde #xcdf #xce0 #xce2 #xce6 
     #xcf0 #xd02 #xd04 #xd05 #xd0d #xd0e #xd11 #xd12 
     #xd29 #xd2a #xd3a #xd3e #xd41 #xd44 #xd46 #xd49 
     #xd4a #xd4d #xd4e #xd57 #xd58 #xd60 #xd62 #xd66 
     #xd70 #xd82 #xd84 #xd85 #xd97 #xd9a #xdb2 #xdb3 
     #xdbc #xdbd #xdbe #xdc0 #xdc7 #xdca #xdcb #xdcf 
     #xdd2 #xdd5 #xdd6 #xdd7 #xdd8 #xde0 #xdf2 #xdf4 
     #xdf5 #xe01 #xe31 #xe32 #xe34 #xe3b #xe3f #xe40 
     #xe46 #xe47 #xe4f #xe50 #xe5a #xe5c #xe81 #xe83 
     #xe84 #xe85 #xe87 #xe89 #xe8a #xe8b #xe8d #xe8e 
     #xe94 #xe98 #xe99 #xea0 #xea1 #xea4 #xea5 #xea6 
     #xea7 #xea8 #xeaa #xeac #xead #xeb1 #xeb2 #xeb4 
     #xeba #xebb #xebd #xebe #xec0 #xec5 #xec6 #xec7 
     #xec8 #xece #xed0 #xeda #xedc #xede #xf00 #xf01 
     #xf04 #xf13 #xf18 #xf1a #xf20 #xf2a #xf34 #xf35 
     #xf36 #xf37 #xf38 #xf39 #xf3a #xf3b #xf3c #xf3d 
     #xf3e #xf40 #xf48 #xf49 #xf6b #xf71 #xf7f #xf80 
     #xf85 #xf86 #xf88 #xf8c #xf90 #xf98 #xf99 #xfbd 
     #xfbe #xfc6 #xfc7 #xfcd #xfcf #xfd0 #xfd2 #x1000 
     #x1022 #x1023 #x1028 #x1029 #x102b #x102c #x102d #x1031 
     #x1032 #x1033 #x1036 #x1038 #x1039 #x103a #x1040 #x104a 
     #x1050 #x1056 #x1058 #x105a #x10a0 #x10c6 #x10d0 #x10fb 
     #x10fc #x10fd #x1100 #x115a #x115f #x11a3 #x11a8 #x11fa 
     #x1200 #x1249 #x124a #x124e #x1250 #x1257 #x1258 #x1259 
     #x125a #x125e #x1260 #x1289 #x128a #x128e #x1290 #x12b1 
     #x12b2 #x12b6 #x12b8 #x12bf #x12c0 #x12c1 #x12c2 #x12c6 
     #x12c8 #x12d7 #x12d8 #x1311 #x1312 #x1316 #x1318 #x135b 
     #x135f #x1360 #x1361 #x1369 #x137d #x1380 #x1390 #x139a 
     #x13a0 #x13f5 #x1401 #x166d #x166f #x1677 #x1680 #x1681 
     #x169b #x169c #x169d #x16a0 #x16eb #x16ee #x16f1 #x1700 
     #x170d #x170e #x1712 #x1715 #x1720 #x1732 #x1735 #x1737 
     #x1740 #x1752 #x1754 #x1760 #x176d #x176e #x1771 #x1772 
     #x1774 #x1780 #x17b4 #x17b6 #x17b7 #x17be #x17c6 #x17c7 
     #x17c9 #x17d4 #x17d7 #x17d8 #x17db #x17dc #x17dd #x17de 
     #x17e0 #x17ea #x17f0 #x17fa #x1800 #x1806 #x1807 #x180b 
     #x180e #x180f #x1810 #x181a #x1820 #x1843 #x1844 #x1878 
     #x1880 #x18a9 #x18aa #x1900 #x191d #x1920 #x1923 #x1927 
     #x1929 #x192c #x1930 #x1932 #x1933 #x1939 #x193c #x1940 
     #x1941 #x1944 #x1946 #x1950 #x196e #x1970 #x1975 #x1980 
     #x19aa #x19b0 #x19c1 #x19c8 #x19ca #x19d0 #x19da #x19de 
     #x19e0 #x1a00 #x1a17 #x1a19 #x1a1c #x1a1e #x1a20 #x1d00 
     #x1d2c #x1d62 #x1d78 #x1d79 #x1d9b #x1dc0 #x1dc4 #x1e00 
     #x1e01 #x1e02 #x1e03 #x1e04 #x1e05 #x1e06 #x1e07 #x1e08 
     #x1e09 #x1e0a #x1e0b #x1e0c #x1e0d #x1e0e #x1e0f #x1e10 
     #x1e11 #x1e12 #x1e13 #x1e14 #x1e15 #x1e16 #x1e17 #x1e18 
     #x1e19 #x1e1a #x1e1b #x1e1c #x1e1d #x1e1e #x1e1f #x1e20 
     #x1e21 #x1e22 #x1e23 #x1e24 #x1e25 #x1e26 #x1e27 #x1e28 
     #x1e29 #x1e2a #x1e2b #x1e2c #x1e2d #x1e2e #x1e2f #x1e30 
     #x1e31 #x1e32 #x1e33 #x1e34 #x1e35 #x1e36 #x1e37 #x1e38 
     #x1e39 #x1e3a #x1e3b #x1e3c #x1e3d #x1e3e #x1e3f #x1e40 
     #x1e41 #x1e42 #x1e43 #x1e44 #x1e45 #x1e46 #x1e47 #x1e48 
     #x1e49 #x1e4a #x1e4b #x1e4c #x1e4d #x1e4e #x1e4f #x1e50 
     #x1e51 #x1e52 #x1e53 #x1e54 #x1e55 #x1e56 #x1e57 #x1e58 
     #x1e59 #x1e5a #x1e5b #x1e5c #x1e5d #x1e5e #x1e5f #x1e60 
     #x1e61 #x1e62 #x1e63 #x1e64 #x1e65 #x1e66 #x1e67 #x1e68 
     #x1e69 #x1e6a #x1e6b #x1e6c #x1e6d #x1e6e #x1e6f #x1e70 
     #x1e71 #x1e72 #x1e73 #x1e74 #x1e75 #x1e76 #x1e77 #x1e78 
     #x1e79 #x1e7a #x1e7b #x1e7c #x1e7d #x1e7e #x1e7f #x1e80 
     #x1e81 #x1e82 #x1e83 #x1e84 #x1e85 #x1e86 #x1e87 #x1e88 
     #x1e89 #x1e8a #x1e8b #x1e8c #x1e8d #x1e8e #x1e8f #x1e90 
     #x1e91 #x1e92 #x1e93 #x1e94 #x1e95 #x1e9c #x1ea0 #x1ea1 
     #x1ea2 #x1ea3 #x1ea4 #x1ea5 #x1ea6 #x1ea7 #x1ea8 #x1ea9 
     #x1eaa #x1eab #x1eac #x1ead #x1eae #x1eaf #x1eb0 #x1eb1 
     #x1eb2 #x1eb3 #x1eb4 #x1eb5 #x1eb6 #x1eb7 #x1eb8 #x1eb9 
     #x1eba #x1ebb #x1ebc #x1ebd #x1ebe #x1ebf #x1ec0 #x1ec1 
     #x1ec2 #x1ec3 #x1ec4 #x1ec5 #x1ec6 #x1ec7 #x1ec8 #x1ec9 
     #x1eca #x1ecb #x1ecc #x1ecd #x1ece #x1ecf #x1ed0 #x1ed1 
     #x1ed2 #x1ed3 #x1ed4 #x1ed5 #x1ed6 #x1ed7 #x1ed8 #x1ed9 
     #x1eda #x1edb #x1edc #x1edd #x1ede #x1edf #x1ee0 #x1ee1 
     #x1ee2 #x1ee3 #x1ee4 #x1ee5 #x1ee6 #x1ee7 #x1ee8 #x1ee9 
     #x1eea #x1eeb #x1eec #x1eed #x1eee #x1eef #x1ef0 #x1ef1 
     #x1ef2 #x1ef3 #x1ef4 #x1ef5 #x1ef6 #x1ef7 #x1ef8 #x1ef9 
     #x1efa #x1f00 #x1f08 #x1f10 #x1f16 #x1f18 #x1f1e #x1f20 
     #x1f28 #x1f30 #x1f38 #x1f40 #x1f46 #x1f48 #x1f4e #x1f50 
     #x1f58 #x1f59 #x1f5a #x1f5b #x1f5c #x1f5d #x1f5e #x1f5f 
     #x1f60 #x1f68 #x1f70 #x1f7e #x1f80 #x1f88 #x1f90 #x1f98 
     #x1fa0 #x1fa8 #x1fb0 #x1fb5 #x1fb6 #x1fb8 #x1fbc #x1fbd 
     #x1fbe #x1fbf #x1fc2 #x1fc5 #x1fc6 #x1fc8 #x1fcc #x1fcd 
     #x1fd0 #x1fd4 #x1fd6 #x1fd8 #x1fdc #x1fdd #x1fe0 #x1fe8 
     #x1fed #x1ff0 #x1ff2 #x1ff5 #x1ff6 #x1ff8 #x1ffc #x1ffd 
     #x1fff #x2000 #x200b #x2010 #x2016 #x2018 #x2019 #x201a 
     #x201b #x201d #x201e #x201f #x2020 #x2028 #x2029 #x202a 
     #x202f #x2030 #x2039 #x203a #x203b #x203f #x2041 #x2044 
     #x2045 #x2046 #x2047 #x2052 #x2053 #x2054 #x2055 #x205f 
     #x2060 #x2064 #x206a #x2070 #x2071 #x2072 #x2074 #x207a 
     #x207d #x207e #x207f #x2080 #x208a #x208d #x208e #x208f 
     #x2090 #x2095 #x20a0 #x20b6 #x20d0 #x20dd #x20e1 #x20e2 
     #x20e5 #x20ec #x2100 #x2102 #x2103 #x2107 #x2108 #x210a 
     #x210b #x210e #x2110 #x2113 #x2114 #x2115 #x2116 #x2119 
     #x211e #x2124 #x2125 #x2126 #x2127 #x2128 #x2129 #x212a 
     #x212e #x212f #x2130 #x2132 #x2133 #x2134 #x2135 #x2139 
     #x213a #x213c #x213e #x2140 #x2145 #x2146 #x214a #x214b 
     #x214c #x214d #x2153 #x2160 #x2184 #x2190 #x2195 #x219a 
     #x219c #x21a0 #x21a1 #x21a3 #x21a4 #x21a6 #x21a7 #x21ae 
     #x21af #x21ce #x21d0 #x21d2 #x21d3 #x21d4 #x21d5 #x21f4 
     #x2300 #x2308 #x230c #x2320 #x2322 #x2329 #x232a #x232b 
     #x237c #x237d #x239b #x23b4 #x23b5 #x23b6 #x23b7 #x23dc 
     #x2400 #x2427 #x2440 #x244b #x2460 #x249c #x24ea #x2500 
     #x25b7 #x25b8 #x25c1 #x25c2 #x25f8 #x2600 #x266f #x2670 
     #x269d #x26a0 #x26b2 #x2701 #x2705 #x2706 #x270a #x270c 
     #x2728 #x2729 #x274c #x274d #x274e #x274f #x2753 #x2756 
     #x2757 #x2758 #x275f #x2761 #x2768 #x2769 #x276a #x276b 
     #x276c #x276d #x276e #x276f #x2770 #x2771 #x2772 #x2773 
     #x2774 #x2775 #x2776 #x2794 #x2795 #x2798 #x27b0 #x27b1 
     #x27bf #x27c0 #x27c5 #x27c6 #x27c7 #x27d0 #x27e6 #x27e7 
     #x27e8 #x27e9 #x27ea #x27eb #x27ec #x27f0 #x2800 #x2900 
     #x2983 #x2984 #x2985 #x2986 #x2987 #x2988 #x2989 #x298a 
     #x298b #x298c #x298d #x298e #x298f #x2990 #x2991 #x2992 
     #x2993 #x2994 #x2995 #x2996 #x2997 #x2998 #x2999 #x29d8 
     #x29d9 #x29da #x29db #x29dc #x29fc #x29fd #x29fe #x2b00 
     #x2b14 #x2c00 #x2c2f #x2c30 #x2c5f #x2c80 #x2c81 #x2c82 
     #x2c83 #x2c84 #x2c85 #x2c86 #x2c87 #x2c88 #x2c89 #x2c8a 
     #x2c8b #x2c8c #x2c8d #x2c8e #x2c8f #x2c90 #x2c91 #x2c92 
     #x2c93 #x2c94 #x2c95 #x2c96 #x2c97 #x2c98 #x2c99 #x2c9a 
     #x2c9b #x2c9c #x2c9d #x2c9e #x2c9f #x2ca0 #x2ca1 #x2ca2 
     #x2ca3 #x2ca4 #x2ca5 #x2ca6 #x2ca7 #x2ca8 #x2ca9 #x2caa 
     #x2cab #x2cac #x2cad #x2cae #x2caf #x2cb0 #x2cb1 #x2cb2 
     #x2cb3 #x2cb4 #x2cb5 #x2cb6 #x2cb7 #x2cb8 #x2cb9 #x2cba 
     #x2cbb #x2cbc #x2cbd #x2cbe #x2cbf #x2cc0 #x2cc1 #x2cc2 
     #x2cc3 #x2cc4 #x2cc5 #x2cc6 #x2cc7 #x2cc8 #x2cc9 #x2cca 
     #x2ccb #x2ccc #x2ccd #x2cce #x2ccf #x2cd0 #x2cd1 #x2cd2 
     #x2cd3 #x2cd4 #x2cd5 #x2cd6 #x2cd7 #x2cd8 #x2cd9 #x2cda 
     #x2cdb #x2cdc #x2cdd #x2cde #x2cdf #x2ce0 #x2ce1 #x2ce2 
     #x2ce3 #x2ce5 #x2ceb #x2cf9 #x2cfd #x2cfe #x2d00 #x2d26 
     #x2d30 #x2d66 #x2d6f #x2d70 #x2d80 #x2d97 #x2da0 #x2da7 
     #x2da8 #x2daf #x2db0 #x2db7 #x2db8 #x2dbf #x2dc0 #x2dc7 
     #x2dc8 #x2dcf #x2dd0 #x2dd7 #x2dd8 #x2ddf #x2e00 #x2e02 
     #x2e03 #x2e04 #x2e05 #x2e06 #x2e09 #x2e0a #x2e0b #x2e0c 
     #x2e0d #x2e0e #x2e17 #x2e18 #x2e1c #x2e1d #x2e1e #x2e80 
     #x2e9a #x2e9b #x2ef4 #x2f00 #x2fd6 #x2ff0 #x2ffc #x3000 
     #x3001 #x3004 #x3005 #x3006 #x3007 #x3008 #x3009 #x300a 
     #x300b #x300c #x300d #x300e #x300f #x3010 #x3011 #x3012 
     #x3014 #x3015 #x3016 #x3017 #x3018 #x3019 #x301a #x301b 
     #x301c #x301d #x301e #x3020 #x3021 #x302a #x3030 #x3031 
     #x3036 #x3038 #x303b #x303c #x303d #x303e #x3040 #x3041 
     #x3097 #x3099 #x309b #x309d #x309f #x30a0 #x30a1 #x30fb 
     #x30fc #x30ff #x3100 #x3105 #x312d #x3131 #x318f #x3190 
     #x3192 #x3196 #x31a0 #x31b8 #x31c0 #x31d0 #x31f0 #x3200 
     #x321f #x3220 #x322a #x3244 #x3250 #x3251 #x3260 #x3280 
     #x328a #x32b1 #x32c0 #x32ff #x3300 #x3400 #x4db6 #x4dc0 
     #x4e00 #x9fbc #xa000 #xa015 #xa016 #xa48d #xa490 #xa4c7 
     #xa700 #xa717 #xa800 #xa802 #xa803 #xa806 #xa807 #xa80b 
     #xa80c #xa823 #xa825 #xa827 #xa828 #xa82c #xac00 #xd7a4 
     #xd800 #xe000 #xf900 #xfa2e #xfa30 #xfa6b #xfa70 #xfada 
     #xfb00 #xfb07 #xfb13 #xfb18 #xfb1d #xfb1e #xfb1f #xfb29 
     #xfb2a #xfb37 #xfb38 #xfb3d #xfb3e #xfb3f #xfb40 #xfb42 
     #xfb43 #xfb45 #xfb46 #xfbb2 #xfbd3 #xfd3e #xfd3f #xfd40 
     #xfd50 #xfd90 #xfd92 #xfdc8 #xfdf0 #xfdfc #xfdfd #xfdfe 
     #xfe00 #xfe10 #xfe17 #xfe18 #xfe19 #xfe1a #xfe20 #xfe24 
     #xfe30 #xfe31 #xfe33 #xfe35 #xfe36 #xfe37 #xfe38 #xfe39 
     #xfe3a #xfe3b #xfe3c #xfe3d #xfe3e #xfe3f #xfe40 #xfe41 
     #xfe42 #xfe43 #xfe44 #xfe45 #xfe47 #xfe48 #xfe49 #xfe4d 
     #xfe50 #xfe53 #xfe54 #xfe58 #xfe59 #xfe5a #xfe5b #xfe5c 
     #xfe5d #xfe5e #xfe5f #xfe62 #xfe63 #xfe64 #xfe67 #xfe68 
     #xfe69 #xfe6a #xfe6c #xfe70 #xfe75 #xfe76 #xfefd #xfeff 
     #xff00 #xff01 #xff04 #xff05 #xff08 #xff09 #xff0a #xff0b 
     #xff0c #xff0d #xff0e #xff10 #xff1a #xff1c #xff1f #xff21 
     #xff3b #xff3c #xff3d #xff3e #xff3f #xff40 #xff41 #xff5b 
     #xff5c #xff5d #xff5e #xff5f #xff60 #xff61 #xff62 #xff63 
     #xff64 #xff66 #xff70 #xff71 #xff9e #xffa0 #xffbf #xffc2 
     #xffc8 #xffca #xffd0 #xffd2 #xffd8 #xffda #xffdd #xffe0 
     #xffe2 #xffe3 #xffe4 #xffe5 #xffe7 #xffe8 #xffe9 #xffed 
     #xffef #xfff9 #xfffc #xfffe #x10000 #x1000c #x1000d #x10027 
     #x10028 #x1003b #x1003c #x1003e #x1003f #x1004e #x10050 #x1005e 
     #x10080 #x100fb #x10100 #x10102 #x10103 #x10107 #x10134 #x10137 
     #x10140 #x10175 #x10179 #x1018a #x1018b #x10300 #x1031f #x10320 
     #x10324 #x10330 #x1034a #x1034b #x10380 #x1039e #x1039f #x103a0 
     #x103c4 #x103c8 #x103d0 #x103d1 #x103d6 #x10400 #x10428 #x10450 
     #x1049e #x104a0 #x104aa #x10800 #x10806 #x10808 #x10809 #x1080a 
     #x10836 #x10837 #x10839 #x1083c #x1083d #x1083f #x10840 #x10a00 
     #x10a01 #x10a04 #x10a05 #x10a07 #x10a0c #x10a10 #x10a14 #x10a15 
     #x10a18 #x10a19 #x10a34 #x10a38 #x10a3b #x10a3f #x10a40 #x10a48 
     #x10a50 #x10a59 #x1d000 #x1d0f6 #x1d100 #x1d127 #x1d12a #x1d165 
     #x1d167 #x1d16a #x1d16d #x1d173 #x1d17b #x1d183 #x1d185 #x1d18c 
     #x1d1aa #x1d1ae #x1d1de #x1d200 #x1d242 #x1d245 #x1d246 #x1d300 
     #x1d357 #x1d400 #x1d41a #x1d434 #x1d44e #x1d455 #x1d456 #x1d468 
     #x1d482 #x1d49c #x1d49d #x1d49e #x1d4a0 #x1d4a2 #x1d4a3 #x1d4a5 
     #x1d4a7 #x1d4a9 #x1d4ad #x1d4ae #x1d4b6 #x1d4ba #x1d4bb #x1d4bc 
     #x1d4bd #x1d4c4 #x1d4c5 #x1d4d0 #x1d4ea #x1d504 #x1d506 #x1d507 
     #x1d50b #x1d50d #x1d515 #x1d516 #x1d51d #x1d51e #x1d538 #x1d53a 
     #x1d53b #x1d53f #x1d540 #x1d545 #x1d546 #x1d547 #x1d54a #x1d551 
     #x1d552 #x1d56c #x1d586 #x1d5a0 #x1d5ba #x1d5d4 #x1d5ee #x1d608 
     #x1d622 #x1d63c #x1d656 #x1d670 #x1d68a #x1d6a6 #x1d6a8 #x1d6c1 
     #x1d6c2 #x1d6db #x1d6dc #x1d6e2 #x1d6fb #x1d6fc #x1d715 #x1d716 
     #x1d71c #x1d735 #x1d736 #x1d74f #x1d750 #x1d756 #x1d76f #x1d770 
     #x1d789 #x1d78a #x1d790 #x1d7a9 #x1d7aa #x1d7c3 #x1d7c4 #x1d7ca 
     #x1d7ce #x1d800 #x20000 #x2a6d7 #x2f800 #x2fa1e #xe0001 #xe0002 
     #xe0020 #xe0080 #xe0100 #xe01f0 #xf0000 #xffffe #x100000 #x10fffe ))

; The following array of bytes implements a direct mapping
; from small code points to indices into the above vector.
;
; This table contains 256 entries.

(define general-category-indices-for-common-characters
  (do ((i 0 (+ i 1))
       (bv (make-bytevector 256)))
      ((= i 256)
       bv)
    (bytevector-set! bv
                     i
                     (general-category-symbol->index
                      (char-general-category
                       (integer->char i))))))

; This vector contains the numerical adjustments to make
; when converting a character from one case to another.
; For conversions to uppercase or titlecase, add the
; adjustment contained in this vector.
; For conversions to lowercase, subtract the adjustment
; contained in this vector.
;
; This table contains 60 elements.

(define simple-case-adjustments
  '#(
     #x-1c60 #x-1c25 #x-12c #x-e8 #x-db #x-da #x-d9 #x-d6 
     #x-d5 #x-d3 #x-d2 #x-d1 #x-cf #x-ce #x-cd #x-cb 
     #x-ca #x-60 #x-56 #x-53 #x-50 #x-4f #x-40 #x-3f 
     #x-3e #x-3b #x-39 #x-36 #x-30 #x-2f #x-28 #x-26 
     #x-25 #x-20 #x-1f #x-1a #x-10 #x-2 #x-1 #x7 
     #x8 #x9 #x38 #x3c #x4a #x54 #x56 #x61 
     #x64 #x70 #x79 #x7e #x80 #x82 #xa3 #xc7 
     #x2e7 #x1d5d #x2046 #x20bf ))

; This bytevector uses two bytes per code point
; to list all 16-bit code points, in increasing order,
; that have a simple uppercase mapping.
;
; This table contains 1724 elements.

; FIXME: This and several other tables are created using
; list->bytevector because we can't rely on the #vu8(...)
; syntax for cross-compilation.

(define simple-upcase-chars-16bit
  (list->bytevector
      '(
        #x0 #x61 #x0 #x62 #x0 #x63 #x0 #x64 
        #x0 #x65 #x0 #x66 #x0 #x67 #x0 #x68 
        #x0 #x69 #x0 #x6a #x0 #x6b #x0 #x6c 
        #x0 #x6d #x0 #x6e #x0 #x6f #x0 #x70 
        #x0 #x71 #x0 #x72 #x0 #x73 #x0 #x74 
        #x0 #x75 #x0 #x76 #x0 #x77 #x0 #x78 
        #x0 #x79 #x0 #x7a #x0 #xb5 #x0 #xe0 
        #x0 #xe1 #x0 #xe2 #x0 #xe3 #x0 #xe4 
        #x0 #xe5 #x0 #xe6 #x0 #xe7 #x0 #xe8 
        #x0 #xe9 #x0 #xea #x0 #xeb #x0 #xec 
        #x0 #xed #x0 #xee #x0 #xef #x0 #xf0 
        #x0 #xf1 #x0 #xf2 #x0 #xf3 #x0 #xf4 
        #x0 #xf5 #x0 #xf6 #x0 #xf8 #x0 #xf9 
        #x0 #xfa #x0 #xfb #x0 #xfc #x0 #xfd 
        #x0 #xfe #x0 #xff #x1 #x1 #x1 #x3 
        #x1 #x5 #x1 #x7 #x1 #x9 #x1 #xb 
        #x1 #xd #x1 #xf #x1 #x11 #x1 #x13 
        #x1 #x15 #x1 #x17 #x1 #x19 #x1 #x1b 
        #x1 #x1d #x1 #x1f #x1 #x21 #x1 #x23 
        #x1 #x25 #x1 #x27 #x1 #x29 #x1 #x2b 
        #x1 #x2d #x1 #x2f #x1 #x31 #x1 #x33 
        #x1 #x35 #x1 #x37 #x1 #x3a #x1 #x3c 
        #x1 #x3e #x1 #x40 #x1 #x42 #x1 #x44 
        #x1 #x46 #x1 #x48 #x1 #x4b #x1 #x4d 
        #x1 #x4f #x1 #x51 #x1 #x53 #x1 #x55 
        #x1 #x57 #x1 #x59 #x1 #x5b #x1 #x5d 
        #x1 #x5f #x1 #x61 #x1 #x63 #x1 #x65 
        #x1 #x67 #x1 #x69 #x1 #x6b #x1 #x6d 
        #x1 #x6f #x1 #x71 #x1 #x73 #x1 #x75 
        #x1 #x77 #x1 #x7a #x1 #x7c #x1 #x7e 
        #x1 #x7f #x1 #x83 #x1 #x85 #x1 #x88 
        #x1 #x8c #x1 #x92 #x1 #x95 #x1 #x99 
        #x1 #x9a #x1 #x9e #x1 #xa1 #x1 #xa3 
        #x1 #xa5 #x1 #xa8 #x1 #xad #x1 #xb0 
        #x1 #xb4 #x1 #xb6 #x1 #xb9 #x1 #xbd 
        #x1 #xbf #x1 #xc5 #x1 #xc6 #x1 #xc8 
        #x1 #xc9 #x1 #xcb #x1 #xcc #x1 #xce 
        #x1 #xd0 #x1 #xd2 #x1 #xd4 #x1 #xd6 
        #x1 #xd8 #x1 #xda #x1 #xdc #x1 #xdd 
        #x1 #xdf #x1 #xe1 #x1 #xe3 #x1 #xe5 
        #x1 #xe7 #x1 #xe9 #x1 #xeb #x1 #xed 
        #x1 #xef #x1 #xf2 #x1 #xf3 #x1 #xf5 
        #x1 #xf9 #x1 #xfb #x1 #xfd #x1 #xff 
        #x2 #x1 #x2 #x3 #x2 #x5 #x2 #x7 
        #x2 #x9 #x2 #xb #x2 #xd #x2 #xf 
        #x2 #x11 #x2 #x13 #x2 #x15 #x2 #x17 
        #x2 #x19 #x2 #x1b #x2 #x1d #x2 #x1f 
        #x2 #x23 #x2 #x25 #x2 #x27 #x2 #x29 
        #x2 #x2b #x2 #x2d #x2 #x2f #x2 #x31 
        #x2 #x33 #x2 #x3c #x2 #x53 #x2 #x54 
        #x2 #x56 #x2 #x57 #x2 #x59 #x2 #x5b 
        #x2 #x60 #x2 #x63 #x2 #x68 #x2 #x69 
        #x2 #x6f #x2 #x72 #x2 #x75 #x2 #x80 
        #x2 #x83 #x2 #x88 #x2 #x8a #x2 #x8b 
        #x2 #x92 #x2 #x94 #x3 #x45 #x3 #xac 
        #x3 #xad #x3 #xae #x3 #xaf #x3 #xb1 
        #x3 #xb2 #x3 #xb3 #x3 #xb4 #x3 #xb5 
        #x3 #xb6 #x3 #xb7 #x3 #xb8 #x3 #xb9 
        #x3 #xba #x3 #xbb #x3 #xbc #x3 #xbd 
        #x3 #xbe #x3 #xbf #x3 #xc0 #x3 #xc1 
        #x3 #xc2 #x3 #xc3 #x3 #xc4 #x3 #xc5 
        #x3 #xc6 #x3 #xc7 #x3 #xc8 #x3 #xc9 
        #x3 #xca #x3 #xcb #x3 #xcc #x3 #xcd 
        #x3 #xce #x3 #xd0 #x3 #xd1 #x3 #xd5 
        #x3 #xd6 #x3 #xd9 #x3 #xdb #x3 #xdd 
        #x3 #xdf #x3 #xe1 #x3 #xe3 #x3 #xe5 
        #x3 #xe7 #x3 #xe9 #x3 #xeb #x3 #xed 
        #x3 #xef #x3 #xf0 #x3 #xf1 #x3 #xf2 
        #x3 #xf5 #x3 #xf8 #x3 #xfb #x4 #x30 
        #x4 #x31 #x4 #x32 #x4 #x33 #x4 #x34 
        #x4 #x35 #x4 #x36 #x4 #x37 #x4 #x38 
        #x4 #x39 #x4 #x3a #x4 #x3b #x4 #x3c 
        #x4 #x3d #x4 #x3e #x4 #x3f #x4 #x40 
        #x4 #x41 #x4 #x42 #x4 #x43 #x4 #x44 
        #x4 #x45 #x4 #x46 #x4 #x47 #x4 #x48 
        #x4 #x49 #x4 #x4a #x4 #x4b #x4 #x4c 
        #x4 #x4d #x4 #x4e #x4 #x4f #x4 #x50 
        #x4 #x51 #x4 #x52 #x4 #x53 #x4 #x54 
        #x4 #x55 #x4 #x56 #x4 #x57 #x4 #x58 
        #x4 #x59 #x4 #x5a #x4 #x5b #x4 #x5c 
        #x4 #x5d #x4 #x5e #x4 #x5f #x4 #x61 
        #x4 #x63 #x4 #x65 #x4 #x67 #x4 #x69 
        #x4 #x6b #x4 #x6d #x4 #x6f #x4 #x71 
        #x4 #x73 #x4 #x75 #x4 #x77 #x4 #x79 
        #x4 #x7b #x4 #x7d #x4 #x7f #x4 #x81 
        #x4 #x8b #x4 #x8d #x4 #x8f #x4 #x91 
        #x4 #x93 #x4 #x95 #x4 #x97 #x4 #x99 
        #x4 #x9b #x4 #x9d #x4 #x9f #x4 #xa1 
        #x4 #xa3 #x4 #xa5 #x4 #xa7 #x4 #xa9 
        #x4 #xab #x4 #xad #x4 #xaf #x4 #xb1 
        #x4 #xb3 #x4 #xb5 #x4 #xb7 #x4 #xb9 
        #x4 #xbb #x4 #xbd #x4 #xbf #x4 #xc2 
        #x4 #xc4 #x4 #xc6 #x4 #xc8 #x4 #xca 
        #x4 #xcc #x4 #xce #x4 #xd1 #x4 #xd3 
        #x4 #xd5 #x4 #xd7 #x4 #xd9 #x4 #xdb 
        #x4 #xdd #x4 #xdf #x4 #xe1 #x4 #xe3 
        #x4 #xe5 #x4 #xe7 #x4 #xe9 #x4 #xeb 
        #x4 #xed #x4 #xef #x4 #xf1 #x4 #xf3 
        #x4 #xf5 #x4 #xf7 #x4 #xf9 #x5 #x1 
        #x5 #x3 #x5 #x5 #x5 #x7 #x5 #x9 
        #x5 #xb #x5 #xd #x5 #xf #x5 #x61 
        #x5 #x62 #x5 #x63 #x5 #x64 #x5 #x65 
        #x5 #x66 #x5 #x67 #x5 #x68 #x5 #x69 
        #x5 #x6a #x5 #x6b #x5 #x6c #x5 #x6d 
        #x5 #x6e #x5 #x6f #x5 #x70 #x5 #x71 
        #x5 #x72 #x5 #x73 #x5 #x74 #x5 #x75 
        #x5 #x76 #x5 #x77 #x5 #x78 #x5 #x79 
        #x5 #x7a #x5 #x7b #x5 #x7c #x5 #x7d 
        #x5 #x7e #x5 #x7f #x5 #x80 #x5 #x81 
        #x5 #x82 #x5 #x83 #x5 #x84 #x5 #x85 
        #x5 #x86 #x1e #x1 #x1e #x3 #x1e #x5 
        #x1e #x7 #x1e #x9 #x1e #xb #x1e #xd 
        #x1e #xf #x1e #x11 #x1e #x13 #x1e #x15 
        #x1e #x17 #x1e #x19 #x1e #x1b #x1e #x1d 
        #x1e #x1f #x1e #x21 #x1e #x23 #x1e #x25 
        #x1e #x27 #x1e #x29 #x1e #x2b #x1e #x2d 
        #x1e #x2f #x1e #x31 #x1e #x33 #x1e #x35 
        #x1e #x37 #x1e #x39 #x1e #x3b #x1e #x3d 
        #x1e #x3f #x1e #x41 #x1e #x43 #x1e #x45 
        #x1e #x47 #x1e #x49 #x1e #x4b #x1e #x4d 
        #x1e #x4f #x1e #x51 #x1e #x53 #x1e #x55 
        #x1e #x57 #x1e #x59 #x1e #x5b #x1e #x5d 
        #x1e #x5f #x1e #x61 #x1e #x63 #x1e #x65 
        #x1e #x67 #x1e #x69 #x1e #x6b #x1e #x6d 
        #x1e #x6f #x1e #x71 #x1e #x73 #x1e #x75 
        #x1e #x77 #x1e #x79 #x1e #x7b #x1e #x7d 
        #x1e #x7f #x1e #x81 #x1e #x83 #x1e #x85 
        #x1e #x87 #x1e #x89 #x1e #x8b #x1e #x8d 
        #x1e #x8f #x1e #x91 #x1e #x93 #x1e #x95 
        #x1e #x9b #x1e #xa1 #x1e #xa3 #x1e #xa5 
        #x1e #xa7 #x1e #xa9 #x1e #xab #x1e #xad 
        #x1e #xaf #x1e #xb1 #x1e #xb3 #x1e #xb5 
        #x1e #xb7 #x1e #xb9 #x1e #xbb #x1e #xbd 
        #x1e #xbf #x1e #xc1 #x1e #xc3 #x1e #xc5 
        #x1e #xc7 #x1e #xc9 #x1e #xcb #x1e #xcd 
        #x1e #xcf #x1e #xd1 #x1e #xd3 #x1e #xd5 
        #x1e #xd7 #x1e #xd9 #x1e #xdb #x1e #xdd 
        #x1e #xdf #x1e #xe1 #x1e #xe3 #x1e #xe5 
        #x1e #xe7 #x1e #xe9 #x1e #xeb #x1e #xed 
        #x1e #xef #x1e #xf1 #x1e #xf3 #x1e #xf5 
        #x1e #xf7 #x1e #xf9 #x1f #x0 #x1f #x1 
        #x1f #x2 #x1f #x3 #x1f #x4 #x1f #x5 
        #x1f #x6 #x1f #x7 #x1f #x10 #x1f #x11 
        #x1f #x12 #x1f #x13 #x1f #x14 #x1f #x15 
        #x1f #x20 #x1f #x21 #x1f #x22 #x1f #x23 
        #x1f #x24 #x1f #x25 #x1f #x26 #x1f #x27 
        #x1f #x30 #x1f #x31 #x1f #x32 #x1f #x33 
        #x1f #x34 #x1f #x35 #x1f #x36 #x1f #x37 
        #x1f #x40 #x1f #x41 #x1f #x42 #x1f #x43 
        #x1f #x44 #x1f #x45 #x1f #x51 #x1f #x53 
        #x1f #x55 #x1f #x57 #x1f #x60 #x1f #x61 
        #x1f #x62 #x1f #x63 #x1f #x64 #x1f #x65 
        #x1f #x66 #x1f #x67 #x1f #x70 #x1f #x71 
        #x1f #x72 #x1f #x73 #x1f #x74 #x1f #x75 
        #x1f #x76 #x1f #x77 #x1f #x78 #x1f #x79 
        #x1f #x7a #x1f #x7b #x1f #x7c #x1f #x7d 
        #x1f #x80 #x1f #x81 #x1f #x82 #x1f #x83 
        #x1f #x84 #x1f #x85 #x1f #x86 #x1f #x87 
        #x1f #x90 #x1f #x91 #x1f #x92 #x1f #x93 
        #x1f #x94 #x1f #x95 #x1f #x96 #x1f #x97 
        #x1f #xa0 #x1f #xa1 #x1f #xa2 #x1f #xa3 
        #x1f #xa4 #x1f #xa5 #x1f #xa6 #x1f #xa7 
        #x1f #xb0 #x1f #xb1 #x1f #xb3 #x1f #xbe 
        #x1f #xc3 #x1f #xd0 #x1f #xd1 #x1f #xe0 
        #x1f #xe1 #x1f #xe5 #x1f #xf3 #x21 #x70 
        #x21 #x71 #x21 #x72 #x21 #x73 #x21 #x74 
        #x21 #x75 #x21 #x76 #x21 #x77 #x21 #x78 
        #x21 #x79 #x21 #x7a #x21 #x7b #x21 #x7c 
        #x21 #x7d #x21 #x7e #x21 #x7f #x24 #xd0 
        #x24 #xd1 #x24 #xd2 #x24 #xd3 #x24 #xd4 
        #x24 #xd5 #x24 #xd6 #x24 #xd7 #x24 #xd8 
        #x24 #xd9 #x24 #xda #x24 #xdb #x24 #xdc 
        #x24 #xdd #x24 #xde #x24 #xdf #x24 #xe0 
        #x24 #xe1 #x24 #xe2 #x24 #xe3 #x24 #xe4 
        #x24 #xe5 #x24 #xe6 #x24 #xe7 #x24 #xe8 
        #x24 #xe9 #x2c #x30 #x2c #x31 #x2c #x32 
        #x2c #x33 #x2c #x34 #x2c #x35 #x2c #x36 
        #x2c #x37 #x2c #x38 #x2c #x39 #x2c #x3a 
        #x2c #x3b #x2c #x3c #x2c #x3d #x2c #x3e 
        #x2c #x3f #x2c #x40 #x2c #x41 #x2c #x42 
        #x2c #x43 #x2c #x44 #x2c #x45 #x2c #x46 
        #x2c #x47 #x2c #x48 #x2c #x49 #x2c #x4a 
        #x2c #x4b #x2c #x4c #x2c #x4d #x2c #x4e 
        #x2c #x4f #x2c #x50 #x2c #x51 #x2c #x52 
        #x2c #x53 #x2c #x54 #x2c #x55 #x2c #x56 
        #x2c #x57 #x2c #x58 #x2c #x59 #x2c #x5a 
        #x2c #x5b #x2c #x5c #x2c #x5d #x2c #x5e 
        #x2c #x81 #x2c #x83 #x2c #x85 #x2c #x87 
        #x2c #x89 #x2c #x8b #x2c #x8d #x2c #x8f 
        #x2c #x91 #x2c #x93 #x2c #x95 #x2c #x97 
        #x2c #x99 #x2c #x9b #x2c #x9d #x2c #x9f 
        #x2c #xa1 #x2c #xa3 #x2c #xa5 #x2c #xa7 
        #x2c #xa9 #x2c #xab #x2c #xad #x2c #xaf 
        #x2c #xb1 #x2c #xb3 #x2c #xb5 #x2c #xb7 
        #x2c #xb9 #x2c #xbb #x2c #xbd #x2c #xbf 
        #x2c #xc1 #x2c #xc3 #x2c #xc5 #x2c #xc7 
        #x2c #xc9 #x2c #xcb #x2c #xcd #x2c #xcf 
        #x2c #xd1 #x2c #xd3 #x2c #xd5 #x2c #xd7 
        #x2c #xd9 #x2c #xdb #x2c #xdd #x2c #xdf 
        #x2c #xe1 #x2c #xe3 #x2d #x0 #x2d #x1 
        #x2d #x2 #x2d #x3 #x2d #x4 #x2d #x5 
        #x2d #x6 #x2d #x7 #x2d #x8 #x2d #x9 
        #x2d #xa #x2d #xb #x2d #xc #x2d #xd 
        #x2d #xe #x2d #xf #x2d #x10 #x2d #x11 
        #x2d #x12 #x2d #x13 #x2d #x14 #x2d #x15 
        #x2d #x16 #x2d #x17 #x2d #x18 #x2d #x19 
        #x2d #x1a #x2d #x1b #x2d #x1c #x2d #x1d 
        #x2d #x1e #x2d #x1f #x2d #x20 #x2d #x21 
        #x2d #x22 #x2d #x23 #x2d #x24 #x2d #x25 
        #xff #x41 #xff #x42 #xff #x43 #xff #x44 
        #xff #x45 #xff #x46 #xff #x47 #xff #x48 
        #xff #x49 #xff #x4a #xff #x4b #xff #x4c 
        #xff #x4d #xff #x4e #xff #x4f #xff #x50 
        #xff #x51 #xff #x52 #xff #x53 #xff #x54 
        #xff #x55 #xff #x56 #xff #x57 #xff #x58 
        #xff #x59 #xff #x5a ))
)

; This vector contains all other code points,
; in increasing order, that have a simple
; uppercase mapping.
;
; This table contains 40 elements.

(define simple-upcase-chars-morebits
  '#(
     #x10428 #x10429 #x1042a #x1042b #x1042c #x1042d #x1042e #x1042f 
     #x10430 #x10431 #x10432 #x10433 #x10434 #x10435 #x10436 #x10437 
     #x10438 #x10439 #x1043a #x1043b #x1043c #x1043d #x1043e #x1043f 
     #x10440 #x10441 #x10442 #x10443 #x10444 #x10445 #x10446 #x10447 
     #x10448 #x10449 #x1044a #x1044b #x1044c #x1044d #x1044e #x1044f ))

; This bytevector uses two bytes per code point
; to list all 16-bit code points, in increasing order,
; that have a simple lowercase mapping.
;
; This table contains 1706 elements.

(define simple-downcase-chars-16bit
  (list->bytevector
      '(
        #x0 #x41 #x0 #x42 #x0 #x43 #x0 #x44 
        #x0 #x45 #x0 #x46 #x0 #x47 #x0 #x48 
        #x0 #x49 #x0 #x4a #x0 #x4b #x0 #x4c 
        #x0 #x4d #x0 #x4e #x0 #x4f #x0 #x50 
        #x0 #x51 #x0 #x52 #x0 #x53 #x0 #x54 
        #x0 #x55 #x0 #x56 #x0 #x57 #x0 #x58 
        #x0 #x59 #x0 #x5a #x0 #xc0 #x0 #xc1 
        #x0 #xc2 #x0 #xc3 #x0 #xc4 #x0 #xc5 
        #x0 #xc6 #x0 #xc7 #x0 #xc8 #x0 #xc9 
        #x0 #xca #x0 #xcb #x0 #xcc #x0 #xcd 
        #x0 #xce #x0 #xcf #x0 #xd0 #x0 #xd1 
        #x0 #xd2 #x0 #xd3 #x0 #xd4 #x0 #xd5 
        #x0 #xd6 #x0 #xd8 #x0 #xd9 #x0 #xda 
        #x0 #xdb #x0 #xdc #x0 #xdd #x0 #xde 
        #x1 #x0 #x1 #x2 #x1 #x4 #x1 #x6 
        #x1 #x8 #x1 #xa #x1 #xc #x1 #xe 
        #x1 #x10 #x1 #x12 #x1 #x14 #x1 #x16 
        #x1 #x18 #x1 #x1a #x1 #x1c #x1 #x1e 
        #x1 #x20 #x1 #x22 #x1 #x24 #x1 #x26 
        #x1 #x28 #x1 #x2a #x1 #x2c #x1 #x2e 
        #x1 #x30 #x1 #x32 #x1 #x34 #x1 #x36 
        #x1 #x39 #x1 #x3b #x1 #x3d #x1 #x3f 
        #x1 #x41 #x1 #x43 #x1 #x45 #x1 #x47 
        #x1 #x4a #x1 #x4c #x1 #x4e #x1 #x50 
        #x1 #x52 #x1 #x54 #x1 #x56 #x1 #x58 
        #x1 #x5a #x1 #x5c #x1 #x5e #x1 #x60 
        #x1 #x62 #x1 #x64 #x1 #x66 #x1 #x68 
        #x1 #x6a #x1 #x6c #x1 #x6e #x1 #x70 
        #x1 #x72 #x1 #x74 #x1 #x76 #x1 #x78 
        #x1 #x79 #x1 #x7b #x1 #x7d #x1 #x81 
        #x1 #x82 #x1 #x84 #x1 #x86 #x1 #x87 
        #x1 #x89 #x1 #x8a #x1 #x8b #x1 #x8e 
        #x1 #x8f #x1 #x90 #x1 #x91 #x1 #x93 
        #x1 #x94 #x1 #x96 #x1 #x97 #x1 #x98 
        #x1 #x9c #x1 #x9d #x1 #x9f #x1 #xa0 
        #x1 #xa2 #x1 #xa4 #x1 #xa6 #x1 #xa7 
        #x1 #xa9 #x1 #xac #x1 #xae #x1 #xaf 
        #x1 #xb1 #x1 #xb2 #x1 #xb3 #x1 #xb5 
        #x1 #xb7 #x1 #xb8 #x1 #xbc #x1 #xc4 
        #x1 #xc5 #x1 #xc7 #x1 #xc8 #x1 #xca 
        #x1 #xcb #x1 #xcd #x1 #xcf #x1 #xd1 
        #x1 #xd3 #x1 #xd5 #x1 #xd7 #x1 #xd9 
        #x1 #xdb #x1 #xde #x1 #xe0 #x1 #xe2 
        #x1 #xe4 #x1 #xe6 #x1 #xe8 #x1 #xea 
        #x1 #xec #x1 #xee #x1 #xf1 #x1 #xf2 
        #x1 #xf4 #x1 #xf6 #x1 #xf7 #x1 #xf8 
        #x1 #xfa #x1 #xfc #x1 #xfe #x2 #x0 
        #x2 #x2 #x2 #x4 #x2 #x6 #x2 #x8 
        #x2 #xa #x2 #xc #x2 #xe #x2 #x10 
        #x2 #x12 #x2 #x14 #x2 #x16 #x2 #x18 
        #x2 #x1a #x2 #x1c #x2 #x1e #x2 #x20 
        #x2 #x22 #x2 #x24 #x2 #x26 #x2 #x28 
        #x2 #x2a #x2 #x2c #x2 #x2e #x2 #x30 
        #x2 #x32 #x2 #x3b #x2 #x3d #x2 #x41 
        #x3 #x86 #x3 #x88 #x3 #x89 #x3 #x8a 
        #x3 #x8c #x3 #x8e #x3 #x8f #x3 #x91 
        #x3 #x92 #x3 #x93 #x3 #x94 #x3 #x95 
        #x3 #x96 #x3 #x97 #x3 #x98 #x3 #x99 
        #x3 #x9a #x3 #x9b #x3 #x9c #x3 #x9d 
        #x3 #x9e #x3 #x9f #x3 #xa0 #x3 #xa1 
        #x3 #xa3 #x3 #xa4 #x3 #xa5 #x3 #xa6 
        #x3 #xa7 #x3 #xa8 #x3 #xa9 #x3 #xaa 
        #x3 #xab #x3 #xd8 #x3 #xda #x3 #xdc 
        #x3 #xde #x3 #xe0 #x3 #xe2 #x3 #xe4 
        #x3 #xe6 #x3 #xe8 #x3 #xea #x3 #xec 
        #x3 #xee #x3 #xf4 #x3 #xf7 #x3 #xf9 
        #x3 #xfa #x4 #x0 #x4 #x1 #x4 #x2 
        #x4 #x3 #x4 #x4 #x4 #x5 #x4 #x6 
        #x4 #x7 #x4 #x8 #x4 #x9 #x4 #xa 
        #x4 #xb #x4 #xc #x4 #xd #x4 #xe 
        #x4 #xf #x4 #x10 #x4 #x11 #x4 #x12 
        #x4 #x13 #x4 #x14 #x4 #x15 #x4 #x16 
        #x4 #x17 #x4 #x18 #x4 #x19 #x4 #x1a 
        #x4 #x1b #x4 #x1c #x4 #x1d #x4 #x1e 
        #x4 #x1f #x4 #x20 #x4 #x21 #x4 #x22 
        #x4 #x23 #x4 #x24 #x4 #x25 #x4 #x26 
        #x4 #x27 #x4 #x28 #x4 #x29 #x4 #x2a 
        #x4 #x2b #x4 #x2c #x4 #x2d #x4 #x2e 
        #x4 #x2f #x4 #x60 #x4 #x62 #x4 #x64 
        #x4 #x66 #x4 #x68 #x4 #x6a #x4 #x6c 
        #x4 #x6e #x4 #x70 #x4 #x72 #x4 #x74 
        #x4 #x76 #x4 #x78 #x4 #x7a #x4 #x7c 
        #x4 #x7e #x4 #x80 #x4 #x8a #x4 #x8c 
        #x4 #x8e #x4 #x90 #x4 #x92 #x4 #x94 
        #x4 #x96 #x4 #x98 #x4 #x9a #x4 #x9c 
        #x4 #x9e #x4 #xa0 #x4 #xa2 #x4 #xa4 
        #x4 #xa6 #x4 #xa8 #x4 #xaa #x4 #xac 
        #x4 #xae #x4 #xb0 #x4 #xb2 #x4 #xb4 
        #x4 #xb6 #x4 #xb8 #x4 #xba #x4 #xbc 
        #x4 #xbe #x4 #xc1 #x4 #xc3 #x4 #xc5 
        #x4 #xc7 #x4 #xc9 #x4 #xcb #x4 #xcd 
        #x4 #xd0 #x4 #xd2 #x4 #xd4 #x4 #xd6 
        #x4 #xd8 #x4 #xda #x4 #xdc #x4 #xde 
        #x4 #xe0 #x4 #xe2 #x4 #xe4 #x4 #xe6 
        #x4 #xe8 #x4 #xea #x4 #xec #x4 #xee 
        #x4 #xf0 #x4 #xf2 #x4 #xf4 #x4 #xf6 
        #x4 #xf8 #x5 #x0 #x5 #x2 #x5 #x4 
        #x5 #x6 #x5 #x8 #x5 #xa #x5 #xc 
        #x5 #xe #x5 #x31 #x5 #x32 #x5 #x33 
        #x5 #x34 #x5 #x35 #x5 #x36 #x5 #x37 
        #x5 #x38 #x5 #x39 #x5 #x3a #x5 #x3b 
        #x5 #x3c #x5 #x3d #x5 #x3e #x5 #x3f 
        #x5 #x40 #x5 #x41 #x5 #x42 #x5 #x43 
        #x5 #x44 #x5 #x45 #x5 #x46 #x5 #x47 
        #x5 #x48 #x5 #x49 #x5 #x4a #x5 #x4b 
        #x5 #x4c #x5 #x4d #x5 #x4e #x5 #x4f 
        #x5 #x50 #x5 #x51 #x5 #x52 #x5 #x53 
        #x5 #x54 #x5 #x55 #x5 #x56 #x10 #xa0 
        #x10 #xa1 #x10 #xa2 #x10 #xa3 #x10 #xa4 
        #x10 #xa5 #x10 #xa6 #x10 #xa7 #x10 #xa8 
        #x10 #xa9 #x10 #xaa #x10 #xab #x10 #xac 
        #x10 #xad #x10 #xae #x10 #xaf #x10 #xb0 
        #x10 #xb1 #x10 #xb2 #x10 #xb3 #x10 #xb4 
        #x10 #xb5 #x10 #xb6 #x10 #xb7 #x10 #xb8 
        #x10 #xb9 #x10 #xba #x10 #xbb #x10 #xbc 
        #x10 #xbd #x10 #xbe #x10 #xbf #x10 #xc0 
        #x10 #xc1 #x10 #xc2 #x10 #xc3 #x10 #xc4 
        #x10 #xc5 #x1e #x0 #x1e #x2 #x1e #x4 
        #x1e #x6 #x1e #x8 #x1e #xa #x1e #xc 
        #x1e #xe #x1e #x10 #x1e #x12 #x1e #x14 
        #x1e #x16 #x1e #x18 #x1e #x1a #x1e #x1c 
        #x1e #x1e #x1e #x20 #x1e #x22 #x1e #x24 
        #x1e #x26 #x1e #x28 #x1e #x2a #x1e #x2c 
        #x1e #x2e #x1e #x30 #x1e #x32 #x1e #x34 
        #x1e #x36 #x1e #x38 #x1e #x3a #x1e #x3c 
        #x1e #x3e #x1e #x40 #x1e #x42 #x1e #x44 
        #x1e #x46 #x1e #x48 #x1e #x4a #x1e #x4c 
        #x1e #x4e #x1e #x50 #x1e #x52 #x1e #x54 
        #x1e #x56 #x1e #x58 #x1e #x5a #x1e #x5c 
        #x1e #x5e #x1e #x60 #x1e #x62 #x1e #x64 
        #x1e #x66 #x1e #x68 #x1e #x6a #x1e #x6c 
        #x1e #x6e #x1e #x70 #x1e #x72 #x1e #x74 
        #x1e #x76 #x1e #x78 #x1e #x7a #x1e #x7c 
        #x1e #x7e #x1e #x80 #x1e #x82 #x1e #x84 
        #x1e #x86 #x1e #x88 #x1e #x8a #x1e #x8c 
        #x1e #x8e #x1e #x90 #x1e #x92 #x1e #x94 
        #x1e #xa0 #x1e #xa2 #x1e #xa4 #x1e #xa6 
        #x1e #xa8 #x1e #xaa #x1e #xac #x1e #xae 
        #x1e #xb0 #x1e #xb2 #x1e #xb4 #x1e #xb6 
        #x1e #xb8 #x1e #xba #x1e #xbc #x1e #xbe 
        #x1e #xc0 #x1e #xc2 #x1e #xc4 #x1e #xc6 
        #x1e #xc8 #x1e #xca #x1e #xcc #x1e #xce 
        #x1e #xd0 #x1e #xd2 #x1e #xd4 #x1e #xd6 
        #x1e #xd8 #x1e #xda #x1e #xdc #x1e #xde 
        #x1e #xe0 #x1e #xe2 #x1e #xe4 #x1e #xe6 
        #x1e #xe8 #x1e #xea #x1e #xec #x1e #xee 
        #x1e #xf0 #x1e #xf2 #x1e #xf4 #x1e #xf6 
        #x1e #xf8 #x1f #x8 #x1f #x9 #x1f #xa 
        #x1f #xb #x1f #xc #x1f #xd #x1f #xe 
        #x1f #xf #x1f #x18 #x1f #x19 #x1f #x1a 
        #x1f #x1b #x1f #x1c #x1f #x1d #x1f #x28 
        #x1f #x29 #x1f #x2a #x1f #x2b #x1f #x2c 
        #x1f #x2d #x1f #x2e #x1f #x2f #x1f #x38 
        #x1f #x39 #x1f #x3a #x1f #x3b #x1f #x3c 
        #x1f #x3d #x1f #x3e #x1f #x3f #x1f #x48 
        #x1f #x49 #x1f #x4a #x1f #x4b #x1f #x4c 
        #x1f #x4d #x1f #x59 #x1f #x5b #x1f #x5d 
        #x1f #x5f #x1f #x68 #x1f #x69 #x1f #x6a 
        #x1f #x6b #x1f #x6c #x1f #x6d #x1f #x6e 
        #x1f #x6f #x1f #x88 #x1f #x89 #x1f #x8a 
        #x1f #x8b #x1f #x8c #x1f #x8d #x1f #x8e 
        #x1f #x8f #x1f #x98 #x1f #x99 #x1f #x9a 
        #x1f #x9b #x1f #x9c #x1f #x9d #x1f #x9e 
        #x1f #x9f #x1f #xa8 #x1f #xa9 #x1f #xaa 
        #x1f #xab #x1f #xac #x1f #xad #x1f #xae 
        #x1f #xaf #x1f #xb8 #x1f #xb9 #x1f #xba 
        #x1f #xbb #x1f #xbc #x1f #xc8 #x1f #xc9 
        #x1f #xca #x1f #xcb #x1f #xcc #x1f #xd8 
        #x1f #xd9 #x1f #xda #x1f #xdb #x1f #xe8 
        #x1f #xe9 #x1f #xea #x1f #xeb #x1f #xec 
        #x1f #xf8 #x1f #xf9 #x1f #xfa #x1f #xfb 
        #x1f #xfc #x21 #x26 #x21 #x2a #x21 #x2b 
        #x21 #x60 #x21 #x61 #x21 #x62 #x21 #x63 
        #x21 #x64 #x21 #x65 #x21 #x66 #x21 #x67 
        #x21 #x68 #x21 #x69 #x21 #x6a #x21 #x6b 
        #x21 #x6c #x21 #x6d #x21 #x6e #x21 #x6f 
        #x24 #xb6 #x24 #xb7 #x24 #xb8 #x24 #xb9 
        #x24 #xba #x24 #xbb #x24 #xbc #x24 #xbd 
        #x24 #xbe #x24 #xbf #x24 #xc0 #x24 #xc1 
        #x24 #xc2 #x24 #xc3 #x24 #xc4 #x24 #xc5 
        #x24 #xc6 #x24 #xc7 #x24 #xc8 #x24 #xc9 
        #x24 #xca #x24 #xcb #x24 #xcc #x24 #xcd 
        #x24 #xce #x24 #xcf #x2c #x0 #x2c #x1 
        #x2c #x2 #x2c #x3 #x2c #x4 #x2c #x5 
        #x2c #x6 #x2c #x7 #x2c #x8 #x2c #x9 
        #x2c #xa #x2c #xb #x2c #xc #x2c #xd 
        #x2c #xe #x2c #xf #x2c #x10 #x2c #x11 
        #x2c #x12 #x2c #x13 #x2c #x14 #x2c #x15 
        #x2c #x16 #x2c #x17 #x2c #x18 #x2c #x19 
        #x2c #x1a #x2c #x1b #x2c #x1c #x2c #x1d 
        #x2c #x1e #x2c #x1f #x2c #x20 #x2c #x21 
        #x2c #x22 #x2c #x23 #x2c #x24 #x2c #x25 
        #x2c #x26 #x2c #x27 #x2c #x28 #x2c #x29 
        #x2c #x2a #x2c #x2b #x2c #x2c #x2c #x2d 
        #x2c #x2e #x2c #x80 #x2c #x82 #x2c #x84 
        #x2c #x86 #x2c #x88 #x2c #x8a #x2c #x8c 
        #x2c #x8e #x2c #x90 #x2c #x92 #x2c #x94 
        #x2c #x96 #x2c #x98 #x2c #x9a #x2c #x9c 
        #x2c #x9e #x2c #xa0 #x2c #xa2 #x2c #xa4 
        #x2c #xa6 #x2c #xa8 #x2c #xaa #x2c #xac 
        #x2c #xae #x2c #xb0 #x2c #xb2 #x2c #xb4 
        #x2c #xb6 #x2c #xb8 #x2c #xba #x2c #xbc 
        #x2c #xbe #x2c #xc0 #x2c #xc2 #x2c #xc4 
        #x2c #xc6 #x2c #xc8 #x2c #xca #x2c #xcc 
        #x2c #xce #x2c #xd0 #x2c #xd2 #x2c #xd4 
        #x2c #xd6 #x2c #xd8 #x2c #xda #x2c #xdc 
        #x2c #xde #x2c #xe0 #x2c #xe2 #xff #x21 
        #xff #x22 #xff #x23 #xff #x24 #xff #x25 
        #xff #x26 #xff #x27 #xff #x28 #xff #x29 
        #xff #x2a #xff #x2b #xff #x2c #xff #x2d 
        #xff #x2e #xff #x2f #xff #x30 #xff #x31 
        #xff #x32 #xff #x33 #xff #x34 #xff #x35 
        #xff #x36 #xff #x37 #xff #x38 #xff #x39 
        #xff #x3a ))
)

; This vector contains all other code points,
; in increasing order, that have a simple
; lowercase mapping.
;
; This table contains 40 elements.

(define simple-downcase-chars-morebits
  '#(
     #x10400 #x10401 #x10402 #x10403 #x10404 #x10405 #x10406 #x10407 
     #x10408 #x10409 #x1040a #x1040b #x1040c #x1040d #x1040e #x1040f 
     #x10410 #x10411 #x10412 #x10413 #x10414 #x10415 #x10416 #x10417 
     #x10418 #x10419 #x1041a #x1041b #x1041c #x1041d #x1041e #x1041f 
     #x10420 #x10421 #x10422 #x10423 #x10424 #x10425 #x10426 #x10427 ))

; The bytes of this bytevector are indexes into
; the simple-case-adjustments vector, and correspond
; to the code points in simple-upcase-chars-16bit
; followed by those in simple-upcase-chars-morebits.
;
; This table contains 902 elements.

(define simple-upcase-adjustments
  (list->bytevector
      '(
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x38 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x32 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x3 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x2 #x26 #x26 #x26 #x26 #x26 #x2f #x26 
        #x36 #x35 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x2a #x26 #x25 #x26 
        #x25 #x26 #x25 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x15 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x25 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #xa #xd 
        #xe #xe #x10 #xf #xe #xc #xb #x9 
        #x9 #x8 #x7 #x5 #x5 #x5 #x6 #x6 
        #x4 #x13 #x2d #x1f #x20 #x20 #x20 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x22 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x16 #x17 #x17 #x18 #x1a #x1d 
        #x1b #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x12 #x14 #x27 
        #x11 #x26 #x26 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x14 #x14 #x14 #x14 #x14 
        #x14 #x14 #x14 #x14 #x14 #x14 #x14 #x14 
        #x14 #x14 #x14 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x19 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x2c #x2c #x2e #x2e #x2e #x2e 
        #x30 #x30 #x34 #x34 #x31 #x31 #x33 #x33 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x29 #x1 #x29 #x28 #x28 #x28 
        #x28 #x27 #x29 #x24 #x24 #x24 #x24 #x24 
        #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 
        #x24 #x24 #x24 #x23 #x23 #x23 #x23 #x23 
        #x23 #x23 #x23 #x23 #x23 #x23 #x23 #x23 
        #x23 #x23 #x23 #x23 #x23 #x23 #x23 #x23 
        #x23 #x23 #x23 #x23 #x23 #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x0 #x0 
        #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 
        #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 
        #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 
        #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 
        #x0 #x0 #x0 #x0 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x1e #x1e 
        #x1e #x1e #x1e #x1e #x1e #x1e #x1e #x1e 
        #x1e #x1e #x1e #x1e #x1e #x1e #x1e #x1e 
        #x1e #x1e #x1e #x1e #x1e #x1e #x1e #x1e 
        #x1e #x1e #x1e #x1e #x1e #x1e #x1e #x1e 
        #x1e #x1e #x1e #x1e #x1e #x1e ))
)

; The bytes of this bytevector are indexes into
; the simple-case-adjustments vector, and correspond
; to the code points in simple-downcase-chars-16bit
; followed by those in simple-downcase-chars-morebits.
;
; This table contains 893 elements.

(define simple-downcase-adjustments
  (list->bytevector
      '(
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x37 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x32 #x26 #x26 #x26 #xa 
        #x26 #x26 #xd #x26 #xe #xe #x26 #x15 
        #x10 #xf #x26 #xe #xc #x9 #xb #x26 
        #x9 #x8 #x7 #x26 #x26 #x26 #x5 #x26 
        #x5 #x26 #x5 #x26 #x6 #x6 #x26 #x26 
        #x4 #x26 #x26 #x25 #x26 #x25 #x26 #x25 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x25 #x26 #x26 #x2f #x2a #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x35 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x36 #x13 
        #x1f #x20 #x20 #x20 #x16 #x17 #x17 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x2b #x26 #x27 
        #x26 #x14 #x14 #x14 #x14 #x14 #x14 #x14 
        #x14 #x14 #x14 #x14 #x14 #x14 #x14 #x14 
        #x14 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x0 
        #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 
        #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 
        #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 
        #x0 #x0 #x0 #x0 #x0 #x0 #x0 #x0 
        #x0 #x0 #x0 #x0 #x0 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28 
        #x28 #x28 #x28 #x28 #x28 #x28 #x28 #x2c 
        #x2c #x29 #x2e #x2e #x2e #x2e #x29 #x28 
        #x28 #x30 #x30 #x28 #x28 #x31 #x31 #x27 
        #x34 #x34 #x33 #x33 #x29 #x39 #x3b #x3a 
        #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 
        #x24 #x24 #x24 #x24 #x24 #x24 #x24 #x24 
        #x23 #x23 #x23 #x23 #x23 #x23 #x23 #x23 
        #x23 #x23 #x23 #x23 #x23 #x23 #x23 #x23 
        #x23 #x23 #x23 #x23 #x23 #x23 #x23 #x23 
        #x23 #x23 #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x1c #x1c #x1c #x1c #x1c #x1c #x1c 
        #x1c #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x26 #x26 #x26 #x26 #x26 
        #x26 #x26 #x26 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x21 #x21 #x21 
        #x21 #x21 #x21 #x21 #x21 #x1e #x1e #x1e 
        #x1e #x1e #x1e #x1e #x1e #x1e #x1e #x1e 
        #x1e #x1e #x1e #x1e #x1e #x1e #x1e #x1e 
        #x1e #x1e #x1e #x1e #x1e #x1e #x1e #x1e 
        #x1e #x1e #x1e #x1e #x1e #x1e #x1e #x1e 
        #x1e #x1e #x1e #x1e #x1e ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End of Unicode stuff.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; eof
