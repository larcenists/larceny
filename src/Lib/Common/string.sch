; Copyright Lightship Software, Incorporated.
;
; $Id$
;
; Larceny library --  characters, strings, and bytevectors.
;
; Should there be a bytevector-like-subfill! primop to use here?

($$trace "string")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
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
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *char-table* '#())


; ISO Latin 1 character set.

(define (make-ascii-table)

  (define tbl (make-bytevector 256))

  (bytevector-fill! tbl 0)

  (do ((i (char->integer #\A) (+ i 1)))
      ((> i (char->integer #\Z)))
    (bytevector-set! tbl i 1))

  (do ((i (char->integer #\a) (+ i 1)))
      ((> i (char->integer #\z)))
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

(set! *char-table* (make-ascii-table))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; End of deprecated (and probably obsolete) stuff.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define string-copy
  (lambda (x . rest)
    (let* ((start (if (null? rest) 0 (car rest)))
           (end  (if (or (null? rest) (null? (cdr rest)))
                     (string-length x)
                     (cadr rest))))
      (substring x start end))))

; Copies (substring x i j) into y starting at k.
; Used only within this file.
; Performs no checking.
; Assumes x and y are distinct strings, or that k <= i.

(define string-copy-into-down!
  (lambda (x i j y k)
    (do ((i i (.+:idx:idx i 1))
         (k k (.+:idx:idx k 1)))
        ((.>=:fix:fix i j))
      (.string-set!:trusted y k (.string-ref:trusted x i)))))

; As above, but assumes k >= i.

(define string-copy-into-up!
  (lambda (x i j y k)
    (do ((j (- j 1) (- j 1))
         (k (+ k (- j i) -1) (- k 1)))
        ((> i j))
      (string-set! y k (string-ref x j)))))

;;; R7RS 6.7 says "It is an error if at is less than zero or greater than
;;; the length of to.  It is also an error if (- (string-length to) at)
;;; is less than (- end start)."
;;; The R7RS does not say what the last argument (end) defaults to if
;;; omitted.  If end is not specified, Larceny uses the largest index
;;; that will work.

(define string-copy!
  (lambda (dst at src . rest)

    (define (complain0 msgcode)
      (assertion-violation 'string-copy!
                           (errmsg msgcode)
                           (cons dst (cons at (cons src rest)))))

    (define (complain msgcode culprit)
      (assertion-violation 'string-copy!
                           (errmsg msgcode)
                           culprit))

    (let* ((start (if (null? rest)
                      0
                      (car rest)))
           (end (if (or (null? rest) (null? (cdr rest)))
                    (min (string-length src)
                         (+ start (- (string-length dst) at)))
                    (cadr rest))))
      (cond ((not (string? dst))
             (complain 'msg:notstring dst))
            ((not (string? src))
             (complain 'msg:notstring src))
            ((not (fixnum? at))
             (complain 'msg:notfixnum at))
            ((not (fixnum? start))
             (complain 'msg:notfixnum start))
            ((not (fixnum? end))
             (complain 'msg:notfixnum end))
            ((not (and (<= 0 at (string-length dst))
                       (<= 0 start end (string-length src))
                       (<= (+ at (- end start)) (string-length dst))))
             (complain0 'msg:rangeerror))
            (else
             ((if (<= at start)
                  string-copy-into-down!
                  string-copy-into-up!)
              src start end dst at))))))

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
             (string-copy-into-down! this-string 0 length
                                     result-string position)
             result-string))
          ((null? tail) (make-string position))
          (else (error 'concatenate-strings
                       (errmsg 'msg:notlist)
                       string-list))))
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
          (string-copy-into-down! s m n y 0)
          y)
        (error 'substring (errmsg 'msg:illegalargs) s m n))))

(define string-fill!
  (lambda (s c . rest)
    (let* ((start (if (null? rest) 0 (car rest)))
           (end (if (or (null? rest) (null? (cdr rest)))
                    (string-length s)
                    (cadr rest))))
      (substring-fill! s start end c))))

(define substring-fill!
  (lambda (s start end c)
    (do ((i start (+ i 1)))
        ((>= i end) s)
        (string-set! s i c))))

; Make-string is now a primitive; see primops.sch.

(define list->string
  (letrec ((loop
             (lambda (s i l)
               (if (pair? l)
                   (begin (string-set! s i (car l))
                          (loop s (+ i 1) (cdr l)))
                   s))))
    (lambda (l)
      (loop (make-string (length l)) 0 l))))

(define (string->list s . rest)
  (define (loop s start i chars)
    (if (< i start)
        chars
        (loop s start (- i 1) (cons (string-ref s i) chars))))
  (let* ((start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest)))
                  (string-length s)
                  (cadr rest))))
    (loop s start (- end 1) '())))

;;; FIXME:  This code must be kept in sync with the definition of
;;; twobit-symbol-hash in Compiler/pass2if.sch.
;;; Any change to this code must be made there also, and vice versa.

;;; Returns a value in the range 0 .. 2^27-1 (which should be a fixnum).
;;;
;;; FIXME: Common Larceny's fixnums are much smaller, but limiting the
;;; result to Common Larceny's fixnum range produces a far less effective
;;; hash function.

(define (string-hash string)

  (define (string-hash-step code byte)
    (fxlogxor code
              ;; Avoid consing fixnums
              (let* ((code (fxlogand code #x3FFFFF)) ; 22 bits
                     (l (fxlsh code 5)))             ; 27 bits
                (fxlogxor l byte))))

  (define (string-hash-loop string limit i code)
    (if (= i limit)
        code
        (string-hash-loop
         string limit (+ i 1)
         (string-hash-step code
                           (fxlogand #xFFFF
                                     (char->integer (string-ref string i)))))))

  (let ((n (string-length string)))
    (string-hash-loop string n 0 (fxlogxor n #x1aa5))))

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
;;;                                   (shift-right hash_n 2)
;;;                                   (string-ref string index)))
;;;

;;; The end result is a > 25% speedup in hashing, and a better
;;; distribution of hash values.  (Hashing a set of words from a
;;; dictionary showed fewer empty buckets, more buckets with exactly
;;; one entry and fewer buckets with three or more entries.)

;;; This version doesn't work as well, mainly because it stays within
;;; the range of 16-bit fixnums, so it's commented out for now.

; Returns a value in the range 0 .. 2^16-1 (a fixnum in Larceny).

'
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

'
(define string-hash
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

(define (string-ci-hash s)
  (string-hash (string-foldcase s)))

; FIXME:  These downcase and upcase procedures are superseded
; by the Unicode versions, which are defined in unicode*.sch.
; The string comparisons are also incorrect.

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

; FIXME:  end of problematic definitions.

; make-nary-comparison is defined in src/Lib/Arch/*/primops.sch

(define string=?
  (make-nary-comparison 'string=?
                        (lambda (a b)
                          (= (string-compare a b) 0))))

(define string-equal? string=?)       ; for backward compatibility

(define string<?
  (make-nary-comparison 'string<?
                        (lambda (a b)
                          (< (string-compare a b) 0))))

(define string<=?
  (make-nary-comparison 'string<=?
                        (lambda (a b)
                          (<= (string-compare a b) 0))))

(define string>?
  (make-nary-comparison 'string>?
                        (lambda (a b)
                          (> (string-compare a b) 0))))

(define string>=?
  (make-nary-comparison 'string>=?
                        (lambda (a b)
                          (>= (string-compare a b) 0))))

(define (string-compare a b)
  (let* ((na (string-length a))
         (nb (string-length b))
         (n (if (<= na nb) na nb)))
    (define (loop i)
      (if (= i n)
          (cond ((< na nb) -1)
                ((> na nb) +1)
                (else 0))
          (let ((ca (string-ref a i))
                (cb (string-ref b i)))
            (cond ((char<? ca cb) -1)
                  ((char>? ca cb) +1)
                  (else (loop (+ i 1)))))))
    (loop 0)))

; Added for R6RS.

(define (string-for-each f s . rest)

  (define (for-each1 i n)
    (if (< i n)
	(begin (f (string-ref s i))
	       (for-each1 (+ i 1) n))
	(unspecified)))

  (define (for-each2 s2 i n)
    (if (< i n)
	(begin (f (string-ref s i) (string-ref s2 i))
	       (for-each2 s2 (+ i 1) n))
	(unspecified)))

  (define (for-each-n revstrings i n)
    (if (< i n)
        (do ((rev revstrings (cdr rev))
             (chars '() (cons (string-ref (car rev) i) chars)))
            ((null? rev)
             (apply f chars)
             (for-each-n revstrings (+ i 1) n)))
	(unspecified)))

  (let ((n (string-length s)))
    (cond ((null? rest)
           (for-each1 0 n))
          ((and (null? (cdr rest))
                (string? (car rest))
                (= n (string-length (car rest))))
           (for-each2 (car rest) 0 n))
          (else
           (let ((args (cons s rest)))
             (do ((ss rest (cdr ss)))
                 ((null? ss)
                  (for-each-n (reverse args) 0 n))
               (let ((x (car ss)))
                 (if (or (not (string? x))
                         (not (= n (string-length x))))
                     (assertion-violation 'string-for-each
                                          "illegal-arguments"
                                          (cons f args))))))))))

;;; Added for R7RS.

; FIXME:  The performance of string-map can be improved, but
; this implementation plays well with first class continuations.

(define (string-map f x . rest)

  (define (string-map1 f x)
    (list->string (map f (string->list x))))

  (define (string-map2 f x y)
    (list->string (map f (string->list x) (string->list y))))

  (define (string-mapn f lists)
    (list->string (apply map f (map string->list lists))))

  (case (length rest)
    ((0)  (string-map1 f x))
    ((1)  (string-map2 f x (car rest)))
    (else (string-mapn f (cons x rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Unicode.
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

; eof
