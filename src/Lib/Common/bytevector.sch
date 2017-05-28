; Copyright Lightship Software, Incorporated.
;
; $Id$
;
; Larceny library --  bytevectors.

($$trace "bytevector")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Larceny's traditional procedures.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; New for R7RS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bytevector . args)
  (let* ((n (length args))
         (bv (make-bytevector n)))
    (do ((i 0 (+ i 1))
         (args args (cdr args)))
        ((= i n)
         bv)
      (bytevector-u8-set! bv i (car args)))))

(define (bytevector-append . args)
  (let* ((lengths (map bytevector-length args))
         (n (apply + lengths))
         (bv (make-bytevector n)))
    (do ((j 0 (+ j (car lengths)))
         (args args (cdr args))
         (lengths lengths (cdr lengths)))
        ((null? args) bv)
      (r7rs:bytevector-copy! bv j (car args)))))

;;; R7RS version of bytevector-copy! is incompatible with R6RS
;;; when five arguments are passed.

;;; R7RS 6.7 says "It is an error if at is less than zero or greater than
;;; the length of to.  It is also an error if (- (bytevector-length to) at)
;;; is less than (- end start)."
;;; The R7RS does not say what the last argument (end) defaults to if
;;; omitted.  If end is not specified, Larceny uses the largest index
;;; that will work.

(define (r7rs:bytevector-copy! dst at src . rest)
  (let* ((start (if (null? rest) 0 (car rest)))
         (rest (if (null? rest) rest (cdr rest)))
         (end (if (or (null? rest) (null? (cdr rest)))
                  (min (bytevector-length src)
                       (+ start (- (bytevector-length dst) at)))
                  (cadr rest)))
         (kount (- end start)))
    (r6rs:bytevector-copy! src start dst at kount)))

;;; FIXME: this is the R6RS version with a temporary hack.

(define *someone-has-used-bytevector-copy!* #f)

(define (bytevector-copy! src i dst j kount)
  (if (not *someone-has-used-bytevector-copy!*)
      (begin (set! *someone-has-used-bytevector-copy!* #t)
             (display "***** Someone used bytevector-copy! *****\n"
                      (current-error-port))))
  (r6rs:bytevector-copy! src i dst j kount))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Bytevector procedures that have been proposed for R6RS.
; FIXME:  These should be bummed for performance.
; In particular, they could use some fixnum arithmetic.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Help syntax and procedures; not exported.

(define-syntax bytevector:div
  (syntax-rules ()
   ((_ x y) (quotient x y))))

(define-syntax bytevector:mod
  (syntax-rules ()
   ((_ x y) (remainder x y))))

(define-syntax u8->s8
  (syntax-rules ()
   ((_ octet0)
    (let ((octet octet0))
      (if (> octet 127)
          (- octet 256)
          octet)))))

(define-syntax s8->u8
  (syntax-rules ()
   ((_ val0)
    (let ((val val0))
      (if (negative? val)
          (+ val 256)
          val)))))

(define (make-uint-ref size)
  (lambda (bytevector k endianness)
    (bytevector-uint-ref bytevector k endianness size)))

(define (make-sint-ref size)
  (lambda (bytevector k endianness)
    (bytevector-sint-ref bytevector k endianness size)))

(define (make-uint-set! size)
  (lambda (bytevector k n endianness)
    (bytevector-uint-set! bytevector k n endianness size)))

(define (make-sint-set! size)
  (lambda (bytevector k n endianness)
    (bytevector-sint-set! bytevector k n endianness size)))

(define (make-ref/native base base-ref)
  (lambda (bytevector index)
    (ensure-aligned index base)
    (base-ref bytevector index (native-endianness))))

(define (make-set!/native base base-set!)
  (lambda (bytevector index val)
    (ensure-aligned index base)
    (base-set! bytevector index val (native-endianness))))

(define (ensure-aligned index base)
  (if (not (zero? (bytevector:mod index base)))
      (error "non-aligned bytevector access" index base)))

(define (make-bytevector->int-list bytevector-ref)
  (lambda (b endness size)
    (let ((ref (lambda (i) (bytevector-ref b i endness size)))
	  (length (bytevector-length b)))
      (let loop ((i 0) (r '()))
	(if (>= i length)
	    (reverse r)
	    (loop (+ i size)
		  (cons (ref i) r)))))))

(define (make-int-list->bytevector bytevector-set!)
  (lambda (l endness size)
    (let* ((bytevector (make-bytevector (* size (length l))))
	   (setter! (lambda (i n)
                      (bytevector-set! bytevector i n endness size))))
      (let loop ((i 0) (l l))
	(if (null? l)
	    bytevector
	    (begin
	      (setter! i (car l))
	      (loop (+ i size) (cdr l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Exported procedures (and, alas, syntax).
;
; In Larceny, *any* symbol names a supported endianness.
; The symbols big and little have their expected meanings.
; All other symbols mean (native-endianness) with respect
; to integer operations, but mean the opposite of
; (native-endianness) with respect to IEEE-754 operations.
; For string operations, the endianness must be big or little.
;
; The extension described above is allowed by the current draft R6RS.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;  (export endianness native-endianness
;          bytevector? make-bytevector bytevector-length
;          bytevector-u8-ref bytevector-s8-ref
;          bytevector-u8-set! bytevector-s8-set!
;          bytevector-uint-ref bytevector-sint-ref
;          bytevector-uint-set! bytevector-sint-set!
;          bytevector-u16-ref bytevector-s16-ref
;          bytevector-u16-set! bytevector-s16-set!
;          bytevector-u16-native-ref bytevector-s16-native-ref
;          bytevector-u16-native-set! bytevector-s16-native-set!
;          bytevector-u32-ref bytevector-s32-ref
;          bytevector-u32-set! bytevector-s32-set!
;          bytevector-u32-native-ref bytevector-s32-native-ref
;          bytevector-u32-native-set! bytevector-s32-native-set!
;          bytevector-u64-ref bytevector-s64-ref
;          bytevector-u64-set! bytevector-s64-set!
;          bytevector-u64-native-ref bytevector-s64-native-ref
;          bytevector-u64-native-set! bytevector-s64-native-set!
;          bytevector=?
;          bytevector-ieee-single-native-ref bytevector-ieee-single-ref
;          bytevector-ieee-double-native-ref bytevector-ieee-double-ref
;          bytevector-ieee-single-native-set! bytevector-ieee-single-set!
;          bytevector-ieee-double-native-set! bytevector-ieee-double-set!
;          bytevector-copy! bytevector-copy
;          bytevector->u8-list u8-list->bytevector
;          bytevector->uint-list bytevector->sint-list
;          uint-list->bytevector sint-list->bytevector
;
;          utf8->string utf16->string utf32->string
;          string->utf8 string->utf16 string->utf32)

; The endianness syntax is now in usual.sch.

(define *native-endianness* #f)

(define (native-endianness)
  (let ((result *native-endianness*))
    (if result
        result
        (begin (set! *native-endianness*
                     (cdr (assq 'arch-endianness (system-features))))
               (native-endianness)))))

; These are primops:
;
;          bytevector? make-bytevector bytevector-length

; These are inlined by common.imp.sch:
;
;          bytevector-u8-ref bytevector-s8-ref
;          bytevector-u8-set! bytevector-s8-set!

(define (bytevector-u8-ref bv i) (bytevector-ref bv i))

(define (bytevector-u8-set! bv i o) (bytevector-set! bv i o))

(define (bytevector-s8-ref b k)
  (u8->s8 (bytevector-u8-ref b k)))

(define (bytevector-s8-set! b k val)
  (bytevector-u8-set! b k (s8->u8 val)))

(define (bytevector-uint-ref bytevector index endness size)
  (case endness
   ((big)
    (do ((i 0 (+ i 1))
         (result 0 (+ (* 256 result)
                      (bytevector-u8-ref bytevector (+ index i)))))
        ((>= i size)
         result)))
   ((little)
    (do ((i (- size 1) (- i 1))
         (result 0 (+ (* 256 result)
                      (bytevector-u8-ref bytevector (+ index i)))))
        ((< i 0)
         result)))
   (else
    (bytevector-uint-ref bytevector index (native-endianness) size))))

(define (bytevector-sint-ref bytevector index endness size)
  (let* ((high-byte (bytevector-u8-ref bytevector
                               (if (eq? endness 'big)
                                   index
                                   (+ index size -1))))
         (uresult (bytevector-uint-ref bytevector index endness size)))
    (if (> high-byte 127)
        (- uresult (expt 256 size))
	uresult)))

; FIXME: Some of these procedures may not do enough range checking.

(define (bytevector-uint-set! bytevector index val endness size)
  (case endness
   ((little)
    (do ((i 0 (+ i 1))
         (val val (bytevector:div val 256)))
        ((>= i size)
         (unspecified))
      (bytevector-u8-set! bytevector (+ index i) (bytevector:mod val 256))))
   ((big)
    (do ((i (- size 1) (- i 1))
         (val val (bytevector:div val 256)))
        ((< i 0)
         (unspecified))
      (bytevector-u8-set! bytevector (+ index i) (bytevector:mod val 256))))
   (else
    (bytevector-uint-set! bytevector index val (native-endianness) size))))

(define (bytevector-sint-set! bytevector index val endness size)
  (let ((uval (if (< val 0)
                  (+ val (expt 256 size))
                  val)))
    (bytevector-uint-set! bytevector index uval endness size)))
  
(define bytevector-u16-ref (make-uint-ref 2))
(define bytevector-u16-set! (make-uint-set! 2))
(define bytevector-s16-ref (make-sint-ref 2))
(define bytevector-s16-set! (make-sint-set! 2))
(define bytevector-u16-native-ref (make-ref/native 2 bytevector-u16-ref))
(define bytevector-u16-native-set! (make-set!/native 2 bytevector-u16-set!))
(define bytevector-s16-native-ref (make-ref/native 2 bytevector-s16-ref))
(define bytevector-s16-native-set! (make-set!/native 2 bytevector-s16-set!))

(define bytevector-u32-ref (make-uint-ref 4))
(define bytevector-u32-set! (make-uint-set! 4))
(define bytevector-s32-ref (make-sint-ref 4))
(define bytevector-s32-set! (make-sint-set! 4))
(define bytevector-u32-native-ref (make-ref/native 4 bytevector-u32-ref))
(define bytevector-u32-native-set! (make-set!/native 4 bytevector-u32-set!))
(define bytevector-s32-native-ref (make-ref/native 4 bytevector-s32-ref))
(define bytevector-s32-native-set! (make-set!/native 4 bytevector-s32-set!))

(define bytevector-u64-ref (make-uint-ref 8))
(define bytevector-u64-set! (make-uint-set! 8))
(define bytevector-s64-ref (make-sint-ref 8))
(define bytevector-s64-set! (make-sint-set! 8))
(define bytevector-u64-native-ref (make-ref/native 8 bytevector-u64-ref))
(define bytevector-u64-native-set! (make-set!/native 8 bytevector-u64-set!))
(define bytevector-s64-native-ref (make-ref/native 8 bytevector-s64-ref))
(define bytevector-s64-native-set! (make-set!/native 8 bytevector-s64-set!))

(define (bytevector=? b1 b2)
  (if (or (not (bytevector? b1))
          (not (bytevector? b2)))
      (error 'bytevector=? "Illegal arguments: " b1 b2)
      (let ((n1 (bytevector-length b1))
            (n2 (bytevector-length b2)))
        (and (= n1 n2)
             (do ((i 0 (+ i 1)))
                 ((or (= i n1)
                      (not (= (bytevector-u8-ref b1 i)
                              (bytevector-u8-ref b2 i))))
                  (= i n1)))))))

; FIXME: should use word-at-a-time when possible

(define (bytevector-fill! b fill)
  (if (<= -128 fill -1)
      (bytevector-fill! b (+ fill 256))
      (let ((n (bytevector-length b)))
        (do ((i 0 (+ i 1)))
            ((= i n))
          (bytevector-u8-set! b i fill)))))

(define (r6rs:bytevector-copy! source source-start target target-start count)
  (if (>= source-start target-start)
      (do ((i 0 (+ i 1)))
          ((>= i count))
        (bytevector-u8-set! target
                       (+ target-start i)
                       (bytevector-u8-ref source (+ source-start i))))
      (do ((i (- count 1) (- i 1)))
          ((< i 0))
        (bytevector-u8-set! target
                       (+ target-start i)
                       (bytevector-u8-ref source (+ source-start i))))))

;;; Generalized from one argument for R7RS.

(define (bytevector-copy b . rest)
  (let* ((n (bytevector-length b))
         (start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest))) n (cadr rest)))
         (k (- end start))
         (b2 (make-bytevector k)))
    (r6rs:bytevector-copy! b start b2 0 k)
    b2))

(define (bytevector->u8-list b)
  (let ((n (bytevector-length b)))
    (do ((i (- n 1) (- i 1))
         (result '() (cons (bytevector-u8-ref b i) result)))
        ((< i 0)
         result))))

(define (bytevector->s8-list b)
  (let ((n (bytevector-length b)))
    (do ((i (- n 1) (- i 1))
         (result '() (cons (bytevector-s8-ref b i) result)))
        ((< i 0)
         result))))

(define (u8-list->bytevector vals)
  (let* ((n (length vals))
         (b (make-bytevector n)))
    (do ((vals vals (cdr vals))
         (i 0 (+ i 1)))
        ((null? vals))
      (bytevector-u8-set! b i (car vals)))
    b))

(define (s8-list->bytevector l)
  (let* ((n (length vals))
         (b (make-bytevector n)))
    (do ((vals vals (cdr vals))
         (i 0 (+ i 1)))
        ((null? vals))
      (bytevector-s8-set! b i (car vals)))
    b))

(define bytevector->uint-list (make-bytevector->int-list bytevector-uint-ref))
(define bytevector->sint-list (make-bytevector->int-list bytevector-sint-ref))

(define uint-list->bytevector (make-int-list->bytevector bytevector-uint-set!))
(define sint-list->bytevector (make-int-list->bytevector bytevector-sint-set!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Bytevector operations that involve IEEE floating point.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME: these definitions are temporary, in case the
; infinite? and nan? procedures aren't yet in a system's
; preliminary version of (r6rs base).

(define (bytevector:nan? x) (nan? x))

(define (bytevector:infinite? x) (infinite? x))

; Magic numbers for IEEE-754 single and double precision:
;     the exponent bias (127 or 1023)
;     the integer value of the hidden bit (2^23 or 2^52)

(define bytevector:single-maxexponent 255)
(define bytevector:single-bias
  (bytevector:div bytevector:single-maxexponent 2))
(define bytevector:single-hidden-bit 8388608)

(define bytevector:double-maxexponent 2047)
(define bytevector:double-bias
  (bytevector:div bytevector:double-maxexponent 2))
(define bytevector:double-hidden-bit 4503599627370496)

; Given four exact integers, returns
;
;     (-1)^sign * (2^exponent) * p/q
;
; as an inexact real.

(define (bytevector:normalized sign exponent p q)
  (let* ((p/q (exact->inexact (/ p q)))
         (x (* p/q (expt 2.0 exponent))))
    (cond ((= sign 0) x)
          ((= x 0.0) -0.0)
          (else (- x)))))

; Given exact positive integers p and q,
; returns three values:
; exact integers exponent, p2, and q2 such that
;     q2 <= p2 < q2+q2
;     p / q = (p2 * 2^exponent) / q2

(define (bytevector:normalized-ieee-parts p q)
  (cond ((< p q)
         (do ((p p (+ p p))
              (e 0 (- e 1)))
             ((>= p q)
              (values e p q))))
        ((<= (+ q q) p)
         (do ((q q (+ q q))
              (e 0 (+ e 1)))
             ((< p (+ q q))
              (values e p q))))
        (else
         (values 0 p q))))

; Given an inexact real x, an exponent bias, and an exact positive
; integer q that is a power of 2 representing the integer value of
; the hidden bit, returns three exact integers:
;
; sign
; biased-exponent
; p
;
; If x is normalized, then 0 < biased-exponent <= bias+bias,
; q <= p < 2*q, and
;
;     x = (-1)^sign * (2^(biased-exponent - bias)) * p/q
;
; If x is denormalized, then p < q and the equation holds.
; If x is zero, then biased-exponent and p are zero.
; If x is infinity, then biased-exponent = bias+bias+1 and p=0.
; If x is a NaN, then biased-exponent = bias+bias+1 and p>0.
;

(define (bytevector:ieee-parts x bias q)
  (cond ((bytevector:nan? x)
         (values 0 (+ bias bias 1) (- q 1)))
        ((bytevector:infinite? x)
         (values (if (positive? x) 0 1) (+ bias bias 1) 0))
        ((zero? x)
         (values (if (eqv? x -0.0) 1 0) 0 0))
        (else
         (let* ((sign (if (negative? x) 1 0))
                (y (inexact->exact (abs x)))
                (num (numerator y))
                (den (denominator y)))
           (call-with-values
            (lambda () (bytevector:normalized-ieee-parts num den))
            (lambda (exponent num den)
              (let ((biased-exponent (+ exponent bias)))
                (cond ((< 0 biased-exponent (+ bias bias 1))
                       ; within the range of normalized numbers
                       (if (<= den q)
                           (let* ((factor (/ q den))
                                  (num*factor (* num factor)))
                             (if (integer? factor)
                                 (values sign biased-exponent num*factor)
                                 (error 'bytevector:ieee-parts
                                        "this shouldn't happen: " x bias q)))
                           (let* ((factor (/ den q))
                                  (num*factor (/ num factor))
                                  (p (round num*factor)))
                             (if (< p (+ q q))
                                 (values sign
                                         biased-exponent
                                         p)
                                 (values sign
                                         (+ biased-exponent 1)
                                         (- p q))))))
                      ((>= biased-exponent (+ bias bias 1))
                       ; infinity
                       (values (if (positive? x) 0 1) (+ bias bias 1) 0))
                      (else
                       ; denormalized
                       ; FIXME: this has the double rounding bug
                       (do ((biased biased-exponent (+ biased 1))
                            (num (round (/ (* q num) den))
                                 (round (bytevector:div num 2))))
                           ((and (< num q) (= biased 1))
                            (values sign biased num))))))))))))

; The exported procedures

(define (bytevector-ieee-double-native-ref bytevector k)
  (assert (eq? 0 (fxand k 7)))
  (let ((x (make-bytevector 12)))
    (bytevector-set! x 4  (bytevector-ref bytevector (+ k 0)))
    (bytevector-set! x 5  (bytevector-ref bytevector (+ k 1)))
    (bytevector-set! x 6  (bytevector-ref bytevector (+ k 2)))
    (bytevector-set! x 7  (bytevector-ref bytevector (+ k 3)))
    (bytevector-set! x 8  (bytevector-ref bytevector (+ k 4)))
    (bytevector-set! x 9  (bytevector-ref bytevector (+ k 5)))
    (bytevector-set! x 10 (bytevector-ref bytevector (+ k 6)))
    (bytevector-set! x 11 (bytevector-ref bytevector (+ k 7)))
    (typetag-set! x 2)
    x))

(define (bytevector-ieee-double-native-set! bytevector k x)
  (assert (eq? 0 (fxand k 7)))
  (bytevector-set! bytevector (+ k 0) (bytevector-like-ref x 4))
  (bytevector-set! bytevector (+ k 1) (bytevector-like-ref x 5))
  (bytevector-set! bytevector (+ k 2) (bytevector-like-ref x 6))
  (bytevector-set! bytevector (+ k 3) (bytevector-like-ref x 7))
  (bytevector-set! bytevector (+ k 4) (bytevector-like-ref x 8))
  (bytevector-set! bytevector (+ k 5) (bytevector-like-ref x 9))
  (bytevector-set! bytevector (+ k 6) (bytevector-like-ref x 10))
  (bytevector-set! bytevector (+ k 7) (bytevector-like-ref x 11)))

(define (bytevector-ieee-single-native-ref bytevector k)
  (assert (eq? 0 (fxand k 3)))
  (let* ((origb0 (bytevector-u8-ref bytevector k))
         (origb1 (bytevector-u8-ref bytevector (+ k 1)))
         (origb2 (bytevector-u8-ref bytevector (+ k 2)))
         (origb3 (bytevector-u8-ref bytevector (+ k 3)))
         (swap? (not (eq? 'big (native-endianness))))
         (b0 (if swap? origb3 origb0))
         (b1 (if swap? origb2 origb1))
         (b2 (if swap? origb1 origb2))
         (b3 (if swap? origb0 origb3)))
    (let ((sign (bytevector:div b0 128))
          (exponent (+ (* 2 (bytevector:mod b0 128))
                       (bytevector:div b1 128)))
          (fraction (+ (* 256 256 (bytevector:mod b1 128))
                       (* 256 b2)
                       b3)))
      (cond ((< 0 exponent bytevector:single-maxexponent)
             ; normalized (the usual case)
             (bytevector:normalized sign
                               (- exponent bytevector:single-bias)
                               (+ bytevector:single-hidden-bit fraction)
                               bytevector:single-hidden-bit))
            ((= 0 exponent)
             (cond ((> fraction 0)
                    ; denormalized
                    (bytevector:normalized sign
                                      (+ (- bytevector:single-bias) 1)
                                      fraction
                                      bytevector:single-hidden-bit))
                   ((= sign 0) 0.0)
                   (else -0.0)))
            ((= 0 fraction)
             (if (= sign 0) +inf.0 -inf.0))
            (else
             (if (= sign 0) +nan.0 -nan.0))))))

(define (bytevector-ieee-single-native-set! bytevector k x)
  (assert (eq? 0 (fxand k 3)))
  (call-with-values
   (lambda ()
     (bytevector:ieee-parts x bytevector:single-bias
                            bytevector:single-hidden-bit))
   (lambda (sign biased-exponent frac)
     (define (store! sign biased-exponent frac)
       (if (eq? 'big (native-endianness))
           (begin
            (bytevector-u8-set! bytevector k
                                (+ (* 128 sign)
                                (bytevector:div biased-exponent 2)))
            (bytevector-u8-set! bytevector (+ k 1)
                                 (+ (* 128 (bytevector:mod biased-exponent 2))
                                    (bytevector:div frac (* 256 256))))
            (bytevector-u8-set! bytevector (+ k 2)
                                (bytevector:div
                                 (bytevector:mod frac (* 256 256)) 256))
            (bytevector-u8-set! bytevector (+ k 3)
                                (bytevector:mod frac 256)))
           (begin
            (bytevector-u8-set! bytevector (+ k 3)
                                (+ (* 128 sign)
                                (bytevector:div biased-exponent 2)))
            (bytevector-u8-set! bytevector (+ k 2)
                                 (+ (* 128 (bytevector:mod biased-exponent 2))
                                    (bytevector:div frac (* 256 256))))
            (bytevector-u8-set! bytevector (+ k 1)
                                (bytevector:div
                                 (bytevector:mod frac (* 256 256)) 256))
            (bytevector-u8-set! bytevector k
                                (bytevector:mod frac 256))))
       (unspecified))
     (cond ((= biased-exponent bytevector:single-maxexponent)
            (store! sign biased-exponent frac))
           ((< frac bytevector:single-hidden-bit)
            (store! sign 0 frac))
           (else
            (store! sign biased-exponent
                    (- frac bytevector:single-hidden-bit)))))))

(define (bytevector-ieee-single-ref bytevector k endianness)
  (if (eq? endianness (native-endianness))
      (if (= 0 (bytevector:mod k 4))
          (bytevector-ieee-single-native-ref bytevector k)
          (let ((b (make-bytevector 4)))
            (r6rs:bytevector-copy! bytevector k b 0 4)
            (bytevector-ieee-single-native-ref b 0)))
      (let ((b (make-bytevector 4)))
        (bytevector-u8-set! b 0 (bytevector-u8-ref bytevector (+ k 3)))
        (bytevector-u8-set! b 1 (bytevector-u8-ref bytevector (+ k 2)))
        (bytevector-u8-set! b 2 (bytevector-u8-ref bytevector (+ k 1)))
        (bytevector-u8-set! b 3 (bytevector-u8-ref bytevector k))
        (bytevector-ieee-single-native-ref b 0))))

(define (bytevector-ieee-double-ref bytevector k endianness)
  (if (eq? endianness (native-endianness))
      (if (= 0 (bytevector:mod k 8))
          (bytevector-ieee-double-native-ref bytevector k)
          (let ((b (make-bytevector 8)))
            (r6rs:bytevector-copy! bytevector k b 0 8)
            (bytevector-ieee-double-native-ref b 0)))
      (let ((b (make-bytevector 8)))
        (bytevector-u8-set! b 0 (bytevector-u8-ref bytevector (+ k 7)))
        (bytevector-u8-set! b 1 (bytevector-u8-ref bytevector (+ k 6)))
        (bytevector-u8-set! b 2 (bytevector-u8-ref bytevector (+ k 5)))
        (bytevector-u8-set! b 3 (bytevector-u8-ref bytevector (+ k 4)))
        (bytevector-u8-set! b 4 (bytevector-u8-ref bytevector (+ k 3)))
        (bytevector-u8-set! b 5 (bytevector-u8-ref bytevector (+ k 2)))
        (bytevector-u8-set! b 6 (bytevector-u8-ref bytevector (+ k 1)))
        (bytevector-u8-set! b 7 (bytevector-u8-ref bytevector k))
        (bytevector-ieee-double-native-ref b 0))))

(define (bytevector-ieee-single-set! bytevector k x endianness)
  (if (eq? endianness (native-endianness))
      (if (= 0 (bytevector:mod k 4))
          (bytevector-ieee-single-native-set! bytevector k x)
          (let ((b (make-bytevector 4)))
            (bytevector-ieee-single-native-set! b 0 x)
            (r6rs:bytevector-copy! b 0 bytevector k 4)))
      (let ((b (make-bytevector 4)))
        (bytevector-ieee-single-native-set! b 0 x)
        (bytevector-u8-set! bytevector k (bytevector-u8-ref b 3))
        (bytevector-u8-set! bytevector (+ k 1) (bytevector-u8-ref b 2))
        (bytevector-u8-set! bytevector (+ k 2) (bytevector-u8-ref b 1))
        (bytevector-u8-set! bytevector (+ k 3) (bytevector-u8-ref b 0)))))

(define (bytevector-ieee-double-set! bytevector k x endianness)
  (if (eq? endianness (native-endianness))
      (if (= 0 (bytevector:mod k 8))
          (bytevector-ieee-double-native-set! bytevector k x)
          (let ((b (make-bytevector 8)))
            (bytevector-ieee-double-native-set! b 0 x)
            (r6rs:bytevector-copy! b 0 bytevector k 8)))
      (let ((b (make-bytevector 8)))
        (bytevector-ieee-double-native-set! b 0 x)
        (bytevector-u8-set! bytevector k (bytevector-u8-ref b 7))
        (bytevector-u8-set! bytevector (+ k 1) (bytevector-u8-ref b 6))
        (bytevector-u8-set! bytevector (+ k 2) (bytevector-u8-ref b 5))
        (bytevector-u8-set! bytevector (+ k 3) (bytevector-u8-ref b 4))
        (bytevector-u8-set! bytevector (+ k 4) (bytevector-u8-ref b 3))
        (bytevector-u8-set! bytevector (+ k 5) (bytevector-u8-ref b 2))
        (bytevector-u8-set! bytevector (+ k 6) (bytevector-u8-ref b 1))
        (bytevector-u8-set! bytevector (+ k 7) (bytevector-u8-ref b 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Temporary, until Larceny supports (r6rs arithmetic bitwise).
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax bitwise-arithmetic-shift-left
  (syntax-rules ()
   ((_ n0 k0)
    (let ((n n0) (k k0))
      (fxlsh n k)))))

(define-syntax bitwise-and
  (syntax-rules ()
   ((_ i0 j0)
    (let ((i i0) (j j0))
      (fxlogand i j)))))

(define-syntax bitwise-ior
  (syntax-rules ()
   ((_ i0 j0)
    (let ((i i0) (j j0))
      (fxlogior i j)))))

(define-syntax bitwise-bit-field
  (syntax-rules ()
   ((_ n0 i0 j0)
    (let ((n n0) (i i0) (j j0))
      (fxrshl (fxlogxor n (fxlsh (fxrshl n j) j)) i)))))

(define-syntax bytevector-u8-ref
  (syntax-rules ()
   ((_ bv i)
    (bytevector-ref bv i))))

(define-syntax bytevector-u8-set!
  (syntax-rules ()
   ((_ bv i j)
    (bytevector-set! bv i j))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Conversions between bytevectors and strings.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string->utf8 string . rest)
  (let* ((n (string-length string))
         (start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest))) n (cadr rest)))
         (k (do ((i start (+ i 1))
                 (k 0 (+ k (let ((sv (char->integer (string-ref string i))))
                             (cond ((<= sv #x007f) 1)
                                   ((<= sv #x07ff) 2)
                                   ((<= sv #xffff) 3)
                                   (else 4))))))
                ((= i end) k)))
         (bv (make-bytevector k)))
    (define (loop i j)
      (if (>= i end)
          bv
          (let ((sv (char->integer (string-ref string i))))
            (cond ((<= sv #x007f)
                   (bytevector-u8-set! bv j sv)
                   (loop (+ i 1) (+ j 1)))
                  ((<= sv #x07ff)
                   (let ((u0 (bitwise-ior #b11000000
                                          (bitwise-bit-field sv 6 11)))
                         (u1 (bitwise-ior #b10000000
                                          (bitwise-bit-field sv 0 6))))
                     (bytevector-u8-set! bv j u0)
                     (bytevector-u8-set! bv (+ j 1) u1)
                     (loop (+ i 1) (+ j 2))))
                  ((<= sv #xffff)
                   (let ((u0 (bitwise-ior #b11100000
                                          (bitwise-bit-field sv 12 16)))
                         (u1 (bitwise-ior #b10000000
                                          (bitwise-bit-field sv 6 12)))
                         (u2 (bitwise-ior #b10000000
                                          (bitwise-bit-field sv 0 6))))
                     (bytevector-u8-set! bv j u0)
                     (bytevector-u8-set! bv (+ j 1) u1)
                     (bytevector-u8-set! bv (+ j 2) u2)
                     (loop (+ i 1) (+ j 3))))
                  (else
                   (let ((u0 (bitwise-ior #b11110000
                                          (bitwise-bit-field sv 18 21)))
                         (u1 (bitwise-ior #b10000000
                                          (bitwise-bit-field sv 12 18)))
                         (u2 (bitwise-ior #b10000000
                                          (bitwise-bit-field sv 6 12)))
                         (u3 (bitwise-ior #b10000000
                                          (bitwise-bit-field sv 0 6))))
                     (bytevector-u8-set! bv j u0)
                     (bytevector-u8-set! bv (+ j 1) u1)
                     (bytevector-u8-set! bv (+ j 2) u2)
                     (bytevector-u8-set! bv (+ j 3) u3)
                     (loop (+ i 1) (+ j 4))))))))
    (loop start 0)))

; Given a bytevector containing the UTF-8 encoding
; of a string, decodes and returns a newly allocated
; string (unless empty).
;
; If the bytevector begins with the three-byte sequence
; #xef #xbb #xbf, then those bytes are ignored.  (They
; are conventionally used as a signature to indicate
; UTF-8 encoding.  The string->utf8 procedure does not
; emit those bytes, but UTF-8 encodings produced by
; other sources may contain them.)
;
; The main difficulty is that Unicode Corrigendum #1
; ( http://unicode.org/versions/corrigendum1.html )
; forbids interpretation of illegal code unit sequences,
; which include non-shortest forms.  A UTF-8 decoder
; must therefore detect non-shortest forms and treat
; them as errors.
;
; Another difficulty is that the specification of this
; particular decoder says it will replace an illegal
; code unit sequence by a replacement character, but
; does not fully specify the recovery process, which
; affects the number of replacement characters that
; will appear in the result.
;
; Ignoring the special treatment of a ZERO WIDTH
; NO-BREAK SPACE at the beginning of a bytevector,
; the decoding is implemented by the following
; state machine.  q0 is the start state and the
; only state in which no more input is acceptable.
;
; q0 --- dispatching on the first byte of a character
; Dispatch on the next byte according to Table 3.1B
; of Corrigendum #1.  Note that there are two error
; ranges not shown in that table, for a total of 9.
; The 00..7f, 80..c1, and f5..ff ranges remain in
; state q0.  00..7f is an Ascii character; the other
; two ranges that remain in state q0 are illegal.
;
; q1 --- expecting one more byte in range 80..bf
;
; q2 --- expecting two more bytes, the first in range lower..bf
;
; q3 --- expecting three more bytes, the first in range lower..upper

(define (utf8->string bv . rest)
  (let* ((n (bytevector-length bv))
         (start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest))) n (cadr rest)))
         (replacement-character (integer->char #xfffd))
         (begins-with-bom?
          (and (<= 3 n)
               (= #xef (bytevector-u8-ref bv 0))
               (= #xbb (bytevector-u8-ref bv 1))
               (= #xbf (bytevector-u8-ref bv 2)))))

    (define bits->char (lambda (bits)
                         (cond ((<= 0 bits #xd7ff)
                                (integer->char bits))
                               ((<= #xe000 bits #x10ffff)
                                (integer->char bits))
                               (else
                                replacement-character))))

    (define (result-length)
      ; i is index of the next byte
      ; k is the number of characters encoded by bytes 0 through i-1
      (define (q0 i k)
        (if (= i end)
            k
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1))
                  (k1 (+ k 1)))
              (cond ((<= unit #x7f)
                     (q0 i1 k1))
                    ((<= unit #xc1)
                     ; illegal
                     (q0 i1 k1))
                    ((<= unit #xdf)
                     (q1 i1 k1))
                    ((<= unit #xe0)
                     (q2 i1 k1 #xa0))
                    ((<= unit #xef)
                     (q2 i1 k1 #x80))
                    ((<= unit #xf0)
                     (q3 i1 k1 #x90 #xbf))
                    ((<= unit #xf3)
                     (q3 i1 k1 #x80 #xbf))
                    ((<= unit #xf4)
                     (q3 i1 k1 #x80 #x8f))
                    (else
                     ; illegal
                     (q0 i1 k1))))))
      (define (q1 i k)
        (if (= i end)
            k
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1)))
              (cond ((< unit #x80)
                     ; illegal
                     (q0 i k))
                    ((<= unit #xbf)
                     (q0 i1 k))
                    (else
                     ; illegal
                     (q0 i k))))))
      (define (q2 i k lower)
        (if (= i end)
            k
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1)))
              (cond ((< unit lower)
                     ; illegal
                     (q0 i k))
                    ((<= unit #xbf)
                     (q1 i1 k))
                    (else
                     ; illegal
                     (q0 i k))))))
      (define (q3 i k lower upper)
        (if (= i end)
            k
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1)))
              (cond ((< unit lower)
                     ; illegal
                     (q0 i k))
                    ((<= unit upper)
                     (q2 i1 k #x80))
                    (else
                     ; illegal
                     (q0 i k))))))
      (if (and begins-with-bom? (= start 0))
          (q0 3 0)
          (q0 start 0)))

    (let* ((k (result-length))
           (s (make-string k)))

      ; i is index of the next byte in bv
      ; k is index of the next character in s

      (define (q0 i k)
        (if (< i end)
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1))
                  (k1 (+ k 1)))
              (cond ((<= unit #x7f)
                     (string-set! s k (integer->char unit))
                     (q0 i1 k1))
                    ((<= unit #xc1)
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i1 k1))
                    ((<= unit #xdf)
                     (q1 i1 k (bitwise-and unit #x1f)))
                    ((<= unit #xe0)
                     (q2 i1 k #xa0 0))
                    ((<= unit #xef)
                     (q2 i1 k #x80 (bitwise-and unit #x0f)))
                    ((<= unit #xf0)
                     (q3 i1 k #x90 #xbf 0))
                    ((<= unit #xf3)
                     (q3 i1 k #x80 #xbf (bitwise-and unit #x07)))
                    ((<= unit #xf4)
                     (q3 i1 k #x80 #x8f (bitwise-and unit #x07)))
                    (else
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i1 k1))))))
      (define (q1 i k bits)
        (if (= i end)
            (string-set! s k replacement-character)
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1))
                  (k1 (+ k 1)))
              (cond ((< unit #x80)
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i k1))
                    ((<= unit #xbf)
                     (string-set! s k (bits->char
                                       (bitwise-ior
                                        (bitwise-arithmetic-shift-left bits 6)
                                        (bitwise-and unit #x3f))))
                     (q0 i1 k1))
                    (else
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i k1))))))
      (define (q2 i k lower bits)
        (if (= i end)
            (string-set! s k replacement-character)
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1)))
              (cond ((< unit lower)
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i (+ k 1)))
                    ((<= unit #x00bf)
                     (q1 i1 k (bitwise-ior
                               (bitwise-arithmetic-shift-left bits 6)
                               (bitwise-and unit #x3f))))
                    (else
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i (+ k 1)))))))
      (define (q3 i k lower upper bits)
        (if (= i end)
            (string-set! s k replacement-character)
            (let ((unit (bytevector-u8-ref bv i))
                  (i1 (+ i 1)))
              (cond ((< unit lower)
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i (+ k 1)))
                    ((<= unit upper)
                     (q2 i1 k #x80 (bitwise-ior
                                    (bitwise-arithmetic-shift-left bits 6)
                                    (bitwise-and unit #x3f))))
                    (else
                     ; illegal
                     (string-set! s k replacement-character)
                     (q0 i (+ k 1)))))))
      (if (and begins-with-bom? (= start 0))
          (q0 3 0)
          (q0 start 0))
      s)))

; (utf-16-codec) might write a byte order mark,
; so it's better not to use textual i/o for this.

(define (string->utf16 string . rest)
  (let* ((endianness (cond ((null? rest) 'big)
                           ((not (null? (cdr rest)))
                            (apply assertion-violation 'string->utf16
                                   "too many arguments" string rest))
                           ((eq? (car rest) 'big) 'big)
                           ((eq? (car rest) 'little) 'little)
                           (else (endianness-violation
                                  'string->utf16
                                  (car rest)))))

         ; endianness-dependent adjustments to indexing

         (hi (if (eq? 'big endianness) 0 1))
         (lo (- 1 hi))

         (n (string-length string)))

    (define (result-length)
      (do ((i 0 (+ i 1))
           (k 0 (let ((sv (char->integer (string-ref string i))))
                  (if (< sv #x10000) (+ k 2) (+ k 4)))))
          ((= i n) k)))

    (let ((bv (make-bytevector (result-length))))

      (define (loop i k)
        (if (< i n)
            (let ((sv (char->integer (string-ref string i))))
              (if (< sv #x10000)
                  (let ((hibits (bitwise-bit-field sv 8 16))
                        (lobits (bitwise-bit-field sv 0 8)))
                    (bytevector-u8-set! bv (+ k hi) hibits)
                    (bytevector-u8-set! bv (+ k lo) lobits)
                    (loop (+ i 1) (+ k 2)))
                  (let* ((x (- sv #x10000))
                         (hibits (bitwise-bit-field x 10 20))
                         (lobits (bitwise-bit-field x 0 10))
                         (hi16 (bitwise-ior #xd800 hibits))
                         (lo16 (bitwise-ior #xdc00 lobits))
                         (hi1 (bitwise-bit-field hi16 8 16))
                         (lo1 (bitwise-bit-field hi16 0 8))
                         (hi2 (bitwise-bit-field lo16 8 16))
                         (lo2 (bitwise-bit-field lo16 0 8)))
                    (bytevector-u8-set! bv (+ k hi) hi1)
                    (bytevector-u8-set! bv (+ k lo) lo1)
                    (bytevector-u8-set! bv (+ k hi 2) hi2)
                    (bytevector-u8-set! bv (+ k lo 2) lo2)
                    (loop (+ i 1) (+ k 4)))))))

      (loop 0 0)
      bv)))

; The second argument should be optional should be optional,
; and was optional in the R5.94RS draft, but was made mandatory
; in the R5.95RS draft by someone who misinterpreted John Cowan's
; response of 27 May 2007 to an ambiguous question posed by
; Mike Sperber.  This error was not spotted by anyone, and
; made its way into the ratified R6RS.
;
; Larceny will not perpetuate this error.  In Larceny, the
; second argument is optional.
;
; The R6RS also contradicts itself by saying the bytevector
; will be decoded according to UTF-16BE or UTF-16LE, which
; implies any BOM must be ignored.  I believe the intended
; specification was along these lines:
;
;    Bytevector is decoded acccording to UTF-16, UTF-16BE,
;    UTF-16LE, or a fourth encoding scheme that differs from
;    all three of those, depending upon the optional arguments
;    endianness and endianness-mandatory.  If endianness
;    is the symbol big and endianness-mandatory is absent
;    or false, then bytevector is decoded according to
;    UTF-16.  If endianness is the symbol big and
;    endianness-mandatory is #t, then bytevector is decoded
;    according to UTF-16BE.  If endianness is the symbol
;    little and endianness-mandatory is #t, then bytevector
;    is decoded according to UTF-16LE.  If endianness is
;    the symbol little and endianness-mandatory is absent
;    or #f, then the bytevector is decoded according to
;    UTF-16 if it begins with a BOM but is decoded according
;    to UTF-16LE if it does not begin with a BOM; note that
;    this fourth decoding does not correspond to any of the
;    seven Unicode encoding schemes that are defined by the
;    Unicode standard.
;
; That is the specification implemented by Larceny.

(define (utf16->string bytevector . rest)
  (let* ((n (bytevector-length bytevector))

         (begins-with-bom?
          (and (<= 2 n)
               (let ((b0 (bytevector-u8-ref bytevector 0))
                     (b1 (bytevector-u8-ref bytevector 1)))
                 (or (and (= b0 #xfe) (= b1 #xff) 'big)
                     (and (= b0 #xff) (= b1 #xfe) 'little)))))

         (mandatory? (cond ((or (null? rest) (null? (cdr rest)))
                            #f)
                           ((and (null? (cddr rest))
                                 (boolean? (cadr rest)))
                            (cadr rest))
                           (else
                            (apply assertion-violation 'utf16->string
                                   "illegal arguments" bytevector rest))))

         (endianness (cond ((null? rest)
                            (or begins-with-bom? 'big))
                           ((eq? (car rest) 'big)
                            (if mandatory?
                                'big
                                (or begins-with-bom? 'big)))
                           ((eq? (car rest) 'little)
                            (if mandatory?
                                'little
                                (or begins-with-bom? 'little)))
                           (else (endianness-violation
                                  'utf16->string
                                  (car rest)))))

         (begins-with-bom? (if mandatory? #f begins-with-bom?))

         (endianness (if mandatory? (car rest) endianness))

         ; endianness-dependent adjustments to indexing

         (hi (if (eq? 'big endianness) 0 1))
         (lo (- 1 hi))

         (replacement-character (integer->char #xfffd)))

    ; computes the length of the encoded string

    (define (result-length)
      (define (loop i k)
        (if (>= i n)
            k
            (let ((octet (bytevector-u8-ref bytevector i)))
              (cond ((< octet #xd8)
                     (loop (+ i 2) (+ k 1)))
                    ((< octet #xdc)
                     (let* ((i2 (+ i 2))
                            (octet2 (if (< i2 n)
                                        (bytevector-u8-ref bytevector i2)
                                        0)))
                       (if (<= #xdc octet2 #xdf)
                           (loop (+ i 4) (+ k 1))
                           ; bad surrogate pair, becomes replacement character
                           (loop i2 (+ k 1)))))
                    (else (loop (+ i 2) (+ k 1)))))))
      (if begins-with-bom?
          (loop (+ hi 2) 0)
          (loop hi 0)))

    (if (odd? n)
        (assertion-violation 'utf16->string
                             "bytevector has odd length" bytevector))

    (let ((s (make-string (result-length))))
      (define (loop i k)
        (if (< i n)
            (let ((hibits (bytevector-u8-ref bytevector (+ i hi)))
                  (lobits (bytevector-u8-ref bytevector (+ i lo))))
              (cond ((< hibits #xd8)
                     (let ((c (integer->char
                               (bitwise-ior
                                (bitwise-arithmetic-shift-left hibits 8)
                                lobits))))
                       (string-set! s k c))
                     (loop (+ i 2) (+ k 1)))
                    ((< hibits #xdc)
                     (let* ((i2 (+ i hi 2))
                            (i3 (+ i lo 2))
                            (octet2 (if (< i2 n)
                                        (bytevector-u8-ref bytevector i2)
                                        0))
                            (octet3 (if (< i2 n)
                                        (bytevector-u8-ref bytevector i3)
                                        0)))
                       (if (<= #xdc octet2 #xdf)
                           (let* ((sv (+ #x10000
                                         (bitwise-arithmetic-shift-left
                                          (bitwise-and
                                           (bitwise-ior
                                            (bitwise-arithmetic-shift-left
                                             hibits 8)
                                            lobits)
                                           #x03ff)
                                          10)
                                         (bitwise-and
                                          (bitwise-ior
                                           (bitwise-arithmetic-shift-left
                                            octet2 8)
                                           octet3)
                                          #x03ff)))
                                  (c (if (<= #x10000 sv #x10ffff)
                                         (integer->char sv)
                                         replacement-character)))
                             (string-set! s k c)
                             (loop (+ i 4) (+ k 1)))
                           ; bad surrogate pair
                           (begin (string-set! s k replacement-character)
                                  (loop (+ i 2) (+ k 1))))))
                    ((< hibits #xe0)
                     ; second surrogate not preceded by a first surrogate
                     (string-set! s k replacement-character)
                     (loop (+ i 2) (+ k 1)))
                    (else
                     (let ((c (integer->char
                               (bitwise-ior
                                (bitwise-arithmetic-shift-left hibits 8)
                                lobits))))
                       (string-set! s k c))
                     (loop (+ i 2) (+ k 1)))))))
      (if begins-with-bom?
          (loop 2 0)
          (loop 0 0))
      s)))

; There is no utf-32-codec, so we can't use textual i/o for this.

(define (string->utf32 string . rest)
  (let* ((endianness (cond ((null? rest) 'big)
                           ((eq? (car rest) 'big) 'big)
                           ((eq? (car rest) 'little) 'little)
                           (else (endianness-violation
                                  'string->utf32
                                  (car rest)))))
         (n (string-length string))
         (result (make-bytevector (* 4 n))))
    (do ((i 0 (+ i 1)))
        ((= i n) result)
      (bytevector-u32-set! result
                           (* 4 i)
                           (char->integer (string-ref string i))
                           endianness))))

; There is no utf-32-codec, so we can't use textual i/o for this.

(define (utf32->string bytevector . rest)
  (let* ((n (bytevector-length bytevector))

         (begins-with-bom?
          (and (<= 4 n)
               (let ((b0 (bytevector-u8-ref bytevector 0))
                     (b1 (bytevector-u8-ref bytevector 1))
                     (b2 (bytevector-u8-ref bytevector 2))
                     (b3 (bytevector-u8-ref bytevector 3)))
                 (or (and (= b0 0) (= b1 0) (= b2 #xfe) (= b3 #xff)
                          'big)
                     (and (= b0 #xff) (= b1 #xfe) (= b2 0) (= b3 0)
                          'little)))))

         (mandatory? (cond ((or (null? rest) (null? (cdr rest)))
                            #f)
                           ((and (null? (cddr rest))
                                 (boolean? (cadr rest)))
                            (cadr rest))
                           (else
                            (apply assertion-violation 'utf32->string
                                   "illegal arguments" bytevector rest))))

         (endianness (cond ((null? rest)
                            (or begins-with-bom? 'big))
                           ((eq? (car rest) 'big)
                            (if mandatory?
                                'big
                                (or begins-with-bom? 'big)))
                           ((eq? (car rest) 'little)
                            (if mandatory?
                                'little
                                (or begins-with-bom? 'little)))
                           (else (endianness-violation
                                  'utf32->string
                                  (car rest)))))

         (begins-with-bom? (if mandatory? #f begins-with-bom?))

         (endianness (if mandatory? (car rest) endianness))

         (i0 (if begins-with-bom? 4 0))

         (result (if (zero? (remainder n 4))
                     (make-string (quotient (- n i0) 4))
                     (assertion-violation
                      'utf32->string
                      "Bytevector has bad length." bytevector))))

    (do ((i i0 (+ i 4))
         (j 0 (+ j 1)))
        ((= i n) result)
      (let* ((sv (bytevector-u32-ref bytevector i endianness))
             (sv (cond ((< sv #xd800) sv)
                       ((< sv #xe000) #xfffd) ; replacement character
                       ((< sv #x110000) sv)
                       (else #xfffd)))        ; replacement character
             (c (integer->char sv)))
        (string-set! result j c)))))

(define (endianness-violation who what)
  (assertion-violation who "bad endianness" what))
