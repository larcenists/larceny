;;; Copyright 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For Emacs:
;;; (put 'fxdefine 'scheme-indent-function 1)
;;; (put 'fxdefine+ 'scheme-indent-function 1)

(define-syntax assert
  (syntax-rules ()
    ((_ <expr> <message> . <objs>)
     (unless <expr>
       (apply error <message> (list . <objs>))))))

;;; Basically (x1 x2 ...) -> (begin (f x1) (f x2) ...), but resorting to
;;; run-time 'for-each' in the last position when the input list is improper,
;;; i.e. it contains a rest-arguments parameter.  'f' may be a macro.

(define-syntax for-args
  (syntax-rules ()
    ((_ <operator> ())
     #f)
    ((_ <operator> <args>)
     (for-args <operator> <args> ()))
    ((_ <operator> (<first> . <rest>) (<body> ...))
     (for-args <operator> <rest> (<body> ... (<operator> <first>))))
    ((_ <operator> () (<body> ...))
     (begin <body> ... (values)))
    ((_ <operator> <rest> (<body> ...))
     (begin <body> ... (for-each (lambda (arg) (<operator> arg)) <rest>)))))

(define (fixnum? obj)
  (and (exact-integer? obj)
       (<= (least-fixnum) obj (greatest-fixnum))))

(define (fixnum-width) W)
(define (least-fixnum) (expt -2 (- W 1)))
(define (greatest-fixnum) (- 0 (+ 1 (least-fixnum))))

(define-syntax assert-fixnum-arg
  (syntax-rules ()
    ((_ <arg>)
     (assert (fixnum? <arg>) "argument is not a fixnum" <arg>))))

(define-syntax assert-fixnum-result
  (syntax-rules ()
    ((_ <result>)
     (assert (fixnum? <result>) "result is not a fixnum" <result>))))

;;; fxdefine checks the arguments; fxdefine+ also checks the return value.

(define-syntax fxdefine
  (syntax-rules ()
    ((fxdefine <name>
       (<args>
        <body0> <body> ...)
       ...)
     (define <name>
       (case-lambda
         (<args>
          (for-args assert-fixnum-arg <args>)
          <body0> <body> ...)
         ...)))))

(define-syntax fxdefine+
  (syntax-rules ()
    ((fxdefine+ <name>
       (<args>
        <body0> <body> ...)
       ...)
     (fxdefine <name>
       (<args>
        (let ((result (begin <body0> <body> ...)))
          (assert-fixnum-result result)
          result))
       ...))))

(define-syntax fxdefine-comparison
  (syntax-rules ()
    ((_ <name> <compare>)
     (fxdefine <name>
       ((fx1 fx2)
        (<compare> fx1 fx2))
       ((fx1 fx2 fx3)
        (<compare> fx1 fx2 fx3))
       ((fx1 fx2 fx3 fx4)
        (<compare> fx1 fx2 fx3 fx4))
       ((fx1 fx2 fx3 fx4 fx5)
        (<compare> fx1 fx2 fx3 fx4 fx5))
       ((fx1 fx2 fx3 fx4 fx5 . rest)
        (and (<compare> fx1 fx2 fx3)
             (let loop ((fx1 fx4) (fx2 fx5) (rest rest))
               (and (<compare> fx1 fx2)
                    (or (null? rest) (apply loop fx2 rest))))))))))

(fxdefine-comparison fx=? =)
(fxdefine-comparison fx>? >)
(fxdefine-comparison fx<? <)
(fxdefine-comparison fx>=? >=)
(fxdefine-comparison fx<=? <=)

(define-syntax fxdefine-predicate
  (syntax-rules ()
    ((_ <name> <predicate>)
     (fxdefine <name>
       ((fx)
        (<predicate> fx))))))

(fxdefine-predicate fxzero? zero?)
(fxdefine-predicate fxpositive? positive?)
(fxdefine-predicate fxnegative? negative?)
(fxdefine-predicate fxodd? odd?)
(fxdefine-predicate fxeven? even?)

(define-syntax fxdefine-selection
  (syntax-rules ()
    ((_ <name> <selector>)
     (fxdefine <name>
       ((fx1)
        fx1)
       ((fx1 fx2)
        (<selector> fx1 fx2))
       ((fx1 fx2 fx3)
        (<selector> fx1 fx2 fx3))
       ((fx1 fx2 fx3 fx4)
        (<selector> fx1 fx2 fx3 fx4))
       ((fx1 fx2 fx3 fx4 fx5)
        (<selector> fx1 fx2 fx3 fx4 fx5))
       (all
        (apply <selector> all))))))

(fxdefine-selection fxmax max)
(fxdefine-selection fxmin min)

(fxdefine+ fx+ ((fx1 fx2) (+ fx1 fx2)))
(fxdefine+ fx* ((fx1 fx2) (* fx1 fx2)))
(fxdefine+ fx- ((fx1) (- fx1)) ((fx1 fx2) (- fx1 fx2)))

(define-syntax fxdefine-division
  (syntax-rules ()
    ((_ <name> <operator>)
     (fxdefine+ <name> ((fx1 fx2) (<operator> fx1 fx2))))))

;;; fxdiv-and-mod and fxdiv0-and-mod0 have to check both return values
;;; FIXME: this is less efficient than it should be

(define (fxdiv-and-mod x y)
  (values (fxdiv x y)
          (fxmod x y)))
(fxdefine-division fxdiv div)
(fxdefine-division fxmod mod)

(define (fxdiv0-and-mod0 x y)
  (values (fxdiv0 x y)
          (fxmod0 x y)))
(fxdefine-division fxdiv0 div0)
(fxdefine-division fxmod0 mod0)

(fxdefine fx+/carry
  ((fx1 fx2 fx3)
   (let*-values (((s) (+ fx1 fx2 fx3))
                 ((s1 s0) (div0-and-mod0 s (expt 2 W))))
     (values s0 s1))))

(fxdefine fx-/carry
  ((fx1 fx2 fx3)
   (let*-values (((d) (- fx1 fx2 fx3))
                 ((d1 d0) (div0-and-mod0 d (expt 2 W))))
     (values d0 d1))))

(fxdefine fx*/carry
  ((fx1 fx2 fx3)
   (let*-values (((s) (+ (* fx1 fx2) fx3))
                 ((s1 s0) (div0-and-mod0 s (expt 2 W))))
     (values s0 s1))))

(fxdefine fxnot ((fx1) (- 0 fx1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bit-level hacking.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fixnum-powers-of-two
  (let ((v (make-vector W 1)))
    (do ((i 1 (+ i 1)))
        ((= i W)
         v)
      (let ((n (vector-ref v (- i 1))))
        (vector-set! v i (+ n n))))))

(define (fixnum->bitvector fx)
  (let ((v (make-vector W)))
    (let ((sign-bit (negative? fx)))
      (vector-set! v (- W 1) sign-bit)
      (let loop ((fx (if sign-bit (- fx (least-fixnum)) fx))
                 (i (- W 2)))
        (let* ((one-hot (expt 2 i))
               (bit-set? (>= fx one-hot)))
          (vector-set! v i bit-set?)
          (unless (zero? i)
            (loop (if bit-set? (- fx one-hot) fx)
                  (- i 1))))))
    v))

(define (bitvector->fixnum v)
  (let loop ((result (if (vector-ref v (- W 1))
                         (least-fixnum)
                         0))
             (i (- W 2)))
    (let ((result (if (vector-ref v i)
                      (+ result (expt 2 i))
                      result)))
      (if (zero? i)
          result
          (loop result (- i 1))))))

;;; Given the <name> of a fixnum-only bitwise procedure to be defined,
;;; an identity for the operation performed by the procedure,
;;; and a procedure that takes two bits and returns their result,
;;; defines the procedure.

(define-syntax fxdefine-bitwise
  (syntax-rules ()
   ((_ <name> <identity> <operation>)
    (fxdefine <name>
      (()
       <identity>)
      ((fx1)
       fx1)
      ((fx1 fx2)
       (let loop ((fx1 (if (negative? fx1) (- fx1 (least-fixnum)) fx1))
                  (fx2 (if (negative? fx2) (- fx2 (least-fixnum)) fx2))
                  (i (- W 2))
                  (bits (let* ((bit1 (if (negative? fx1) 1 0))
                               (bit2 (if (negative? fx2) 1 0))
                               (bit (<operation> bit1 bit2)))
                          (if (= 0 bit)
                              0
                              (vector-ref fixnum-powers-of-two (- W 1))))))
         (if (< i 0)
             bits
             (let* ((n (vector-ref fixnum-powers-of-two i))
                    (bit1? (>= fx1 n))
                    (bit2? (>= fx2 n))
                    (bit (<operation> (if bit1? 1 0)
                                      (if bit2? 1 0)))
                    (fx1 (if bit1? (- fx1 n) fx1))
                    (fx2 (if bit2? (- fx2 n) fx2))
                    (bits (if (= 0 bit)
                              bits
                              (+ bits n))))
               (loop fx1 fx2 (- i 1) bits)))))
      ((fx1 fx2 . rest)
       (let loop ((result (<name> fx1 fx2))
                  (rest rest))
         (if (null? rest)
             result
             (loop (<name> result (car rest))
                   (cdr rest)))))))))       

(fxdefine-bitwise fxand
  -1
  (lambda (b1 b2)
    (cond ((= 0 b1) 0)
          ((= 0 b2) 0)
          (else 1))))

(fxdefine-bitwise fxior
  0
  (lambda (b1 b2)
    (cond ((= 1 b1) 1)
          ((= 1 b2) 1)
          (else 0))))

(fxdefine-bitwise fxxor
  0
  (lambda (b1 b2)
    (cond ((= 1 (+ b1 b2)) 1)
          (else 0))))

(fxdefine fxif
  ((fx1 fx2 fx3)
   (cond ((negative? fx1)
          (fxif (fxnot fx1) fx3 fx2))
         ((or (negative? fx2)
              (negative? fx3))
          (fxior (fxand fx1 fx2)
                 (fxand (fxnot fx1) fx3)))
         (else
          (let loop ((fx1 fx1)
                     (fx2 fx2)
                     (fx3 fx3)
                     (i (- W 1))
                     (bits 0))
            (if (< i 0)
                bits
                (let* ((n (vector-ref fixnum-powers-of-two i))
                       (bit1? (>= fx1 n))
                       (bit2? (>= fx2 n))
                       (bit3? (>= fx3 n))
                       (fx1 (if bit1? (- fx1 n) fx1))
                       (fx2 (if bit2? (- fx2 n) fx2))
                       (fx3 (if bit3? (- fx3 n) fx3))
                       (bits (if bit1?
                                 (if bit2? (+ bits n) bits)
                                 (if bit3? (+ bits n) bits))))
                  (loop fx1 fx2 fx3 (- i 1) bits))))))))

(fxdefine fxbit-count
  ((fx)
   (if (< fx 0)
       (fxnot (fxbit-count (fxnot fx)))
       (let loop ((fx fx)
                  (i (- W 1))
                  (result 0))
         (if (= fx 0)
             result
             (let ((n (vector-ref fixnum-powers-of-two i)))
               (if (>= fx n)
                   (loop (- fx n)
                         (- i 1)
                         (+ result 1))
                   (loop fx (- i 1) result))))))))

(fxdefine fxlength
  ((fx)
   (let ((fx (if (negative? fx) (fxnot fx) fx)))
     (let loop ((i 0)
                (n 1))
       (if (< fx n)
           i
           (loop (+ i 1) (+ n n)))))))

(fxdefine fxfirst-bit-set
  ((fx)
   (if (zero? fx)
       -1
       (let ((fx (if (negative? fx) (- fx) fx)))
         (let loop ((fx (if (negative? fx) (- fx) fx))
                    (i (- W 1)))
           (let ((n (vector-ref fixnum-powers-of-two i)))
             (cond ((= fx n)
                    i)
                   ((> fx n)
                    (loop (- fx n)
                          (- i 1)))
                   (else
                    (loop fx
                          (- i 1))))))))))

(define-syntax assert-index
  (syntax-rules ()
    ((_ <fx>)
     (assert (and (not (negative? <fx>)) (< <fx> W))
             "index must be non-negative and less than (fixnum-width)"
             <fx>))))

(fxdefine fxbit-set?
  ((fx1 fx2)
   (assert-index fx2)
   (if (>= fx2 W)
       (negative? fx1)
       (not (fxzero?
             (fxand fx1
                    (expt 2 fx2)))))))

(fxdefine fxcopy-bit
  ((fx1 fx2 fx3)
   (assert-index fx2)
   (assert (or (zero? fx3) (= 1 fx3)) "third argument must be 1 or 0" fx3)
   (let* ((mask (expt 2 fx2))
          (bit  (if (= fx3 0) 0 mask)))
     (fxif mask
           bit
           fx1))))

(define-syntax assert-indices
  (syntax-rules ()
    ((_ fx1 fx2)
     (begin
      (assert-index fx1)
      (assert-index fx2)
      (assert (<= fx1 fx2)
              "first index must be less than or equal to second" fx1 fx2)))))

(fxdefine fxbit-field
  ((fx1 fx2 fx3)
   (assert-indices fx2 fx3)
   (let* ((mask (fxnot
                 (- (expt 2 fx3)))))
     (fxarithmetic-shift-right (fxand fx1 mask)
                               fx2))))

(fxdefine fxcopy-bit-field
  ((fx1 fx2 fx3 fx4)
   (assert-indices fx2 fx3)
   (let* ((to    fx1)
          (start fx2)
          (end   fx3)
          (from  fx4)
          (mask1 (- (expt 2 start)))
          (mask2 (fxnot
                  (- (expt 2 end))))
          (mask (fxand mask1 mask2)))
     (fxif mask
           (fxarithmetic-shift-left from start)
           to))))

(define-syntax assert-shift-count
  (syntax-rules ()
    ((_ <fx>)
     (assert
      (< (abs <fx>) W)
      "shift count's absolute value must be less than (fixnum-width)" <fx>))))

(fxdefine+ fxarithmetic-shift
  ((fx1 fx2)
   (assert-shift-count fx2)
   (floor (* fx1 (expt 2 fx2)))))

(fxdefine+ fxarithmetic-shift-left
  ((fx1 fx2)
   (assert-index fx2)
   (floor (* fx1 (expt 2 fx2)))))

(fxdefine+ fxarithmetic-shift-right
  ((fx1 fx2)
   (assert-index fx2)
   (floor (* fx1 (expt 2 (- fx2))))))

(fxdefine fxrotate-bit-field
  ((fx1 fx2 fx3 fx4)
   (assert-indices fx2 fx3)
   (let ((field-length (- fx3 fx2)))
     (assert (< fx4 field-length)
             "shift count must be less than the field width" fx2 fx3 fx4)
     (let* ((n     fx1)
            (start fx2)
            (end   fx3)
            (count fx4)
            (width (fx- end start)))
       (if (fxpositive? width)
           (let* ((count (fxmod count width))
                  (field0 (fxbit-field n start end))
                  (field1 (fxarithmetic-shift-left field0 count))
                  (field2 (fxarithmetic-shift-right field0 (fx- width count)))
                  (field (fxior field1 field2)))
             (fxcopy-bit-field n start end field))
           n)))))

(fxdefine fxreverse-bit-field
  ((fx1 fx2 fx3)
   (assert-indices fx2 fx3)
   (assert (<= fx2 fx3)
           "bit field widths must be non-negative" fx2 fx3)
   (let* ((field (fxbit-field fx1 fx2 fx3))
          (field (fxreverse-fixnum field (- fx3 fx2))))
     (fxcopy-bit-field fx1 fx2 fx3 field))))

;;; Private to this file.

(define (fxreverse-fixnum fx k)
  (if (negative? fx)
      (+ 1 (fxreverse-fixnum (- fx (least-fixnum))))
      (let loop ((fx fx)
                 (i (- k 1))
                 (bits 0)
                 (val 1))
        (if (< i 0)
            bits
            (let* ((n (vector-ref fixnum-powers-of-two i))
                   (bit1? (>= fx n))
                   (fx (if bit1? (- fx n) fx))
                   (bits (if bit1? (+ bits val) bits)))
              (loop fx (- i 1) bits (+ val val)))))))
