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
       (apply error <message> . <objs>)))))

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

;;; Basically (x1 x2 ...) -> (g (f x1) (f x2) ...), but resorting to run-time
;;; 'apply' and 'map' for the last position when the input list is improper,
;;; i.e. it contains a rest-argument parameter.  'f' may be a macro; 'g' not.
(define-syntax map/apply-args
  (syntax-rules ()
    ((_ <combiner> <operator> <args>)
     (map/apply-args <combiner> <operator> <args> ()))
    ((_ <combiner> <operator> (<first> . <rest>) (<result> ...))
     (map/apply-args
      <combiner> <operator> <rest> (<result> ... (<operator> <first>))))
    ((_ <combiner> <operator> () (<result> ...))
     (<combiner> <result> ...))
    ((_ <combiner> <operator> <rest> (<result> ...))
     (apply
      <combiner> <result> ... (map (lambda (arg) (<operator> arg)) <rest>)))))

;;; 24 is the minimum width an implementation must support; we choose it to make
;;; sure that our fixnum operations really work with unboxed numbers in as many
;;; Scheme implementations as possible.
(define W 24)

(define (fixnum? obj)
  (and (exact-integer? obj)
       (<= (least-fixnum) obj (greatest-fixnum))))

(define (fixnum-width) W)
(define (least-fixnum) (expt -2 (- W 1)))
(define (greatest-fixnum) (- (expt 2 (- W 1)) 1))

(define-syntax assert-fixnum-arg
  (syntax-rules ()
    ((_ <arg>)
     (assert (fixnum? <arg>) "Argument is not a fixnum." <arg>))))

(define-syntax assert-fixnum-result
  (syntax-rules ()
    ((_ <result>)
     (assert (fixnum? <result>) "Result is not a fixnum." <result>))))

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
     (fxdefine <name> ((fx1 fx2) (<operator> fx1 fx2))))))

(fxdefine-division fxdiv-and-mod div-and-mod)
(fxdefine-division fxdiv div)
(fxdefine-division fxmod mod)
(fxdefine-division fxdiv0-and-mod0 div-and-mod)
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

(define-syntax fxdefine-logic
  (syntax-rules ()
    ((_ <name> <operator> <args> ...)
     (fxdefine <name>
       (<args>
        (let ((v (make-vector W)))
          (do ((i 0 (+ i 1)))
              ((= i W) (bitvector->fixnum v))
            (map/apply-args
             (lambda <args>
               (vector-set! v i (map/apply-args
                                 <operator>
                                 (lambda (arg) (vector-ref arg i))
                                 <args>)))
             fixnum->bitvector
             <args>))))
       ...))))

(define-syntax define-logic-proc
  (syntax-rules ()
    ((_ <name> <operator>)
     (define-logic-proc <name> <operator>
       (() (a) (a b) (a b c) (a b c d) (a b c d e))))
    ((_ <name> <operator> ((<arg> ...) ...))
     (define <name>
       (case-lambda
         ((<arg> ...)
          (<operator> <arg> ...))
         ...
         (all
          (let loop ((result (<operator>))
                     (rest all))
            (if (null? rest)
                result
                (loop (<operator> result (car rest))
                      (cdr rest))))))))))

;;; Doesn't need to be a macro, but we want to leverage `define-logic-proc'.
(define-syntax xor
  (syntax-rules ()
    ((_) #f)
    ((_ <expr>) <expr>)
    ((_ <expr0> <expr1> . <exprs>)
     (xor (not (boolean=? (not (not <expr0>)) (not (not <expr1>))))
          . <exprs>))))

(define-logic-proc or-proc or)
(define-logic-proc and-proc and)
(define-logic-proc xor-proc xor)

(fxdefine-logic fxand and-proc () (a) (a b) (a b c) (a b c d) (a b c d e) rest)
(fxdefine-logic fxior  or-proc () (a) (a b) (a b c) (a b c d) (a b c d e) rest)
(fxdefine-logic fxxor xor-proc () (a) (a b) (a b c) (a b c d) (a b c d e) rest)

(fxdefine-logic fxif if (a b c))

(fxdefine fxbit-count
  ((fx)
   (let ((v (fixnum->bitvector fx)))
     (do ((i 0 (+ i 1))
          (count 0 (if (vector-ref v i) (+ count 1) count)))
         ((= i W) count)))))

(fxdefine fxlength
  ((fx)
   (let ((fx (if (negative? fx) (fxnot fx) fx)))
     (if (zero? fx)
         0
         (let ((v (fixnum->bitvector fx)))
           (do ((i (- W 2) (- i 1)))
               ((or (vector-ref v i) (= i 0))
                (+ i 1))))))))

(fxdefine fxfirst-bit-set
  ((fx)
   (if (zero? fx)
       -1
       (let ((v (fixnum->bitvector fx)))
         (do ((i 0 (+ i 1)))
             ((vector-ref v i) i))))))

(define-syntax assert-index
  (syntax-rules ()
    ((_ <fx>)
     (assert (and (not (negative? <fx>)) (< <fx> W))
             "Index must be non-negative and less than (fixnum-width)." <fx>))))

(fxdefine fxbit-set?
  ((fx1 fx2)
   (assert-index fx2)
   (vector-ref (fixnum->bitvector fx1) fx2)))

(fxdefine fxcopy-bit
  ((fx1 fx2 fx3)
   (assert-index fx2)
   (assert (or (zero? fx3) (= 1 fx3)) "Third argument must be 1 or 0." fx3)
   (let ((v (fixnum->bitvector fx1)))
     (vector-set! v fx2 (not (zero? fx3)))
     (bitvector->fixnum v))))

(define-syntax assert-indices
  (syntax-rules ()
    ((_ fx1 fx2)
     (assert-index fx1)
     (assert-index fx2)
     (assert (<= fx1 fx2)
             "First index must be less than or equal to second." fx1 fx2))))

(fxdefine fxbit-field
  ((fx1 fx2 fx3)
   (assert-indices fx2 fx3)
   (let ((v1 (fixnum->bitvector fx1))
         (v2 (make-vector W #f)))
     (do ((i 0 (+ i 1))
          (j fx2 (+ j 1)))
         ((= j fx3) (bitvector->fixnum v2))
       (vector-set! v2 i (vector-ref v1 j))))))

(fxdefine fxcopy-bit-field
  ((fx1 fx2 fx3 fx4)
   (assert-indices fx2 fx3)
   (let ((v1 (fixnum->bitvector fx1))
         (v2 (fixnum->bitvector fx4)))
     (do ((i fx2 (+ i 1)))
         ((= i fx3) (bitvector->fixnum v1))
       (vector-set! v1 i (vector-ref v2 i))))))

(define-syntax assert-shift-count
  (syntax-rules ()
    ((_ <fx>)
     (assert
      (< (abs <fx>) W)
      "Shift count's absolute value must be less than (fixnum-width)." <fx>))))

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
     (assert (< fx4 field-length) "Shift count must be less than the length of
the field specified by the indices." fx2 fx3 fx4)
     (let* ((v1 (fixnum->bitvector fx1))
            (v2 (vector-copy v1)))
       (do ((i 0 (+ i 1)))
           ((= i field-length) (bitvector->fixnum v2))
         (vector-set! v2 (+ fx2 (mod (+ i fx4) field-length))
                      (vector-ref v1 (+ fx2 i))))))))

(fxdefine fxreverse-bit-field
  ((fx1 fx2 fx3)
   (assert-indices fx2 fx3)
   (let ((v (fixnum->bitvector fx1)))
     (do ((i fx2 (+ i 1))
          (j (- fx3 1) (- j 1)))
         ((or (= i j) (= (+ i 1) j)) (bitvector->fixnum v))
       (let ((i-value (vector-ref v i))
             (j-value (vector-ref v j)))
         (vector-set! v i j-value)
         (vector-set! v j i-value))))))
