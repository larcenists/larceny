; Written by William D Clinger for use in Larceny.
;
; $Id$
;
;;; Homogeneous numeric vectors.
;;;
;;; Basic support for SRFI 4 and SRFI 160.
;;; This file defines the representations
;;; and enough hooks so the read procedure
;;; can implement the external syntax.

($$trace "numvec")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Representations.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Here's a representation I decided not to use, but it has
;;; enough advantages that I don't want to flush these comments.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Homogeneous vectors are bytevector-like.
;;;
;;; SRFI 160 requires #u8 vectors to be the same as R6RS/R7RS
;;; bytevectors.
;;;
;;; All other homogeneous numeric vectors are specially tagged
;;; bytevectors.
;;;
;;; The 11 distinct varieties (aside from #u8) are distinguished
;;; by a numeric tag in the first byte.
;;;
;;; That first byte is followed by enough zero bytes to allow
;;; all of the numeric vector's elements to be properly aligned.
;;; 
;;; The following table defines a mapping between the tags used
;;; for external representations and the numeric tags in byte 0,
;;; along with the width of each element.  The number of zero
;;; bytes that follow byte zero is always one less than the width
;;; of each element.
;;;
;;; For now, the width of each element is the same as the subtag
;;; mod 32.  That might make it easier to define procedures that
;;; don't need to know anything beyond the element width and the
;;; number of zero bytes that follow the subtag.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End of comments about a representation I decided not to use.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The representation I decided to use is a slight variation
;;; of the representation used by sample implementations of
;;; SRFI 160.  The main advantage of this representation is
;;; that it can use standard R6RS operations on bytevectors.
;;;
;;; That will lead to some redundant checking, but it's the
;;; fastest way to get this going.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Numeric vectors are represented as records with two fields:
;;;
;;;     hashcode (a 28-bit positive integer)
;;;     bv (a standard bytevector)
;;;
;;; The five low-order bits of the hashcode are the element width
;;; in bytes: 1, 2, 4, 8, or 16
;;;
;;; The eight low-order bits of the hashcode encode the particular
;;; type of numeric vector:
;;;
;;;     s8, u16, s16, u32, s32, u64, s64, f32, f64, c64, or c128
;;;
;;; The 20 high-order bits of the 28-bit hashcode are generated
;;; at random when the numeric vector is created, for use when
;;; hashing on object identity.

;;; The number of bits in the hashcode is not at all critical,
;;; but 28 bits is safely within the range of a fixnum even with
;;; 32-bit Larceny.

(define (larceny:numvec-hashcode->element-width hashcode)
  (fxlogand hashcode #x1f))

(define (larceny:numvec-hashcode->int-tag hashcode)
  (fxlogand hashcode #xff))

(define (larceny:numvec-hashcode->symbol-tag hashcode)
  (cadr (assq (larceny:numvec-hashcode->int-tag
               (fxlogand hashcode #xff))
              numvec-subtag-table)))

(define (larceny:numvec-symtag->int-tag symtag)
  (cadr (assq symtag numvec-subtag-table-reversed)))

(define (larceny:numvec-symtag->width symtag)
  (fxlogand (larceny:numvec-symtag->int-tag symtag)
            #x1f))

(define numvec-subtag-table
  '((33    s8)   ;  1
    ( 2   u16)   ;  2
    (34   s16)   ;  2
    ( 4   u32)   ;  4
    (36   s32)   ;  4
    ( 8   u64)   ;  8
    (40   s64)   ;  8
    (68   f32)   ;  4
    (72   f64)   ;  8
    (104  c64)   ;  8
    (112 c128))) ; 16

(define numvec-subtag-table-reversed
  (map reverse numvec-subtag-table))

#;
(define-record-type larceny:numvec-rtd
  (new-numvec hashcode bv)
  numeric-vector?
  (hashcode larceny:numvec.hashcode)
  (bv       larceny:numvec.bv))

(define larceny:numvec-rtd
  (make-rtd ':numvec
            '#((immutable hashcode)
               (immutable bv))))

(define new-numvec              (rtd-constructor larceny:numvec-rtd))
(define numeric-vector?         (rtd-predicate   larceny:numvec-rtd))
(define larceny:numvec.hashcode (rtd-accessor    larceny:numvec-rtd 'hashcode))
(define larceny:numvec.bv       (rtd-accessor    larceny:numvec-rtd 'bv))

(define (larceny:numvec-write numvec p)
  (let* ((hashcode (larceny:numvec.hashcode numvec))
         (sym (larceny:numvec-hashcode->symbol-tag hashcode)))
    (write-char #\# p)
    (write sym p)
    (write (larceny:numvec->list numvec) p)))

(rtd-printer-set! larceny:numvec-rtd larceny:numvec-write)

(define (larceny:numvec->list numvec . rest)
  (let* ((hashcode (larceny:numvec.hashcode numvec))
         (sym (larceny:numvec-hashcode->symbol-tag hashcode))
         (width (larceny:numvec-hashcode->element-width hashcode))
         (getter (larceny:numvec-getter sym))
         (bv (larceny:numvec.bv numvec))
         (n (bytevector-length bv)))

    ;; lo and i are bytevector indexes
    
    (define (loop lo i vals)
      (if (< i lo)
          vals
          (loop lo
                (- i width)
                (cons (getter bv i) vals))))

    (if (null? rest)
        (loop 0 (- n width) '())        ; fast path
        
        ;; Note that start and end are element indexes,
        ;; but the loop works with bytevector indexes.
        ;; FIXME: That's because this code started out
        ;; without any rest argument, to be used only
        ;; for writing numeric vectors, but it has been
        ;; generalized for use by SRFI 160.

        (let* ((k (quotient n width))
               (start (if (null? rest) 0 (car rest)))
               (end (if (or (null? rest) (null? (cdr rest)))
                        k
                        (cadr rest))))
          (if (and (fixnum? start)
                   (fixnum? end)
                   (<= 0 start end k))
              (loop (* width start)
                    (- (* width end) width)
                    '())
              (error "arguments out of range: " numvec start end))))))

;;; The getter and setter procedures for a numeric vector.

(define (larceny:numvec-getter symtag)
  (case symtag
    ((s8)   bytevector-s8-ref)
    ((u16)  bytevector-u16-native-ref)
    ((s16)  bytevector-s16-native-ref)
    ((u32)  bytevector-u32-native-ref)
    ((s32)  bytevector-s32-native-ref)
    ((u64)  bytevector-u64-native-ref)
    ((s64)  bytevector-s64-native-ref)
    ((f32)  bytevector-ieee-single-native-ref)
    ((f64)  bytevector-ieee-double-native-ref)
    ((c64)  bytevector-c64-native-ref)
    ((c128) bytevector-c128-native-ref)
    (else
     (error "larceny:numvec-getter " sym))))

(define (larceny:numvec-setter symtag)
  (case symtag
    ((s8)   bytevector-s8-set!)
    ((u16)  bytevector-u16-native-set!)
    ((s16)  bytevector-s16-native-set!)
    ((u32)  bytevector-u32-native-set!)
    ((s32)  bytevector-s32-native-set!)
    ((u64)  bytevector-u64-native-set!)
    ((s64)  bytevector-s64-native-set!)
    ((f32)  bytevector-ieee-single-native-set!)
    ((f64)  bytevector-ieee-double-native-set!)
    ((c64)  bytevector-c64-native-set!)
    ((c128) bytevector-c128-native-set!)
    (else
     (error "larceny:numvec-setter " sym))))

(define (larceny:numvec-predicate symtag)
  (case symtag
    ((s8)   s8?)
    ((u16)  u16?)
    ((s16)  s16?)
    ((u32)  u32?)
    ((s32)  s32?)
    ((u64)  u64?)
    ((s64)  s64?)
    ((f32)  flonum?)
    ((f64)  flonum?)
    ((c64)  c64?)
    ((c128) c128?)
    (else
     (error "larceny:numvec-predicate " sym))))

(define (bytevector-c64-native-ref bv k)
  (let* ((x (bytevector-ieee-single-native-ref bv k))
         (y (bytevector-ieee-single-native-ref bv (+ k 4))))
    (make-compnum x y)))

(define (bytevector-c64-native-set! bv k z)
  (let* ((x (real-part z))
         (y (imag-part z)))
    (bytevector-ieee-single-native-set! bv k x)
    (bytevector-ieee-single-native-set! bv (+ k 4) y)))

(define (bytevector-c128-native-ref bv k)
  (let* ((x (bytevector-ieee-double-native-ref bv k))
         (y (bytevector-ieee-double-native-ref bv (+ k 8))))
    (make-compnum x y)))

(define (bytevector-c128-native-set! bv k z)
  (let* ((x (real-part z))
         (y (imag-part z)))
    (bytevector-ieee-double-native-set! bv k x)
    (bytevector-ieee-double-native-set! bv (+ k 8) y)))

(define (larceny:init-numvec bv width ok? vals setter!)
  (define (loop i vals)
    (cond ((null? vals)
           bv)
          ((ok? (car vals))
           (setter! bv i (car vals))
           (loop (+ i width) (cdr vals)))
          (else
           (error "illegal numeric vector element: " (car vals)))))
  (loop 0 vals))

;;; Everything below this is exported to (srfi 160 @).

;;; Hook for the read procedure.
;;;
;;; Given a symbol (s8, u16, s16, u32, s32, u64, s64, f32, f64, c64, or c128)
;;; and a list of values, return a homogeneous numeric vector of those values
;;; provided they are legal.

(define (make-numvec symtag vals)
  (let* ((width (larceny:numvec-symtag->width symtag))
         (bv (make-bytevector (* width (length vals)) 0)))
    (larceny:init-numvec bv
                         width
                         (larceny:numvec-predicate symtag)
                         vals
                         (larceny:numvec-setter symtag))
    (new-numvec (+ (fxlsh (random 1000000) 8)
                   (larceny:numvec-symtag->int-tag symtag))
                bv)))

(define (u8? n)
  (and (fixnum? n)
       (<= 0 n 255)))

(define (s8? n)
  (and (fixnum? n)
       (<= -128 n 127)))

(define (u16? n)
  (and (fixnum? n)
       (<= 0 n #xffff)))

(define (s16? n)
  (and (fixnum? n)
       (<= #x-8000 n #x7fff)))

;;; These procedures try to avoid comparing a fixnum to a bignum.

(define (u32? n)
  (and (integer? n)
       (exact? n)
       (<= 0 n)
       (or (<= n (greatest-fixnum))   ; FIXME: assumes 30-bit fixnums
           (<= n #xffffffff))))

(define (s32? n)
  (and (integer? n)
       (exact? n)
       (or (<= (least-fixnum) n)
           (<= #x-80000000 n))
       (or (<= n (greatest-fixnum))   ; FIXME: assumes 30-bit fixnums
           (<= n #x7fffffff))))

(define (u64? n)
  (and (integer? n)
       (exact? n)
       (<= 0 n)
       (or (<= n (greatest-fixnum))   ; FIXME: assumes fixnums <= 64 bits
           (<= n #xffffffffffffffff))))

(define (s64? n)
  (and (integer? n)
       (exact? n)
       (or (<= (least-fixnum) n)
           (<= #x-8000000000000000 n))
       (or (<= n (greatest-fixnum))   ; FIXME: assumes fixnums <= 64 bits
           (<= n #x7fffffffffffffff))))

(define (f32? x)
  (flonum? x))

(define (f64? x)
  (flonum? x))

(define (c64? z)
  (and (complex? z)
       (inexact? z)))

(define (c128? z)
  (and (complex? z)
       (inexact? z)))
