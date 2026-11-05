;;;; Implementation of SRFI 4 on top of (rnrs bytevectors)
;;;; and Larceny's low-level support for numeric vectors.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Larceny's low-level support.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define max-random (expt 2 20)) ; 20 bits of hash code
(define int-tag-size 8)         ; 8 bits of an integer type subtag
(define int-tag-mask (- (expt 2 int-tag-size) 1))
(define width-mask #x1f)

(define (fill! bv val width size setter!)
  (define (loop i)
    (if (= i size)
        bv
        (begin (setter! bv (* i width) val)
               (loop (+ i 1)))))
  (loop 0))

(define (element-error symtag val)
  (error "illegal values cannot be stored into numeric vector: " symtag val))

(define (vector-error symtag val)
  (error "not a numeric vector of the required subtype: " symtag val))

(define (index-error v i)
  (error "index out of range: " v i)) ; FIXME: should use standard range error

(define (larceny-maker symtag)
  (let* ((int-tag (larceny:numvec-symtag->int-tag symtag))
         (width (fxlogand int-tag width-mask))
         (ok? (larceny:numvec-predicate symtag))
         (setter! (larceny:numvec-setter symtag)))
    (lambda (size . maybe-value)
      (let* ((hashcode (+ int-tag (fxlsh (random max-random) int-tag-size)))
             (bv (make-bytevector (* width size) 0)))
        (if (not (null? maybe-value))
            (let ((val (car maybe-value)))
              (if (ok? val)
                  (fill! bv (car maybe-value) width size setter!)
                  (element-error symtag val))))
        (new-numvec hashcode bv)))))

(define (larceny-maker-from-list symtag)
  (lambda args
    (make-numvec symtag args)))

(define (larceny-specific-vector-predicate symtag)
  (let ((int-tag (larceny:numvec-symtag->int-tag symtag)))
    (lambda (obj)
      (and (numeric-vector? obj)
           (eq? int-tag
                (fxlogand int-tag-mask (larceny:numvec.hashcode obj)))))))

(define (larceny-specific-vector-length symtag)
  (let* ((int-tag (larceny:numvec-symtag->int-tag symtag))
         (width (larceny:numvec-symtag->width symtag)))
    (lambda (obj)
      (if (and (numeric-vector? obj)
               (eq? int-tag
                    (fxlogand int-tag-mask (larceny:numvec.hashcode obj))))
          (/ (bytevector-length (larceny:numvec.bv obj)) width)
          (vector-error symtag obj)))))

(define (larceny-specific-vector-ref symtag)
  (let* ((int-tag (larceny:numvec-symtag->int-tag symtag))
         (width (larceny:numvec-symtag->width symtag))
         (getter (larceny:numvec-getter symtag)))
    (lambda (v i)
      (if (and (numeric-vector? v)
               (eq? int-tag
                    (fxlogand int-tag-mask (larceny:numvec.hashcode v))))
          (let* ((bv (larceny:numvec.bv v))
                 (n (- (bytevector-length bv) 1))
                 (j (* width i)))
            (if (<= 0 j n)
                (getter bv j)
                (index-error v i)))
          (vector-error symtag v)))))

(define (larceny-specific-vector-set! symtag)
  (let* ((int-tag (larceny:numvec-symtag->int-tag symtag))
         (width (larceny:numvec-symtag->width symtag))
         (setter! (larceny:numvec-setter symtag))
         (ok? (larceny:numvec-predicate symtag)))
    (lambda (v i x)
      (if (and (numeric-vector? v)
               (eq? int-tag
                    (fxlogand int-tag-mask (larceny:numvec.hashcode v)))
               (ok? x))
          (let* ((bv (larceny:numvec.bv v))
                 (n (- (bytevector-length bv) 1))
                 (j (* width i)))
            (if (<= 0 j n)
                (setter! bv j x)
                (index-error v i)))
          (if (ok? x)
              (vector-error symtag v)
              (element-error symtag x))))))

(define (larceny-specific->list symtag)
  (let* ((int-tag (larceny:numvec-symtag->int-tag symtag)))
    (lambda (v . rest)
      (if (and (numeric-vector? v)
               (eq? int-tag
                    (fxlogand int-tag-mask (larceny:numvec.hashcode v))))
          (apply larceny:numvec->list v rest)
          (vector-error symtag v)))))

;;; Main constructor

(define (make-u8vector n . rest) ; make-bytevector)
  (if (null? rest)
      (make-bytevector n)
      (let ((fill (car rest)))
        (if (and (fixnum? fill)
                 (<= 0 fill 255))
            (make-bytevector n fill)
            (element-error 'u8 fill)))))

(define make-s8vector   (larceny-maker 's8))
(define make-u16vector  (larceny-maker 'u16))
(define make-s16vector  (larceny-maker 's16))
(define make-u32vector  (larceny-maker 'u32))
(define make-s32vector  (larceny-maker 's32))
(define make-u64vector  (larceny-maker 'u64))
(define make-s64vector  (larceny-maker 's64))
(define make-f32vector  (larceny-maker 'f32))
(define make-f64vector  (larceny-maker 'f64))
(define make-c64vector  (larceny-maker 'c64))    ; not part of (srfi 4)
(define make-c128vector (larceny-maker 'c128))   ; not part of (srfi 4)

;;; Variable-argument constructor

(define u8vector bytevector)

(define s8vector   (larceny-maker-from-list 's8))
(define u16vector  (larceny-maker-from-list 'u16))
(define s16vector  (larceny-maker-from-list 's16))
(define u32vector  (larceny-maker-from-list 'u32))
(define s32vector  (larceny-maker-from-list 's32))
(define u64vector  (larceny-maker-from-list 'u64))
(define s64vector  (larceny-maker-from-list 's64))
(define f32vector  (larceny-maker-from-list 'f32))
(define f64vector  (larceny-maker-from-list 'f64))
(define c64vector  (larceny-maker-from-list 'c64))    ; not part of (srfi 4)
(define c128vector (larceny-maker-from-list 'c128))   ; not part of (srfi 4)

;; Predicate

(define u8vector? bytevector?)

(define s8vector?   (larceny-specific-vector-predicate 's8))
(define u16vector?  (larceny-specific-vector-predicate 'u16))
(define s16vector?  (larceny-specific-vector-predicate 's16))
(define u32vector?  (larceny-specific-vector-predicate 'u32))
(define s32vector?  (larceny-specific-vector-predicate 's32))
(define u64vector?  (larceny-specific-vector-predicate 'u64))
(define s64vector?  (larceny-specific-vector-predicate 's64))
(define f32vector?  (larceny-specific-vector-predicate 'f32))
(define f64vector?  (larceny-specific-vector-predicate 'f64))
(define c64vector?  (larceny-specific-vector-predicate 'c64))  ; not (srfi 4)
(define c128vector? (larceny-specific-vector-predicate 'c128)) ; not (srfi 4)

;; Length

(define u8vector-length bytevector-length)

(define s8vector-length   (larceny-specific-vector-length 's8))
(define u16vector-length  (larceny-specific-vector-length 'u16))
(define s16vector-length  (larceny-specific-vector-length 's16))
(define u32vector-length  (larceny-specific-vector-length 'u32))
(define s32vector-length  (larceny-specific-vector-length 's32))
(define u64vector-length  (larceny-specific-vector-length 'u64))
(define s64vector-length  (larceny-specific-vector-length 's64))
(define f32vector-length  (larceny-specific-vector-length 'f32))
(define f64vector-length  (larceny-specific-vector-length 'f64))
(define c64vector-length  (larceny-specific-vector-length 'c64)) ; not (srfi 4)
(define c128vector-length (larceny-specific-vector-length 'c128)); not (srfi 4)

;; Get element

(define u8vector-ref bytevector-u8-ref)

(define s8vector-ref   (larceny-specific-vector-ref 's8))
(define u16vector-ref  (larceny-specific-vector-ref 'u16))
(define s16vector-ref  (larceny-specific-vector-ref 's16))
(define u32vector-ref  (larceny-specific-vector-ref 'u32))
(define s32vector-ref  (larceny-specific-vector-ref 's32))
(define u64vector-ref  (larceny-specific-vector-ref 'u64))
(define s64vector-ref  (larceny-specific-vector-ref 's64))
(define f32vector-ref  (larceny-specific-vector-ref 'f32))
(define f64vector-ref  (larceny-specific-vector-ref 'f64))
(define c64vector-ref  (larceny-specific-vector-ref 'c64)) ; not (srfi 4)
(define c128vector-ref (larceny-specific-vector-ref 'c128)); not (srfi 4)

;; Set element

(define u8vector-set! bytevector-u8-set!)

(define s8vector-set!   (larceny-specific-vector-set! 's8))
(define u16vector-set!  (larceny-specific-vector-set! 'u16))
(define s16vector-set!  (larceny-specific-vector-set! 's16))
(define u32vector-set!  (larceny-specific-vector-set! 'u32))
(define s32vector-set!  (larceny-specific-vector-set! 's32))
(define u64vector-set!  (larceny-specific-vector-set! 'u64))
(define s64vector-set!  (larceny-specific-vector-set! 's64))
(define f32vector-set!  (larceny-specific-vector-set! 'f32))
(define f64vector-set!  (larceny-specific-vector-set! 'f64))
(define c64vector-set!  (larceny-specific-vector-set! 'c64))  ; not (srfi 4)
(define c128vector-set! (larceny-specific-vector-set! 'c128)) ; not (srfi 4)

;; Vec to list

(define (u8vector->list vec)
  (if (u8vector? vec)
      (bytevector->list vec)
      (vector-error 'u8 vec)))

(define s8vector->list   (larceny-specific->list 's8))
(define u16vector->list  (larceny-specific->list 'u16))
(define s16vector->list  (larceny-specific->list 's16))
(define u32vector->list  (larceny-specific->list 'u32))
(define s32vector->list  (larceny-specific->list 's32))
(define u64vector->list  (larceny-specific->list 'u64))
(define s64vector->list  (larceny-specific->list 's64))
(define f32vector->list  (larceny-specific->list 'f32))
(define f64vector->list  (larceny-specific->list 'f64))
(define c64vector->list  (larceny-specific->list 'c64))    ; not (srfi 4)
(define c128vector->list (larceny-specific->list 'c128))   ; not (srfi 4)

;; List to vec

(define list->u8vector list->bytevector)

(define (list->s8vector list)
  (make-numvec 's8 list))

(define (list->u16vector list)
  (make-numvec 'u16 list))

(define (list->s16vector list)
  (make-numvec 's16 list))

(define (list->u32vector list)
  (make-numvec 'u32 list))

(define (list->s32vector list)
  (make-numvec 's32 list))

(define (list->u64vector list)
  (make-numvec 'u64 list))

(define (list->s64vector list)
  (make-numvec 's64 list))

(define (list->f32vector list)
  (make-numvec 'f32 list))

(define (list->f64vector list)
  (make-numvec 'f64 list))

(define (list->c64vector list)    ; not part of (srfi 4)
  (make-numvec 'c64 list))

(define (list->c128vector list)   ; not part of (srfi 4)
  (make-numvec 'c128 list))
