; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Scheme code for various contagion procedures and numeric coercion.
;
; There are three main procedures: contagion, pcontagion, and econtagion.
;
; Contagion is called to do mixed-representation arithmetic.
; Econtagion is called to do mixed-representation ``=''.
; Pcontagion is called to do mixed-representation ordering predicates.
;
; Peformance.
; Aggressive inlining can be a win here. However, calling [ep]contagion is
; fairly expensive in the first place, so it may not matter much in the
; overall scheme of things.

($$trace "contag")

(define contagion #f)
(define econtagion #f)
(define pcontagion #f)

(let ()

  ; Many of the numbers created here violate various invariants in the 
  ; system; this is how it should be.  
  ;
  ; Some procedures like flonum->bignum and bignum->fixnum exist other 
  ; places in the system.

  (define (fixnum->ratnum f)
    (make-unreduced-ratnum f 1))

  (define (fixnum->rectnum f)
    (make-rectnum f 0))

  (define (fixnum->flonum x)
    (exact->inexact x))

  (define (fixnum->compnum f)
    (make-compnum (exact->inexact f) 0.0))

  (define (bignum->ratnum f)
    (make-unreduced-ratnum f 1))

  (define (bignum->rectnum f)
    (make-rectnum f 0))

  (define (bignum->compnum f)
    (make-compnum (exact->inexact:rational f) 0.0))

  (define (ratnum->rectnum f)
    (make-rectnum f 0))

  (define (ratnum->flonum f)
    ; This is hard when the denominator is large.
    (exact->inexact:rational f))

  (define (ratnum->compnum f)
    (make-compnum (ratnum->flonum f) 0.0))

  (define (rectnum->compnum f)
    (make-compnum (exact->inexact (real-part f)) 
                  (exact->inexact (imag-part f))))

  ;; Used only when comparing to a fixnum.

  (define (flonum->flonum f)
    (if (and (eq? 'extremely (larceny:r7strict))
             (= f 0.0))
        0.5
        f))

  (define (flonum->compnum f)
    (make-compnum f 0.0))

  ;; Used only when comparing to a fixnum for equality.

  (define (compnum->compnum z)
    (if (and (eq? 'extremely (larceny:r7strict))
             (= (real-part z) 0.0))
        (make-rectangular 0.5 0.0)
        z))

  (define (id x) x)

  (define (oops a b retry)
    (error "INTERNAL ERROR in contagion: same-representation arithmetic: "
           "ops = " a ", " b "; code=" (contagion-error-code retry))
    #t)

  (define (oops-in-predicate a b retry)
    (assertion-violation (contagion-error-proc (contagion-error-code retry))
                         (errmsg 'msg:notreal)
                         a b)
    #t)

  (define (fun f1 f2)
    (lambda (a b retry)
      (retry (f1 a) (f2 b))))

  ; For operations targeted by algorithm* a compnum with a zero imag
  ; part is equivalent to a flonum, and the operations are well-defined
  ; in the millicode implementations.  Hence, the compnum case with
  ; zero imag part can just return or operate on the compnum.

  (define (->int a)                        ; 'a' flonum, compnum w/0i, bignum
    (if (bytevector-like? a)
        (let ((t (typetag a)))
          (cond ((eq? t sys$tag.bignum-typetag) a)
                ((eq? t sys$tag.flonum-typetag) (flonum->bignum a))
                ((eq? t sys$tag.compnum-typetag)
                 (if (zero? (imag-part a))
                     (flonum->bignum (real-part a))
                     #f))
                (else
                 #f)))
        #f))

  (define (->rat a)                        ; 'a' flonum, compnum w/0i, ratnum
    (if (bytevector-like? a)
        (let ((t (typetag a)))
          (cond ((eq? t sys$tag.flonum-typetag) (flonum->ratnum a))
                ((eq? t sys$tag.compnum-typetag)
                 (if (zero? (imag-part a))
                     (flonum->ratnum (real-part a))
                     #f))
                (else
                 #f)))
        ; must be a ratnum, so return it
        a))

  (define (->flo a)                        ; 'a' flonum, bignum
    (if (bytevector-like? a)
        (let ((t (typetag a)))
          (cond ((eq? t sys$tag.flonum-typetag) a)
                ((eq? t sys$tag.bignum-typetag) 
                 (exact->inexact:rational a))
                (else
                 #f)))
        #f))

  (define (->flo/comp a)                ; 'a' flonum, compnum, bignum
    (if (and (bytevector-like? a)
             (eq? (typetag a) sys$tag.bignum-typetag))
        (exact->inexact:rational a)
        a))

  (define (->rect a)                        ; 'a' is anything
    (cond ((compnum? a)
           (make-rectangular (inexact->exact (real-part a))
                             (inexact->exact (imag-part a))))
          ((rectnum? a)
           a)
          (else
           (make-rectangular (inexact->exact a) 0))))

  (define (->comp a)                        ; 'a' is anything
    (cond ((compnum? a) a)
          ((rectnum? a)
           (make-rectangular (exact->inexact (real-part a))
                             (exact->inexact (imag-part a))))
          (else
           (make-rectangular (exact->inexact a) 0))))

  ; Algorithm* for arithmetic.  If both are representable as
  ; exact integers, convert to bignums and compute, and then
  ; convert to inexact.
  ; Otherwise, convert to flonums and compute.
  ; One input is a bignum, the other a flonum or compnum.

  (define (algorithm*c a b retry)
    (if (and (integer? a)
             (integer? b))
        (let ((a (->int a))
              (b (->int b)))
          (exact->inexact (retry a b)))
        (let ((a (->flo/comp a))
              (b (->flo/comp b)))
          (retry a b))))

  ; Algorithm* for ordering predicates (<, <=, >, >=): if both are
  ; representable as exact integers, represent as bignums and compare. 
  ; Otherwise represent as ratnums and compare.  One of the arguments
  ; is a bignum, the other is a flonum.

  (define (algorithm*p a b retry)
    (cond ((and (integer? a)
                (integer? b))
           (let ((a (->int a))
                 (b (->int b)))
             (if (and a b)
                 (retry a b)
                 (contagion-error a b retry))))
          ((flonum? a)
           (let ((zero (- a a)))
             (if (= zero zero)
                 (retry (->rat a) (bignum->ratnum b))
                 (retry a 0.0))))
          (else
           (let ((zero (- b b)))
             (if (= zero zero)
                 (retry (bignum->ratnum a) (->rat b))
                 (retry 0.0 b))))))

  ; Algorithm* for ordering predicates (<, <=, >, >=): if both are
  ; representable as exact rationals, represent as such and compare. 
  ; Otherwise the other involves an infinity or NaN, so 0.0 can be
  ; substituted for the ratnum.
  ; One of the arguments is a ratnum, the other is a flonum.

  (define (algorithm*pratnum a b retry)
    (cond ((flonum? a)
           (let ((zero (- a a)))
             (if (= zero zero)
                 (retry (->rat a) b)
                 (retry a 0.0))))
          ((flonum? b)
           (let ((zero (- b b)))
             (if (= zero zero)
                 (retry a (->rat b))
                 (retry 0.0 b))))
          (else
           (contagion-error a b retry))))
          
  ; Algorithm* for equality predicates.  If both are representable as
  ; exact integers, convert to bignums and compare.  Otherwise, convert
  ; to flonums and compare.
  ; One input is a bignum, the other a flonum or compnum.

  (define (algorithm*e a b retry)
    (cond ((and (integer? a)
                (integer? b)
                (if (inexact? a)
                    (let ((zero (- a a)))
                      (= zero zero))
                    (let ((zero (- b b)))
                      (= zero zero))))
           (let ((a (->int a))
                 (b (->int b)))
             (retry a b)))
          ((flonum? a)
           (let ((zero (- a a)))
             (if (= zero zero)
                 (retry (->rat a) (bignum->ratnum b))
                 #f)))
          ((compnum? a)
           (if (zero? (imag-part a))
               (algorithm*e (real-part a) b retry)
               #f))
          (else
           (algorithm*e b a retry))))

  ; Algorithm* for equality.  If both are representable as exact
  ; rationals, represent as such and compare. 
  ; Otherwise the other involves an infinity or NaN, so result is false.
  ; One of the arguments is a ratnum, the other is a flonum or compnum.

  (define (algorithm*eratnum a b retry)
    (cond ((flonum? a)
           (let ((zero (- a a)))
             (if (= zero zero)
                 (retry (->rat a) b)
                 #f)))
          ((flonum? b)
           (let ((zero (- b b)))
             (if (= zero zero)
                 (retry a (->rat b))
                 #f)))
          ((compnum? a)
           (if (zero? (imag-part a))
               (algorithm*eratnum (real-part a) b retry)
               #f))
          ((compnum? b)
           (if (zero? (imag-part b))
               (algorithm*eratnum a (real-part b) retry)
               #f))
          (else
           (contagion-error a b retry))))
          
  ; Algorithm*c for at least one compnum.

  (define (algorithm*cr a b retry)
    (if (and (integer? (real-part a))
             (integer? (imag-part a))
             (integer? (real-part b))
             (integer? (imag-part b)))
        (let ((a (->rect a))
              (b (->rect b)))
          (exact->inexact (retry a b)))
        (let ((a (->comp a))
              (b (->comp b)))
          (retry a b))))

  ; Algorithm*e for at least one complex number.

  (define (algorithm*cre a b retry)
    (if (= (imag-part a) (imag-part b))
        (retry (real-part a) (real-part b))
        #f))

  ; Signal an error given an index or a procedure from the millicode vector.
  
  (define (contagion-error a b retry-idx)
    (let ((code (contagion-error-code retry-idx)))
      (if (not code)
          (error "INTERNAL ERROR in Lib/contag.sch:contagion-error, arguments "
                 (list a b retry-idx))
          ((error-handler) code a b (unspecified)))
      #t))

  (define (do-contagion matrix a b retry)
    (let ((fixtype  0)
          (bigtype  1)
          (rattype  2)
          (recttype 3)
          (flotype  4)
          (comptype 5))

      ; number-type is highly bummed but can be made even faster by
      ; elminating the conds and instead simply shifting bytevector-like
      ; tags (say) by three bits, thereby creating a unique index space
      ; based on the type tag alone, and having contagion matrices with
      ; a number of unused slots.  However, most time is probably not
      ; spent in this procedure.

      (define (number-type x)
        (cond ((fixnum? x) fixtype)
              ((bytevector-like? x)
               (let ((t (typetag x)))
                 (cond ((eq? t sys$tag.flonum-typetag)  flotype)
                       ((eq? t sys$tag.compnum-typetag) comptype)
                       ((eq? t sys$tag.bignum-typetag)  bigtype)
                       (else (contagion-error a b retry)))))
              ((vector-like? x)
               (let ((t (typetag x)))
                 (cond ((eq? t sys$tag.ratnum-typetag)  rattype)
                       ((eq? t sys$tag.rectnum-typetag) recttype)
                       (else (contagion-error a b retry)))))
              (else
               (contagion-error a b retry))))

      (let* ((ta (number-type a))
             (tb (number-type b))
             (f  (vector-ref (vector-ref matrix ta) tb)))
        (f a b retry))))

  (define cmatrix #f)
  (define ematrix #f)
  (define pmatrix #f)

  ; initialize things.

  ; Contagion matrices. They are completely symmetric with respect to the
  ; final types, although the entries in the matrix are different across the
  ; diagonal.
  ; Order: fix big rat rect flo comp
  ; Standard matrix: for arithmetic operations.

  (set! cmatrix
    (vector (vector oops
                    (fun fixnum->bignum id)
                    (fun fixnum->ratnum id) 
                    (fun fixnum->rectnum id)
                    (fun fixnum->flonum id) 
                    (fun fixnum->compnum id))
            (vector (fun id fixnum->bignum)
                    oops
                    (fun bignum->ratnum id)
                    (fun bignum->rectnum id) 
                    algorithm*c
                    algorithm*c)
            (vector (fun id fixnum->ratnum)
                    (fun id bignum->ratnum)
                    oops
                    (fun ratnum->rectnum id)
                    (fun ratnum->flonum id)
                    (fun ratnum->compnum id))
            (vector (fun id fixnum->rectnum)
                    (fun id bignum->rectnum)
                    (fun id ratnum->rectnum)
                    oops
                    algorithm*cr ; (fun rectnum->compnum flonum->compnum)
                    algorithm*cr ; (fun rectnum->compnum id)
                    )
            (vector (fun id fixnum->flonum)
                    algorithm*c
                    (fun id ratnum->flonum) 
                    algorithm*cr ; (fun flonum->compnum rectnum->compnum)
                    oops
                    (fun flonum->compnum id))
            (vector (fun id fixnum->compnum)
                    algorithm*c
                    (fun id ratnum->compnum) 
                    algorithm*cr ; (fun id rectnum->compnum)
                    (fun id flonum->compnum) 
                    oops)))

  ; Predicate matrix: for <, <=, >, >=
  ; No loss of accuracy can be tolerated here.

  (set! pmatrix
    (vector (vector oops
                    (fun fixnum->bignum id)
                    (fun fixnum->ratnum id)
                    (fun fixnum->rectnum id)
                    (fun fixnum->flonum flonum->flonum)
                    oops-in-predicate)
            (vector (fun id fixnum->bignum)
                    oops
                    (fun bignum->ratnum id)
                    (fun bignum->rectnum id)
                    algorithm*p
                    oops-in-predicate)
            (vector (fun id fixnum->ratnum)
                    (fun id bignum->ratnum)
                    oops
                    (fun ratnum->rectnum id)
                    algorithm*pratnum
                    oops-in-predicate)
            (vector (fun id fixnum->rectnum)
                    (fun id bignum->rectnum)
                    (fun id ratnum->rectnum)
                    oops
                    (fun id flonum->ratnum)
                    oops-in-predicate)
            (vector (fun flonum->flonum fixnum->flonum)
                    algorithm*p
                    algorithm*pratnum
                    (fun flonum->ratnum id)
                    oops
                    oops-in-predicate)
            (vector oops-in-predicate
                    oops-in-predicate
                    oops-in-predicate
                    oops-in-predicate
                    oops-in-predicate
                    oops-in-predicate)))

  ; Equality matrix: for = (only)

  (set! ematrix
    (vector (vector oops
                    (fun fixnum->bignum id)
                    (fun fixnum->ratnum id) 
                    (fun fixnum->rectnum id)
                    (fun fixnum->flonum flonum->flonum) 
                    (fun fixnum->compnum compnum->compnum))
            (vector (fun id fixnum->bignum)
                    oops
                    (fun bignum->ratnum id)
                    (fun bignum->rectnum id)
                    algorithm*e
                    algorithm*e)
            (vector (fun id fixnum->ratnum)
                    (fun id bignum->ratnum)
                    oops
                    (fun ratnum->rectnum id)
                    algorithm*eratnum
                    algorithm*eratnum)
            (vector (fun id fixnum->rectnum)
                    (fun id bignum->rectnum)
                    (fun id ratnum->rectnum)
                    oops
                    algorithm*cre
                    algorithm*cre)
            (vector (fun flonum->flonum fixnum->flonum)
                    algorithm*e
                    algorithm*eratnum
                    algorithm*cre
                    oops
                    algorithm*cre)
            (vector (fun compnum->compnum fixnum->compnum)
                    algorithm*e
                    algorithm*eratnum
                    algorithm*cre
                    algorithm*cre
                    oops)))

  (set! contagion (lambda (a b retry)
                    (do-contagion cmatrix a b retry)))

  (set! econtagion (lambda (a b retry)
                     (do-contagion ematrix a b retry)))

  (set! pcontagion (lambda (a b retry)
                     (do-contagion pmatrix a b retry)))

  'contagion)

; eof
