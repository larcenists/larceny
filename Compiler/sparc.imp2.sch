; Target-specific representations.
;
; A few of these representation types must be specified for every target:
;     rep:object
;     rep:procedure
;     rep:true
;     rep:false
;     rep:bottom

(define-subtype 'true       'object)      ; values that count as true
(define-subtype 'eqtype     'object)      ; can use EQ? instead of EQV?
(define-subtype 'nonpointer 'eqtype)      ; can omit write barrier
(define-subtype 'eqtype1    'eqtype)      ; eqtypes excluding #f
(define-subtype 'boolean    'nonpointer)
(define-subtype 'truth      'eqtype1)     ; { #t }
(define-subtype 'truth      'boolean)
(define-subtype 'false      'boolean)     ; { #f }
(define-subtype 'eqtype1    'true)  
(define-subtype 'procedure  'true)
(define-subtype 'vector     'true)
(define-subtype 'bytevector 'true)
(define-subtype 'string     'true)
(define-subtype 'pair       'true)
(define-subtype 'emptylist  'eqtype1)
(define-subtype 'emptylist  'nonpointer)
(define-subtype 'symbol     'eqtype1)
(define-subtype 'char       'eqtype1)
(define-subtype 'char       'nonpointer)
(define-subtype 'number     'true)
(define-subtype 'inexact    'number)
(define-subtype 'flonum     'inexact)
(define-subtype 'integer    'number)
(define-subtype 'exact      'number)
(define-subtype 'exactint   'integer)
(define-subtype 'exactint   'exact)
(define-subtype 'fixnum     'exactint)
(define-subtype '!fixnum    'fixnum)      ; 0 <= n
(define-subtype 'fixnum!    'fixnum)      ; n <= largest index
(define-subtype 'index      '!fixnum)
(define-subtype 'index      'fixnum!)
(define-subtype 'zero       'index)
(define-subtype 'fixnum     'eqtype1)
(define-subtype 'fixnum     'nonpointer)

(compute-type-structure!)

; If the intersection of rep1 and rep2 is known precisely,
; but neither is a subtype of the other, then their intersection
; should be declared explicitly.
; Otherwise a conservative approximation will be used.

(define-intersection 'true 'eqtype 'eqtype1)
(define-intersection 'true 'boolean 'truth)
(define-intersection 'exact 'integer 'exactint)
(define-intersection '!fixnum 'fixnum! 'index)

;(display-unions-and-intersections)

; Parameters.

(define rep:min_fixnum (- (expt 2 29)))
(define rep:max_fixnum (- (expt 2 29) 1))
(define rep:max_index  (- (expt 2 24) 1))

; The representations we'll recognize for now.

(define rep:object       (symbol->rep 'object))
(define rep:true         (symbol->rep 'true))
(define rep:truth        (symbol->rep 'truth))
(define rep:false        (symbol->rep 'false))
(define rep:boolean      (symbol->rep 'boolean))
(define rep:pair         (symbol->rep 'pair))
(define rep:symbol       (symbol->rep 'symbol))
(define rep:number       (symbol->rep 'number))
(define rep:zero         (symbol->rep 'zero))
(define rep:index        (symbol->rep 'index))
(define rep:fixnum       (symbol->rep 'fixnum))
(define rep:exactint     (symbol->rep 'exactint))
(define rep:flonum       (symbol->rep 'flonum))
(define rep:exact        (symbol->rep 'exact))
(define rep:inexact      (symbol->rep 'inexact))
(define rep:integer      (symbol->rep 'integer))
;(define rep:real         (symbol->rep 'real))
(define rep:char         (symbol->rep 'char))
(define rep:string       (symbol->rep 'string))
(define rep:vector       (symbol->rep 'vector))
(define rep:procedure    (symbol->rep 'procedure))
(define rep:bottom       (symbol->rep 'bottom))

; Given the value of a quoted constant, return its representation.

(define (representation-of-value x)
  (cond ((boolean? x)
         (if x
             rep:truth
             rep:false))
        ((pair? x)
         rep:pair)
        ((symbol? x)
         rep:symbol)
        ((number? x)
         (cond ((and (exact? x)
                     (integer? x))
                (cond ((zero? x)
                       rep:zero)
                      ((<= 0 x rep:max_index)
                       rep:index)
                      ((<= rep:min_fixnum
                           x
                           rep:max_fixnum)
                       rep:fixnum)
                      (else
                       rep:exactint)))
               ((and (inexact? x)
                     (real? x))
                rep:flonum)
               (else
                ; We're not tracking other numbers yet.
                rep:number)))
        ((char? x)
         rep:char)
        ((string? x)
         rep:string)
        ((vector? x)
         rep:vector)
        ; Everything counts as true except for #f.
        (else
         rep:true)))

; Tables that express the representation-specific operations,
; and the information about representations that are implied
; by certain operations.

(define rep-specific
  
  (representation-table
   
   ; When the procedure in the first column is called with
   ; arguments described in the middle column, then the procedure
   ; in the last column can be called instead.
   
   '(
     (+                  (index index)               .+:idx:idx)
     (-                  (index index)               .-:idx:idx)

    ;These don't help on the SPARC.
    ;(+                  (fixnum fixnum)             .+:fix:fix)
    ;(-                  (fixnum fixnum)             .-:fix:fix)

     (=                  (fixnum fixnum)             .=:fix:fix)
     (<                  (fixnum fixnum)             .<:fix:fix)
     (<=                 (fixnum fixnum)             .<=:fix:fix)
     (>                  (fixnum fixnum)             .>:fix:fix)
     (>=                 (fixnum fixnum)             .>=:fix:fix)
     
     (fx+                (index index)               .+:idx:idx)
     (fx-                (index index)               .-:idx:idx)

     (fx=                (fixnum fixnum)             .=:fix:fix)
     (fx<                (fixnum fixnum)             .<:fix:fix)
     (fx<=               (fixnum fixnum)             .<=:fix:fix)
     (fx>                (fixnum fixnum)             .>:fix:fix)
     (fx>=               (fixnum fixnum)             .>=:fix:fix)
     
     (+                  (flonum flonum)             .+:flo:flo)
     (-                  (flonum flonum)             .-:flo:flo)
     (*                  (flonum flonum)             .*:flo:flo)
     (/                  (flonum flonum)             ./:flo:flo)
     (=                  (flonum flonum)             .=:flo:flo)
     (<                  (flonum flonum)             .<:flo:flo)
     (<=                 (flonum flonum)             .<=:flo:flo)
     (>                  (flonum flonum)             .>:flo:flo)
     (>=                 (flonum flonum)             .>=:flo:flo)
     
     (fl+                (flonum flonum)             .+:flo:flo)
     (fl-                (flonum flonum)             .-:flo:flo)
     (fl*                (flonum flonum)             .*:flo:flo)
     (fl/                (flonum flonum)             ./:flo:flo)
     (fl=                (flonum flonum)             .=:flo:flo)
     (fl<                (flonum flonum)             .<:flo:flo)
     (fl<=               (flonum flonum)             .<=:flo:flo)
     (fl>                (flonum flonum)             .>:flo:flo)
     (fl>=               (flonum flonum)             .>=:flo:flo)
     
     (cell-set!          (object nonpointer)         .cell-set!:nwb)
     (vector-set!:trusted (vector fixnum nonpointer) .vector-set!:trusted:nwb)
     )))

(define rep-result
  
  (representation-table
   
   ; When the operation in the first column is called with
   ; arguments described in the middle column, then the result
   ; is described by the last column.
   ; Note: operation names in this table are internal, so no dots.
   
   '((fixnum?           (fixnum)                    (truth))
     (vector?           (vector)                    (truth))
     (<=                (zero !fixnum)              (truth))
     (>=                (!fixnum zero)              (truth))
     (<=:fix:fix        (zero !fixnum)              (truth))
     (>=:fix:fix        (!fixnum zero)              (truth))
     
     (+                 (index index)               (!fixnum))
     (+                 (fixnum fixnum)             (exactint))
     (-                 (index index)               (fixnum!))
     (-                 (fixnum fixnum)             (exactint))
     
     (+                 (flonum flonum)             (flonum))
     (-                 (flonum flonum)             (flonum))
     
     (+:idx:idx         (index index)               (!fixnum))
     (+:fix:fix         (index index)               (fixnum))
     (+:fix:fix         (fixnum fixnum)             (exactint))
     (-:idx:idx         (index index)               (fixnum!))
     (-:fix:fix         (index index)               (fixnum))
     (-:fix:fix         (fixnum fixnum)             (exactint))
     
     (fx+               (fixnum fixnum)             (fixnum))
     (fx-               (fixnum fixnum)             (fixnum))

     (fl+               (flonum flonum)             (flonum))
     (fl-               (flonum flonum)             (flonum))
     (fl*               (flonum flonum)             (flonum))
     (fl/               (flonum flonum)             (flonum))

     (make-vector       (object object)             (vector))
     (vector-length:vec (vector)                    (index))

     (make-string       (object object)             (string))
     (string-length:str (string)                    (index))
     (string-ref:trusted (string fixnum)            (char))

     (cons              (object object)             (pair))

     (char->integer     (char)                      (index))
     (integer->char     (fixnum)                    (char))
     
     ; Is it really all that useful to know that the result
     ; of these comparisons is a boolean?
     ; FIXME:  If it is, then there are a zillion other predicates.
     
     (=                 (number number)             (boolean))
     (<                 (number number)             (boolean))
     (<=                (number number)             (boolean))
     (>                 (number number)             (boolean))
     (>=                (number number)             (boolean))
     
     (=:fix:fix         (fixnum fixnum)             (boolean))
     (<:fix:fix         (fixnum fixnum)             (boolean))
     (<=:fix:fix        (fixnum fixnum)             (boolean))
     (>:fix:fix         (fixnum fixnum)             (boolean))
     (>=:fix:fix        (fixnum fixnum)             (boolean))
     )))

(define rep-informing
  
  (representation-table
   
   ; When the predicate in the first column is called in the test position
   ; of a conditional expression, on arguments described by the second
   ; column, then the arguments are described by the third column if the
   ; predicate returns true, and by the fourth column if the predicate
   ; returns false.
   
   '(
     (fixnum?     (object)           (fixnum)          (object))
     (flonum?     (object)           (flonum)          (object))
     (vector?     (object)           (vector)          (object))
     (string?     (object)           (string)          (object))
     (char?       (object)           (char)            (object))
     (pair?       (object)           (pair)            (object))
     
     (=           (exactint index)   (index index)     (exactint index))
     (=           (index exactint)   (index index)     (index exactint))
     (=           (exactint !fixnum) (!fixnum !fixnum) (exactint !fixnum))
     (=           (!fixnum exactint) (!fixnum !fixnum) (!fixnum exactint))
     (=           (exactint fixnum!) (fixnum! fixnum!) (exactint fixnum!))
     (=           (fixnum! exactint) (fixnum! fixnum!) (fixnum! exactint))
     
     (<           (!fixnum fixnum!)  (index index)     (!fixnum fixnum!))
     (<           (fixnum fixnum!)   (fixnum! fixnum!) (fixnum fixnum!))
     (<           (!fixnum fixnum)   (!fixnum !fixnum) (!fixnum fixnum))
     (<           (fixnum! !fixnum)  (fixnum! !fixnum) (index index))
     
     (<=          (!fixnum fixnum!)  (index index)     (!fixnum fixnum!))
     (<=          (fixnum! !fixnum)  (fixnum! !fixnum) (index index))
     (<=          (fixnum fixnum!)   (fixnum! fixnum!) (fixnum fixnum!))
     (<=          (!fixnum fixnum)   (!fixnum !fixnum) (!fixnum fixnum))
     
     (>           (!fixnum fixnum!)  (!fixnum fixnum!) (index index))
     (>           (fixnum! !fixnum)  (index index)     (fixnum! !fixnum))
     (>           (fixnum fixnum!)   (fixnum fixnum!)  (fixnum! fixnum!))
     (>           (!fixnum fixnum)   (!fixnum fixnum)  (!fixnum !fixnum))
     
     (>=          (!fixnum fixnum!)  (!fixnum fixnum!) (index index))
     (>=          (fixnum! !fixnum)  (index index)     (fixnum! !fixnum))
     (>=          (fixnum fixnum!)   (fixnum fixnum!)  (fixnum! fixnum!))
     (>=          (!fixnum fixnum)   (!fixnum fixnum)  (!fixnum !fixnum))
     
     (=:fix:fix   (exactint index)   (index index)     (exactint index))
     (=:fix:fix   (index exactint)   (index index)     (index exactint))
     (=:fix:fix   (exactint !fixnum) (!fixnum !fixnum) (exactint !fixnum))
     (=:fix:fix   (!fixnum exactint) (!fixnum !fixnum) (!fixnum exactint))
     (=:fix:fix   (exactint fixnum!) (fixnum! fixnum!) (exactint fixnum!))
     (=:fix:fix   (fixnum! exactint) (fixnum! fixnum!) (fixnum! exactint))

     (<:fix:fix   (!fixnum fixnum!)  (index index)     (!fixnum fixnum!))
     (<:fix:fix   (fixnum! !fixnum)  (fixnum! !fixnum) (index index))
     (<:fix:fix   (fixnum fixnum!)   (fixnum! fixnum!) (fixnum fixnum!))
     (<:fix:fix   (!fixnum fixnum)   (!fixnum !fixnum) (!fixnum fixnum))
     
     (<=:fix:fix  (!fixnum fixnum!)  (index index)     (!fixnum fixnum!))
     (<=:fix:fix  (fixnum! !fixnum)  (fixnum! !fixnum) (index index))
     (<=:fix:fix  (fixnum fixnum!)   (fixnum! fixnum!) (fixnum fixnum!))
     (<=:fix:fix  (!fixnum fixnum)   (!fixnum !fixnum) (!fixnum fixnum))
     
     (>:fix:fix   (!fixnum fixnum!)  (!fixnum fixnum!) (index index))
     (>:fix:fix   (fixnum! !fixnum)  (index index)     (fixnum! !fixnum))
     (>:fix:fix   (fixnum fixnum!)   (fixnum fixnum!)  (fixnum! fixnum!))
     (>:fix:fix   (!fixnum fixnum)   (!fixnum fixnum)  (!fixnum !fixnum))
     
     (>=:fix:fix  (!fixnum fixnum!)  (!fixnum fixnum!) (index index))
     (>=:fix:fix  (fixnum! !fixnum)  (index index)     (fixnum! !fixnum))
     (>=:fix:fix  (fixnum fixnum!)   (fixnum fixnum!)  (fixnum! fixnum!))
     (>=:fix:fix  (!fixnum fixnum)   (!fixnum fixnum)  (!fixnum !fixnum))
     )))
