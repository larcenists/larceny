; Copyright 1991 William Clinger    -*- indent-tabs-mode: nil -*-
;
; $Id: iasn.imp.sch 6134 2009-03-14 17:26:03Z will $
;
; Larceny -- target-specific information for Twobit's Fence back-end.
; 2012-07-24 / lth

; Issues that need to be resolved at some point
; - Clean up table to pack it looser, group related operations, rename.  Makes it
;   easier to add primitives with related primitives rather than "at end", like now.
; - Aren't creg/creg-set! really obsolete?
; - Fixnum and flonum primitives are not implemented, not important to do this now.
; - We must now implement peephole opt for reasonable performance.
; - Some primitives that do not currently support immediate operands should be
;   fixed to accept them, cf comments in tables below.

; The maximum number of fixed arguments that may be followed by a rest
; argument.  This limitation is removed by the macro expander.

(define @maxargs-with-rest-arg@ 30)

; On the Fence platform, src/Lib/Common/regs.sch, which is loaded before this
; file, defines the following constants:
;
;  *nregs*   is the number of MacScheme machine registers
;  *lastreg* is the index of the highest register (it's *nregs* - 1)
;  *nhwregs* is the number of hardware registers
;
; At the moment we require *nhwregs* to be equal to *nregs*, if this
; changes we need a mechanism for exposing that in arch-prefers-reg?,
; below.

; Overrides hook defined in common.imp.sch.  See comment immediately
; above.

(define (arch-prefers-reg? r1 r2)
  #f)

; Variable names that indicate register targets.

(define *regnames*
  (do ((alist '() (cons (cons (string->symbol
                               (string-append ".REG" (number->string r)))
                              r)
                        alist))
       (r (- *nhwregs* 1) (- r 1)))
      ((<= r 0)
       alist)))

; A non-inclusive upper bound for the instruction encodings.

(define *number-of-mnemonics* 72)

; Integrable procedures and procedure-specific source code transformations.
; Every integrable procedure that takes a varying number of arguments must
; supply a transformation procedure to map calls into the fixed arity
; required by the MacScheme machine instructions.

; The table of integrable procedures.
; Each entry is a list of the following items:
;
;    Procedure name (or #f if the entry is for the use of the assembler only)
;    Arity (or -1 for special primops like .check!)
;    Procedure name to be used by the disassembler
;    Predicate for immediate operands (or #f)
;    Primop code used to name twobit_ macros in the output, note must be < 1000
;    The effects that kill this primop's result
;    The effects of this primop that kill available expressions
;    A flag that is #t if the primitive's implementation may call-out to Scheme
;     to implement the operation, thereby constituting an implicit return point.
;     In the future this will also be #t if the primitive may throw a continuable
;     exception.

(define (prim-entry name)
  (assq name (twobit-integrable-procedures)))

(define (prim-entry-by-opcodename name)
  (let loop ((x (twobit-integrable-procedures)))
    (cond ((null? x) #f)
          ((eq? (prim-opcodename (car x)) name)
           (car x))
          (else 
           (loop (cdr x))))))

(define prim-arity cadr)
(define prim-opcodename caddr)
(define prim-immediate? cadddr)
(define (prim-primcode entry) (car (cddddr entry)))
; effects-that-kill-result (cadr (cddddr entry))
; effects that kill-available (caddr (cddddr entry))

(define (op1-primcode name)
  (prim-primcode (prim-entry-by-opcodename name)))

(define op2-primcode op1-primcode)
(define op3-primcode op1-primcode)

; This predicate returns #t iff its argument will be represented
; as a fixnum on the target machine.

(define smallint?
  (let* ((least (- (expt 2 29)))
         (greatest (- (- least) 1)))
    (lambda (x)
      (and (number? x)
           (exact? x)
           (integer? x)
           (<= least x greatest)))))

(define (fence-imm? x)
  (smallint? x))

(define (fence-eq-imm? x)
  (or (fence-imm? x)
      (eq? x #t)
      (eq? x #f)
      (eq? x '())))

(define (valid-typetag? x)
  (and (fixnum? x)
       (<= 0 x 7)))

; The table of primitives has been extended with
; kill information used for commoning.

(define (prim-lives-until entry)
  (list-ref entry 5))

(define (prim-kills entry)
  (list-ref entry 6))

(define (twobit-integrable-procedures)
  (case (integrate-procedures)
    ((none)    $minimal-integrable-procedures$)
    ((r4rs)    $r4rs-integrable-procedures$)
    ((r5rs)    $r5rs-integrable-procedures$)
    ((larceny) $usual-integrable-procedures$)
    (else ???)))

; This table consists of primitives that are introduced by Twobit's
; source code transformations.

(define $minimal-integrable-procedures$
  (let ((:globals  available:killer:globals)
        (:car      available:killer:car)
        (:cdr      available:killer:cdr)
        (:string   available:killer:string)
        (:vector   available:killer:vector)
        (:cell     available:killer:cell)
        (:io       available:killer:io)
        (:none     available:killer:none)     ; none of the above
        (:all      available:killer:all)      ; all of the above
        (:immortal available:killer:immortal) ; never killed
        (:dead     available:killer:dead)     ; never available
        )

;    external     arity  internal    immediate   primcode  killed     kills cont
;    name                name        predicate             by what
;                                                          kind of
;                                                          effect
  `((,name:CAR        1 car              #f            15 ,:car      ,:none #f)
    (,name:CDR        1 cdr              #f            16 ,:cdr      ,:none #f)
    (,name:MAKE-CELL  1 make-cell        #f            52 ,:dead     ,:none #f)
    (,name:CELL-REF   1 cell-ref         #f            54 ,:cell     ,:none #f)
    (,name:CELL-SET!  2 cell-set!        #f            84 ,:dead     ,:cell #f)
    (.cell-set!:nwb   2 cell-set!:nwb    #f            84 ,:dead     ,:cell #f)  ; FIXME
    (,name:CONS       2 cons             #f            58 ,:dead     ,:none #f)

    (.unspecified     0 unspecified      #f             3 ,:dead     ,:none #f)
    (.undefined       0 undefined        #f             4 ,:dead     ,:none #f)
    (.fixnum?         1 fixnum?          #f            23 ,:immortal ,:none #f)
    (.symbol?         1 symbol?          #f            17 ,:immortal ,:none #f)
    (.char?           1 char?            #f            36 ,:immortal ,:none #f)
    (.char->integer   1 char->integer    #f            37 ,:immortal ,:none #f)
    (.char->integer:chr 1 char->integer:chr #f         37 ,:immortal ,:none #f)
    (.integer->char     1 integer->char     #f         -1 ,:immortal ,:none #f)
    (.integer->char:fix 1 integer->char:fix #f         -1 ,:immortal ,:none #f)
    (.integer->char:trusted 1 integer->char:trusted #f -1 ,:immortal ,:none #f)
    (.--              1 --               #f            32 ,:immortal ,:none #t)

    ; FIXME: unspecified, undefined and -- should not be here with these 
    ; names but are introduced by the compiler, macro expander, or standard 
    ; macros.

;   (unspecified      0 unspecified      #f            -1 ,:dead     ,:none #f)
;   (undefined        0 undefined        #f             4 ,:dead     ,:none #t)
;   (--               1 --               #f            32 ,:immortal ,:none #t)

    ; Added for CSE, representation analysis.

    (,name:CHECK!    -1 check!           #f            -1 ,:dead     ,:none #f)
    (.vector-length:vec 1 vector-length:vec #f        401 ,:immortal ,:none #f)
    (.vector-ref:trusted
                      2 vector-ref:trusted ,fence-imm? 402 ,:vector   ,:none #f)
    (.vector-set!:trusted
                      3 vector-set!:trusted #f        403 ,:dead   ,:vector #f)
    (.vector-set!:trusted:nwb
                      3 vector-set!:trusted:nwb #f    403 ,:dead   ,:vector #f)
    (.string-length:str
                      1 ustring-length:str #f          40 ,:immortal ,:none #f)
    (.string-ref:trusted
                      2 ustring-ref:trusted #f         78 ,:string   ,:none #f)
    (.string-set!:trusted
                      3 ustring-set!:trusted ,fence-imm? 79 ,:dead  ,:string #f)

    ; FIXME: should support immediate index for bytevector-ref

    (.bytevector-like-length:bvl
                      1 bytevector-like-length:bvl #f  -1 ,:immortal ,:none #f)
    (.bytevector-like-ref:trusted
                      2 bytevector-like-ref:trusted #f -1 ,:string   ,:none #f)
    (.bytevector-like-set!:trusted
                      3 bytevector-like-set!:trusted #f -1 ,:dead  ,:string #f)

    (.car:pair        1 car:pair         #f           404 ,:car      ,:none #f)
    (.cdr:pair        1 cdr:pair         #f           405 ,:cdr      ,:none #f)

    (.+:idx:idx       2 +:idx:idx        ,fence-imm?   500 ,:immortal ,:none #f)
    (.+:fix:fix       2 +:fix:fix        ,fence-imm?   501 ,:immortal ,:none #t)
    (.-:idx:idx       2 -:idx:idx        ,fence-imm?   502 ,:immortal ,:none #f)
    (.-:fix:fix       2 -:fix:fix        ,fence-imm?   503 ,:immortal ,:none #t)

    (.=:fix:fix       2 =:fix:fix        ,fence-imm?   406 ,:immortal ,:none #f)
    (.<:fix:fix       2 <:fix:fix        ,fence-imm?   407 ,:immortal ,:none #f)
    (.<=:fix:fix      2 <=:fix:fix       ,fence-imm?   408 ,:immortal ,:none #f)
    (.>=:fix:fix      2 >=:fix:fix       ,fence-imm?   409 ,:immortal ,:none #f)
    (.>:fix:fix       2 >:fix:fix        ,fence-imm?   410 ,:immortal ,:none #f)
    
    (.fxlognot        1 fxlognot         #f            33 ,:immortal ,:none #f)
    (.fxlogand        2 fxlogand         #f            71 ,:immortal ,:none #f)
    (.fxlogior        2 fxlogior         #f            72 ,:immortal ,:none #f)
    (.fxlogxor        2 fxlogxor         #f            73 ,:immortal ,:none #f)
    (.fxlsh           2 fxlsh            #f            74 ,:immortal ,:none #f)
    (.fxrsha          2 fxrsha           #f            75 ,:immortal ,:none #f)
    (.fxrshl          2 fxrshl           #f            76 ,:immortal ,:none #f)

    (.+:flo:flo       2 +:flo:flo        #f            -1 ,:immortal ,:none)
    (.-:flo:flo       2 -:flo:flo        #f            -1 ,:immortal ,:none)
    (.*:flo:flo       2 *:flo:flo        #f            -1 ,:immortal ,:none)
    (./:flo:flo       2 /:flo:flo        #f            -1 ,:immortal ,:none)

    (.=:flo:flo       2 =:flo:flo        #f            -1 ,:immortal ,:none)
    (.<:flo:flo       2 <:flo:flo        #f            -1 ,:immortal ,:none)
    (.<=:flo:flo      2 <=:flo:flo       #f            -1 ,:immortal ,:none)
    (.>:flo:flo       2 >:flo:flo        #f            -1 ,:immortal ,:none)
    (.>=:flo:flo      2 >=:flo:flo       #f            -1 ,:immortal ,:none)

    ;; FIXME: This is not a good place for these, because the compiler
    ;; doesn't need to see them:

    (#f               1 syscall          #f            105 #f         #f     #t)

    ; Introduced by peephole optimization
    ; External name, immediate predicate, killed, and kills are not
    ; used for these.

    ;; FIXME: This is not a good place for these, because the compiler
    ;; doesn't need to see them:

    (#f               2 make-vector:0    #f            600 _          _ #f)
    (#f               2 make-vector:1    #f            601 _          _ #f)
    (#f               2 make-vector:2    #f            602 _          _ #f)
    (#f               2 make-vector:3    #f            603 _          _ #f)
    (#f               2 make-vector:4    #f            604 _          _ #f)
    (#f               2 make-vector:5    #f            605 _          _ #f)
    (#f               2 make-vector:6    #f            606 _          _ #f)
    (#f               2 make-vector:7    #f            607 _          _ #f)
    (#f               2 make-vector:8    #f            608 _          _ #f)
    (#f               2 make-vector:9    #f            609 _          _ #f)
    (#f               1 internal:branchf-null? #f      610 _          _ #f)
    (#f               1 internal:branchf-pair? #f      611 _          _ #f)
    (#f               1 internal:branchf-zero? #f      612 _          _ #t)
    (#f               1 internal:branchf-eof-object? #f 613 _         _ #f)
    (#f               1 internal:branchf-fixnum? #f    614 _          _ #f)
    (#f               1 internal:branchf-char? #f      615 _          _ #f)
    (#f               1 internal:branchf-fxzero? #f    616 _          _ #f)
    (#f               1 internal:branchf-fxnegative? #f 617 _         _ #f)
    (#f               1 internal:branchf-fxpositive? #f 618 _         _ #f)
    (#f               2 internal:branchf-< #f          619 _          _ #t)
    (#f               2 internal:branchf-> #f          620 _          _ #t)
    (#f               2 internal:branchf->= #f         621 _          _ #t)
    (#f               2 internal:branchf-<= #f         622 _          _ #t)
    (#f               2 internal:branchf-= #f          623 _          _ #t)
    (#f               2 internal:branchf-eq? #f        624 _          _ #f)
    (#f               2 internal:branchf-char=? #f     625 _          _ #f)
    (#f               2 internal:branchf-char>=? #f    626 _          _ #f)
    (#f               2 internal:branchf-char>? #f     627 _          _ #f)
    (#f               2 internal:branchf-char<=? #f    628 _          _ #f)
    (#f               2 internal:branchf-char<? #f     629 _          _ #f)
    (#f               2 internal:branchf-fx= #f        630 _          _ #f)
    (#f               2 internal:branchf-fx> #f        631 _          _ #f)
    (#f               2 internal:branchf-fx>= #f       632 _          _ #f)
    (#f               2 internal:branchf-fx< #f        633 _          _ #f)
    (#f               2 internal:branchf-fx<= #f       634 _          _ #f)
    (#f               2 internal:branchf-</imm #f      635 _          _ #t)
    (#f               2 internal:branchf->/imm #f      636 _          _ #t)
    (#f               2 internal:branchf->=/imm #f     637 _          _ #t)
    (#f               2 internal:branchf-<=/imm #f     638 _          _ #t)
    (#f               2 internal:branchf-=/imm #f      639 _          _ #t)
    (#f               2 internal:branchf-eq?/imm #f    640 _          _ #f)
    (#f               2 internal:branchf-char=?/imm #f 641 _          _ #f)
    (#f               2 internal:branchf-char>=?/imm #f 642 _         _ #f)
    (#f               2 internal:branchf-char>?/imm #f 643 _          _ #f)
    (#f               2 internal:branchf-char<=?/imm #f 644 _         _ #f)
    (#f               2 internal:branchf-char<?/imm #f 645 _          _ #f)
    (#f               2 internal:branchf-fx=/imm #f    646 _          _ #f)
    (#f               2 internal:branchf-fx>/imm #f    647 _          _ #f)
    (#f               2 internal:branchf-fx>=/imm #f   648 _          _ #f)
    (#f               2 internal:branchf-fx</imm #f    649 _          _ #f)
    (#f               2 internal:branchf-fx<=/imm #f   650 _          _ #f)
    (#f               1 internal:check-fixnum? #f      651 _          _ #f)
    (#f               1 internal:check-pair? #f        652 _          _ #f)
    (#f               1 internal:check-vector? #f      653 _          _ #f)
    (#f               1 internal:check-string? #f      654 _          _ #f)
    (#f               2 internal:check-<:fix:fix #f    655 _          _ #f)
    (#f               2 internal:check-<=:fix:fix #f   656 _          _ #f)
    (#f               2 internal:check->=:fix:fix #f   657 _          _ #f)
    (#f               2 internal:check-<:fix:fix/imm #f 658 _         _ #f)
    (#f               2 internal:check-<=:fix:fix/imm #f 659 _        _ #f)
    (#f               2 internal:check->=:fix:fix/imm #f 660 _        _ #f)
    (#f               2 internal:check-range #f        661 _          _ #f)
    (#f               2 internal:check-vector?/vector-length:vec #f 662 _ _ #f)
    (#f               2 internal:check-string?/string-length:str #f 663 _ _ #f)
    )))

(define $r4rs-integrable-procedures$
  (let ((:globals  available:killer:globals)
        (:car      available:killer:car)
        (:cdr      available:killer:cdr)
        (:string   available:killer:string)
        (:vector   available:killer:vector)
        (:cell     available:killer:cell)
        (:io       available:killer:io)
        (:none     available:killer:none)     ; none of the above
        (:all      available:killer:all)      ; all of the above
        (:immortal available:killer:immortal) ; never killed
        (:dead     available:killer:dead)     ; never available
        )

;    external     arity  internal    immediate   primcode  killed     kills cont
;    name                name        predicate             by what
;                                                          kind of
;                                                          effect
 (append
  `((eof-object       0 eof-object       #f             5 ,:dead     ,:none #f)
    (not              1 not              #f             9 ,:immortal ,:none #f)
    (null?            1 null?            #f            10 ,:immortal ,:none #f)
    (pair?            1 pair?            #f            11 ,:immortal ,:none #f)
    (eof-object?      1 eof-object?      #f            12 ,:immortal ,:none #f)
    (car              1 car              #f            15 ,:car      ,:none #f)
    (cdr              1 cdr              #f            16 ,:cdr      ,:none #f)
    (symbol?          1 symbol?          #f            17 ,:immortal ,:none #f)
    (number?          1 complex?         #f            18 ,:immortal ,:none #f)
    (complex?         1 complex?         #f            18 ,:immortal ,:none #f)
    (real?            1 rational?        #f            20 ,:immortal ,:none #f)
;   (rational?        1 rational?        #f            20 ,:immortal ,:none #f)
    (integer?         1 integer?         #f            22 ,:immortal ,:none #f)
    (exact?           1 exact?           #f            25 ,:immortal ,:none #f)
    (inexact?         1 inexact?         #f            26 ,:immortal ,:none #f)
    (exact->inexact   1 exact->inexact   #f            27 ,:immortal ,:none #t)
    (inexact          1 exact->inexact   #f            27 ,:immortal ,:none #t)
    (exact            1 inexact->exact   #f            28 ,:immortal ,:none #t)
    (inexact->exact   1 inexact->exact   #f            28 ,:immortal ,:none #t)
    (round            1 round            #f            29 ,:immortal ,:none #t)
    (truncate         1 truncate         #f            30 ,:immortal ,:none #t)
    (zero?            1 zero?            #f            31 ,:immortal ,:none #t)
    (real-part        1 real-part        #f            34 ,:immortal ,:none #f)
    (imag-part        1 imag-part        #f            35 ,:immortal ,:none #f)
    (char?            1 char?            #f            36 ,:immortal ,:none #f)
    (char->integer    1 char->integer    #f            37 ,:immortal ,:none #f)
    (integer->char    1 integer->char    #f            38 ,:immortal ,:none #f)
    (string?          1 ustring?         #f            39 ,:immortal ,:none #f)
    (string-length    1 ustring-length:str #f          40 ,:immortal ,:none #f)
    (vector?          1 vector?          #f            41 ,:immortal ,:none #f)
    (vector-length    1 vector-length    #f            42 ,:immortal ,:none #f)
    (procedure?       1 procedure?       #f            47 ,:immortal ,:none #f)
    (eq?              2 eq?           ,fence-eq-imm?    56 ,:immortal ,:none #f)
    (eqv?             2 eqv?             #f            57 ,:immortal ,:none #t)
    (cons             2 cons             #f            58 ,:dead     ,:none #f)
    (set-car!         2 set-car!         #f            59 ,:dead     ,:car  #f)
    (set-cdr!         2 set-cdr!         #f            60 ,:dead     ,:cdr  #f)
    (+                2 +                ,fence-imm?    61 ,:immortal ,:none #t)
    (-                2 -                ,fence-imm?    62 ,:immortal ,:none #t)
; FIXME: Immediate version of * not yet operational
;    (*                2 *                ,fence-imm?    63 ,:immortal ,:none #t)
    (*                2 *                #f            63 ,:immortal ,:none #t)
    (/                2 /                #f            64 ,:immortal ,:none #t)
    (quotient         2 quotient         #f            65 ,:immortal ,:none #t)
    (<                2 <                ,fence-imm?    66 ,:immortal ,:none #t)
    (<=               2 <=               ,fence-imm?    67 ,:immortal ,:none #t)
    (=                2 =                ,fence-imm?    68 ,:immortal ,:none #t)
    (>                2 >                ,fence-imm?    69 ,:immortal ,:none #t)
    (>=               2 >=               ,fence-imm?    70 ,:immortal ,:none #t)
    (make-string      2 make-ustring     #f           109 ,:dead     ,:none #f)
   ;FIXME: should recognize immediate operands
   ;(string-ref       2 ustring-ref:trusted ,fence-imm? 78 ,:string   ,:none #f)
    (string-ref       2 ustring-ref:trusted ,fence-imm? 78 ,:string   ,:none #f)
   ;(string-set!      3 ustring-set!:trusted ,fence-imm? 79 ,:dead   ,:string #f)
    (string-set!      3 ustring-set!:trusted #f        79 ,:dead    ,:string #f)
    (make-vector      2 make-vector      #f            80 ,:dead     ,:none #f)
    (vector-ref       2 vector-ref       ,fence-imm?    81 ,:vector   ,:none #f)
    (char<?           2 char<?           ,char?        85 ,:immortal ,:none #f)
    (char<=?          2 char<=?          ,char?        86 ,:immortal ,:none #f)
    (char=?           2 char=?           ,char?        87 ,:immortal ,:none #f)
    (char>?           2 char>?           ,char?        88 ,:immortal ,:none #f)
    (char>=?          2 char>=?          ,char?        89 ,:immortal ,:none #f)
    (vector-set!      3 vector-set!      #f            91 ,:dead     ,:vector #f)
    (remainder        2 remainder        #f           103 ,:immortal ,:none #t))
  $minimal-integrable-procedures$)))

(define $r5rs-integrable-procedures$
  $r4rs-integrable-procedures$)

(define $usual-integrable-procedures$
  (let ((:globals  available:killer:globals)
        (:car      available:killer:car)
        (:cdr      available:killer:cdr)
        (:string   available:killer:string)
        (:vector   available:killer:vector)
        (:cell     available:killer:cell)
        (:io       available:killer:io)
        (:none     available:killer:none)     ; none of the above
        (:all      available:killer:all)      ; all of the above
        (:immortal available:killer:immortal) ; never killed
        (:dead     available:killer:dead)     ; never available
        )

;    external     arity  internal    immediate   primcode  killed    kills cont
;    name                name        predicate             by what
;                                                          kind of
;                                                          effect

  (append
  `(
    (larceny-break    0 break            #f             1 ,:dead     ,:all  #f)
    (.creg            0 creg             #f           106 ,:dead     ,:all  #f)
    (enable-interrupts 1 enable-interrupts #f           6 ,:dead     ,:all  #t)
    (disable-interrupts 0 disable-interrupts #f         7 ,:dead     ,:all  #t)

    ; FIXME: these will eventually replace the string operations

    (make-ustring     2 make-ustring     #f            -1 ,:dead     ,:none)
    (ustring?         1 ustring?         #f            39 ,:immortal ,:none)
   ;(ustring-length   1 ustring-length   #f            40 ,:immortal ,:none)
   ;(ustring-ref      2 ustring-ref      ,fence-imm?    78 ,:string   ,:none)
   ;(ustring-set!     3 ustring-set!     #f            -1 ,:dead     ,:string)
    (.ustring-length:str 1 ustring-length:str #f       -1 ,:immortal ,:none)
    (.ustring-ref:trusted 2 ustring-ref:trusted #f     -1 ,:string   ,:none)
    (.ustring-set!:trusted 3 ustring-set!:trusted #f   -1 ,:dead     ,:string)

    
    (typetag          1 typetag          #f             8 ,:dead     ,:none #f)
    (port?            1 port?            #f            13 ,:dead     ,:none #f)
    (structure?       1 structure?       #f            14 ,:dead     ,:none #f)
    (fixnum?          1 fixnum?          #f            23 ,:immortal ,:none #f)
    (flonum?          1 flonum?          #f            24 ,:immortal ,:none #f)
    (compnum?         1 compnum?         #f            21 ,:immortal ,:none #f)
    (fxlognot         1 fxlognot         #f            33 ,:immortal ,:none #f)
    (bytevector?      1 bytevector?      #f            43 ,:immortal ,:none #f)
    (bytevector-length 1 bytevector-length #f          44 ,:immortal ,:none #f)
    (bytevector-fill! 2 bytevector-fill! #f            45 ,:dead     ,:string #f)
    (make-bytevector  1 make-bytevector  #f            46 ,:dead     ,:none #f)
    (procedure-length 1 procedure-length #f            48 ,:dead     ,:none #f)
    (make-procedure   1 make-procedure   #f            49 ,:dead     ,:none #f)
    (.creg-set!       1 creg-set!        #f           107 ,:dead     ,:none #f)
    (typetag-set!     2 typetag-set! ,valid-typetag?   55 ,:dead     ,:all #f)
    (fxlogand         2 fxlogand         #f            71 ,:immortal ,:none #f)
    (fxlogior         2 fxlogior         #f            72 ,:immortal ,:none #f)
    (fxlogxor         2 fxlogxor         #f            73 ,:immortal ,:none #f)
    (fxlsh            2 fxlsh            #f            74 ,:immortal ,:none #f)
    (fxrsha           2 fxrsha           #f            75 ,:immortal ,:none #f)
    (fxrshl           2 fxrshl           #f            76 ,:immortal ,:none #f)
;   (rot              2 rot              #f            77 ,:immortal ,:none #f)
    (bytevector-ref   2 bytevector-ref   ,fence-imm?    82 ,:string   ,:none #f)
    (procedure-ref    2 procedure-ref    #f            83 ,:dead     ,:none #f)
    (sys$partial-list->vector 2 sys$partial-list->vector #f 90 ,:dead ,:all #f)
    (bytevector-set!  3 bytevector-set!  #f            92 ,:dead     ,:string #f)
    (procedure-set!   3 procedure-set!   #f            93 ,:dead     ,:all  #f)
    (bytevector-like? 1 bytevector-like? #f            94 ,:immortal ,:none #f)
    (vector-like?     1 vector-like?     #f            95 ,:immortal ,:none #f)
    (bytevector-like-ref 2 bytevector-like-ref ,fence-imm?  96 ,:string   ,:none #f)
    (bytevector-like-set! 3 bytevector-like-set! #f    97 ,:dead     ,:string #f)
    (sys$bvlcmp       2 sys$bvlcmp       #f            98 ,:dead     ,:all #f)
    (vector-like-ref  2 vector-like-ref  ,fence-imm?    99 ,:vector   ,:none #f)
    (vector-like-set! 3 vector-like-set! #f           100 ,:dead     ,:vector #f)
    (vector-like-length 1 vector-like-length #f       101 ,:immortal ,:none #f)
    (bytevector-like-length 1 bytevector-like-length #f 102 ,:immortal ,:none #f)
    (gc-counter       0 gc-counter       #f            108 ,:dead    ,:none #f)
    (major-gc-counter 0 major-gc-counter #f            109 ,:dead    ,:none #f)
    (.internal:machine-address 1 machine-address #f    110 ,:dead    ,:none #f)

    (most-positive-fixnum
                      0 most-positive-fixnum
                                         #f            200 ,:immortal ,:none #f)
    (most-negative-fixnum
                      0 most-negative-fixnum
                                         #f            201 ,:immortal ,:none #f)
    (fx+          2 fx+          ,fence-imm?    202 ,:immortal ,:none #f)
    (fx-          2 fx-          ,fence-imm?    203 ,:immortal ,:none #f)
    (fx--         1 fx--         #f            204 ,:immortal ,:none #f)
    (fx*          2 fx*          #f            205 ,:immortal ,:none #f)
    (fx=          2 fx=          ,fence-imm?    206 ,:immortal ,:none #f)
    (fx<          2 fx<          ,fence-imm?    207 ,:immortal ,:none #f)
    (fx<=         2 fx<=         ,fence-imm?    208 ,:immortal ,:none #f)
    (fx>          2 fx>          ,fence-imm?    209 ,:immortal ,:none #f)
    (fx>=         2 fx>=         ,fence-imm?    210 ,:immortal ,:none #f)
    (fxzero?      1 fxzero?      #f            211 ,:immortal ,:none #f)
    (fxpositive?  1 fxpositive?  #f            212 ,:immortal ,:none #f)
    (fxnegative?  1 fxnegative?  #f            213 ,:immortal ,:none #f)
; FIXME: not implemented
;    (fl+          2 +            #f            300 ,:immortal ,:none #f)
;    (fl-          2 -            #f            301 ,:immortal ,:none #f)
;    (fl--         1 --           #f            302 ,:immortal ,:none #f)
;    (fl*          2 *            #f            303 ,:immortal ,:none #f)
;    (fl=          2 =            #f            304 ,:immortal ,:none #f)
;    (fl<          2 <            #f            305 ,:immortal ,:none #f)
;    (fl<=         2 <=           #f            306 ,:immortal ,:none #f)
;    (fl>          2 >            #f            307 ,:immortal ,:none #f)
;    (fl>=         2 >=           #f            308 ,:immortal ,:none #f)

    ; Temporary because they are used for bootstrapping

    ; Added for CSE, representation analysis.

    (vector-length:vec 1 vector-length:vec #f          401 ,:immortal ,:none #f)
    (vector-ref:trusted 2 vector-ref:trusted ,fence-imm? 402 ,:vector   ,:none #f)
    (vector-set!:trusted 3 vector-set!:trusted #f      403 ,:dead     ,:vector #f)
    (car:pair         1 car:pair         #f            404 ,:car      ,:none #f)
    (cdr:pair         1 cdr:pair         #f            405 ,:cdr      ,:none #f)
    (=:fix:fix        2 =:fix:fix        ,fence-imm?    406 ,:immortal ,:none #f)
    (<:fix:fix        2 <:fix:fix        ,fence-imm?    407 ,:immortal ,:none #f)
    (<=:fix:fix       2 <=:fix:fix       ,fence-imm?    408 ,:immortal ,:none #f)
    (>=:fix:fix       2 >=:fix:fix       ,fence-imm?    409 ,:immortal ,:none #f)
    (>:fix:fix        2 >:fix:fix        ,fence-imm?    410 ,:immortal ,:none #f)

    (+:idx:idx        2 +:idx:idx        ,fence-imm?    500 ,:immortal ,:none #f)
    (+:fix:fix        2 +:fix:fix        ,fence-imm?    501 ,:immortal ,:none #t)
    (-:idx:idx        2 -:idx:idx        ,fence-imm?    502 ,:immortal ,:none #f)
    (-:fix:fix        2 -:fix:fix        ,fence-imm?    503 ,:immortal ,:none #t)
    ; FIXME: Not yet implemented in twobit.h, and code generating them
    ; has been commented out in standard-C.imp2.sch
    ;(+:exi:exi        2 +:exi:exi        #f             -1 ,:immortal ,:none #f)
    ;(+:flo:flo        2 +:idx:idx        #f             -1 ,:immortal ,:none #f)
    ;(=:flo:flo        2 =:flo:flo        #f             -1 ,:immortal ,:none #f)
    ;(=:obj:flo        2 =:obj:flo        #f             -1 ,:immortal ,:none #f)
    ;(=:flo:obj        2 =:flo:obj        #f             -1 ,:immortal ,:none #f)

    (--               1 --               #f            32 ,:immortal ,:none #t)
    (unspecified      0 unspecified      #f             3 ,:dead     ,:none #f)
    (undefined        0 undefined        #f             4 ,:dead     ,:none #f)

    ;; For Intel; read reflective registers (perhaps its time for iasn.imp.sch)
    ;; These take a bytevector as argument and write the results into it.
;    (larceny-timestamp! 1 timestamp!     #f          1000 ,:dead     ,:all  #f)
;    (larceny-p-monitor! 1 p-monitor!     #f          1001 ,:dead     ,:all  #f)
    )
  $r5rs-integrable-procedures$)))

; This is actually used by Twobit now.

(define $const/setreg                     ; const/setreg       const,k
  (make-mnemonic 'const/setreg))

; Operations introduced by peephole optimizer.

(define $op1/branchf                      ; op1/branchf        prim,L
  (make-mnemonic 'op1/branchf))
(define $op2/branchf                      ; op2/branchf        prim,k2,L
  (make-mnemonic 'op2/branchf))
(define $op2imm/branchf                   ; op2imm/branchf     prim,x,L
  (make-mnemonic 'op2imm/branchf))
(define $reg/op1/branchf                  ; reg/op1/branchf    prim,k,L
  (make-mnemonic 'reg/op1/branchf))
(define $reg/op2/branchf                  ; reg/op2/branchf    prim,k1,k2,L
  (make-mnemonic 'reg/op2/branchf))
(define $reg/op2imm/branchf               ; reg/op2imm/branchf prim,k1,x,L
  (make-mnemonic 'reg/op2imm/branchf))
(define $reg/op1/check                    ; reg/op1/check      prim,k1,k2,k3,k4,exn
  (make-mnemonic 'reg/op1/check))
(define $reg/op2/check                    ; reg/op2/check      prim,k1,k2,k3,k4,k5,exn
  (make-mnemonic 'reg/op2/check))
(define $reg/op2imm/check                 ; reg/op2imm/check   prim,k1,x,k2,k3,k4,exn
  (make-mnemonic 'reg/op2imm/check))
(define $reg/op1/setreg                   ; reg/op1/setreg     prim,k1,kr
  (make-mnemonic 'reg/op1/setreg))
(define $reg/op2/setreg                   ; reg/op2/setreg     prim,k1,k2,kr
  (make-mnemonic 'reg/op2/setreg))
(define $reg/op2imm/setreg                ; reg/op2imm/setreg  prim,k1,x,kr
  (make-mnemonic 'reg/op2imm/setreg))
(define $setrtn/invoke                    ; setrtn/invoke      align,n
  (make-mnemonic 'setrtn/invoke))
(define $setrtn/branch                    ; setrtn/branch      align,L,doc
  (make-mnemonic 'setrtn/branch))
(define $setrtn/jump                      ; setrtn/jump        align,m,o
  (make-mnemonic 'setrtn/jump))
(define $global/invoke                    ; global/invoke      global,n
  (make-mnemonic 'global/invoke))
(define $global/setrtn/invoke             ; global/setrtn/invoke global,align,n
  (make-mnemonic 'global/setrtn/invoke))
(define $global/setreg                    ; global/setreg      global,k
  (make-mnemonic 'global/setreg))
(define $reg/setglbl                      ; reg/setglbl        k,global
  (make-mnemonic 'reg/setglbl))
(define $const/setglbl                    ; const/setglbl      const,global
  (make-mnemonic 'const/setglbl))
(define $reg/op3                          ; reg/op3            prim,k1,k2,k3
  (make-mnemonic 'reg/op3))
(define $reg/branchf                      ; reg/branchf        k, L
  (make-mnemonic 'reg/branchf))
(define $reg/check                        ; reg/check          k,k1,k2,k3,exn
  (make-mnemonic 'reg/check))
(define $save/stores
  (make-mnemonic 'save/stores))           ; save/store         n,(k1 ...),(n1 ...)

; misc

(define $cons     'cons)
(define $car:pair 'car)
(define $cdr:pair 'cdr)

; eof
