; Copyright 1991 William Clinger
;
; $Id$
;
; Larceny -- target-specific information for Twobit's SPARC backend.
;
; 11 June 1999 / wdc

; The maximum number of fixed arguments that may be followed by a rest
; argument.  This limitation is removed by the macro expander.

(define @maxargs-with-rest-arg@ 30)

; The number of MacScheme machine registers.
; (They do not necessarily correspond to hardware registers.)

(define *nregs* 32)
(define *lastreg* (- *nregs* 1))
(define *fullregs* (quotient *nregs* 2))

; The number of argument registers that are represented by hardware
; registers.

(define *nhwregs* 8)

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
;    procedure name
;    arity (or -1 for special primops like .check!)
;    procedure name to be used by the disassembler
;    predicate for immediate operands (or #f)
;    primop code in the MacScheme machine (not used by Larceny)
;    the effects that kill this primop's result
;    the effects of this primop that kill available expressions

(define (prim-entry name)
  (assq name (twobit-integrable-procedures)))

(define prim-arity cadr)
(define prim-opcodename caddr)
(define prim-immediate? cadddr)
(define (prim-primcode entry)
  (car (cddddr entry)))

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

(define (sparc-imm? x)
  (and (fixnum? x)
       (<= -1024 x 1023)))

(define (sparc-eq-imm? x)
  (or (sparc-imm? x)
      (eq? x #t)
      (eq? x #f)
      (eq? x '())))

(define (valid-typetag? x)
  (and (fixnum? x)
       (<= 0 x 7)))

(define (fixnum-primitives) #t)
(define (flonum-primitives) #t)

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

;    external     arity  internal    immediate    ignored  killed     kills
;    name                name        predicate             by what
;                                                          kind of
;                                                          effect
  `((,name:CAR        1 car              #f          #x1b ,:car      ,:none)
    (,name:CDR        1 cdr              #f          #x1c ,:cdr      ,:none)
    (,name:MAKE-CELL  1 make-cell        #f          #x7e ,:dead     ,:none)
    (,name:CELL-REF   1 cell-ref         #f          #x7f ,:cell     ,:none)
    (,name:CELL-SET!  2 cell-set!        #f          #xdf ,:dead     ,:cell)
    (.cell-set!:nwb   2 cell-set!:nwb    #f            -1 ,:dead     ,:cell)
    (,name:CONS       2 cons             #f          #xa8 ,:dead     ,:none)

    (.unspecified     0 unspecified      #f            -1 ,:dead     ,:none)
    (.undefined       0 undefined        #f             8 ,:dead     ,:none)
    (.fixnum?         1 fixnum?          #f          #x23 ,:immortal ,:none)
    (.symbol?         1 symbol?          #f          #x1f ,:immortal ,:none)
    (.char?           1 char?            #f          #x40 ,:immortal ,:none)
    (.char->integer   1 char->integer    #f          #x41 ,:immortal ,:none)
    (.char->integer:chr 1 char->integer  #f          #x41 ,:immortal ,:none)
    (.--              1 --               #f          #x2d ,:immortal ,:none)

    ; Added for CSE, representation analysis.

    (,name:CHECK!    -1 check!           #f            -1 ,:dead     ,:none)
    (.vector-length:vec 1 vector-length:vec #f          -1 ,:immortal ,:none)
    (.vector-ref:trusted 2 vector-ref:trusted ,sparc-imm? -1 ,:vector ,:none)
    (.vector-set!:trusted 3 vector-set!:trusted #f     -1 ,:dead     ,:vector)
    (.vector-set!:trusted:nwb 3 vector-set!:trusted:nwb #f -1 ,:dead ,:vector)
    (.string-length:str 1 string-length:str #f          -1 ,:immortal ,:none)
    (.string-ref:trusted 2 string-ref:trusted #f       -1 ,:string   ,:none)
    (.string-set!:trusted 3 string-set!:trusted #f     -1 ,:dead     ,:string)

    (.car:pair        1 car:pair         #f            -1 ,:car      ,:none)
    (.cdr:pair        1 cdr:pair         #f            -1 ,:cdr      ,:none)

    (.+:idx:idx       2 +:idx:idx        ,sparc-imm?   -1 ,:immortal ,:none)
    (.+:fix:fix       2 +                #f            -1 ,:immortal ,:none)
    (.-:idx:idx       2 -:idx:idx        ,sparc-imm?   -1 ,:immortal ,:none)
    (.-:fix:fix       2 -                #f            -1 ,:immortal ,:none)

    (.=:fix:fix       2 =:fix:fix        ,sparc-imm?   -1 ,:immortal ,:none)
    (.<:fix:fix       2 <:fix:fix        ,sparc-imm?   -1 ,:immortal ,:none)
    (.<=:fix:fix      2 <=:fix:fix       ,sparc-imm?   -1 ,:immortal ,:none)
    (.>=:fix:fix      2 >=:fix:fix       ,sparc-imm?   -1 ,:immortal ,:none)
    (.>:fix:fix       2 >:fix:fix        ,sparc-imm?   -1 ,:immortal ,:none)
    
    ; Not yet implemented.

;    (.+:flo:flo       2 +:flo:flo        #f            -1 ,:immortal ,:none)
;    (.-:flo:flo       2 -:flo:flo        #f            -1 ,:immortal ,:none)
;    (.*:flo:flo       2 *:flo:flo        #f            -1 ,:immortal ,:none)
;    (./:flo:flo       2 /:flo:flo        #f            -1 ,:immortal ,:none)

;    (.=:flo:flo       2 =:flo:flo        #f            -1 ,:immortal ,:none)
;   ;(.=:obj:flo       2 =:obj:flo        #f            -1 ,:immortal ,:none)
;   ;(.=:flo:obj       2 =:flo:obj        #f            -1 ,:immortal ,:none)
;    (.<:flo:flo       2 =:flo:flo        #f            -1 ,:immortal ,:none)
;    (.<=:flo:flo      2 =:flo:flo        #f            -1 ,:immortal ,:none)
;    (.>:flo:flo       2 =:flo:flo        #f            -1 ,:immortal ,:none)
;    (.>=:flo:flo      2 =:flo:flo        #f            -1 ,:immortal ,:none)
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

;    external     arity  internal    immediate    ignored  killed     kills
;    name                name        predicate             by what
;                                                          kind of
;                                                          effect
 (append
  `((eof-object       0 eof-object       #f            -1 ,:dead     ,:none)
    (not              1 not              #f          #x18 ,:immortal ,:none)
    (null?            1 null?            #f          #x19 ,:immortal ,:none)
    (pair?            1 pair?            #f          #x1a ,:immortal ,:none)
    (eof-object?      1 eof-object?      #f            -1 ,:immortal ,:none)
    (port?            1 port?            #f            -1 ,:dead     ,:none)
    (car              1 car              #f          #x1b ,:car      ,:none)
    (cdr              1 cdr              #f          #x1c ,:cdr      ,:none)
    (symbol?          1 symbol?          #f          #x1f ,:immortal ,:none)
    (number?          1 complex?         #f          #x20 ,:immortal ,:none)
    (complex?         1 complex?         #f          #x20 ,:immortal ,:none)
    (real?            1 rational?        #f          #x21 ,:immortal ,:none)
    (rational?        1 rational?        #f          #x21 ,:immortal ,:none)
    (integer?         1 integer?         #f          #x22 ,:immortal ,:none)
    (exact?           1 exact?           #f          #x24 ,:immortal ,:none)
    (inexact?         1 inexact?         #f          #x25 ,:immortal ,:none)
    (exact->inexact   1 exact->inexact   #f          #x26 ,:immortal ,:none)
    (inexact->exact   1 inexact->exact   #f          #x27 ,:immortal ,:none)
    (round            1 round            #f          #x28 ,:immortal ,:none)
    (truncate         1 truncate         #f          #x29 ,:immortal ,:none)
    (zero?            1 zero?            #f          #x2c ,:immortal ,:none)
    (real-part        1 real-part        #f          #x3e ,:immortal ,:none)
    (imag-part        1 imag-part        #f          #x3f ,:immortal ,:none)
    (char?            1 char?            #f          #x40 ,:immortal ,:none)
    (char->integer    1 char->integer    #f          #x41 ,:immortal ,:none)
    (integer->char    1 integer->char    #f          #x42 ,:immortal ,:none)
    (string?          1 string?          #f          #x50 ,:immortal ,:none)
    (string-length    1 string-length    #f          #x51 ,:immortal ,:none)
    (vector?          1 vector?          #f          #x52 ,:immortal ,:none)
    (vector-length    1 vector-length    #f          #x53 ,:immortal ,:none)
    (procedure?       1 procedure?       #f          #x58 ,:immortal ,:none)
    (eq?              2 eq?           ,sparc-eq-imm? #xa1 ,:immortal ,:none)
    (eqv?             2 eqv?             #f          #xa2 ,:immortal ,:none)
    (cons             2 cons             #f          #xa8 ,:dead     ,:none)
    (set-car!         2 set-car!         #f          #xa9 ,:dead     ,:car)
    (set-cdr!         2 set-cdr!         #f          #xaa ,:dead     ,:cdr)
    (+                2 +                ,sparc-imm? #xb0 ,:immortal ,:none)
    (-                2 -                ,sparc-imm? #xb1 ,:immortal ,:none)
    (*                2 *                ,sparc-imm? #xb2 ,:immortal ,:none)
    (/                2 /                #f          #xb3 ,:immortal ,:none)
    (quotient         2 quotient         #f          #xb4 ,:immortal ,:none)
    (<                2 <                ,sparc-imm? #xb5 ,:immortal ,:none)
    (<=               2 <=               ,sparc-imm? #xb6 ,:immortal ,:none)
    (=                2 =                ,sparc-imm? #xb7 ,:immortal ,:none)
    (>                2 >                ,sparc-imm? #xb8 ,:immortal ,:none)
    (>=               2 >=               ,sparc-imm? #xb9 ,:immortal ,:none)
    (make-string      2 make-string      #f            -1 ,:dead     ,:none)
    (string-ref       2 string-ref       ,sparc-imm? #xd1 ,:string   ,:none)
    (string-set!      3 string-set!      #f            -1 ,:dead     ,:string)
    (make-vector      2 make-vector      #f          #xd2 ,:dead     ,:none)
    (vector-ref       2 vector-ref       ,sparc-imm? #xd3 ,:vector   ,:none)
    (char<?           2 char<?           ,char?      #xe0 ,:immortal ,:none)
    (char<=?          2 char<=?          ,char?      #xe1 ,:immortal ,:none)
    (char=?           2 char=?           ,char?      #xe2 ,:immortal ,:none)
    (char>?           2 char>?           ,char?      #xe3 ,:immortal ,:none)
    (char>=?          2 char>=?          ,char?      #xe4 ,:immortal ,:none)
    (vector-set!      3 vector-set!      #f          #xf1 ,:dead     ,:vector)
    (remainder        2 remainder        #f            -1 ,:immortal ,:none))
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

;    external     arity  internal    immediate    ignored  killed     kills
;    name                name        predicate             by what
;                                                          kind of
;                                                          effect

 (append
  `((larceny-break    0 break            #f             3 ,:dead     ,:all)
    (creg             0 creg             #f             7 ,:dead     ,:all)
    (enable-interrupts 1 enable-interrupts #f          -1 ,:dead     ,:all)
    (disable-interrupts 0 disable-interrupts #f        -1 ,:dead     ,:all)

    (unspecified      0 unspecified      #f            -1 ,:dead     ,:none)
    (undefined        0 undefined        #f             8 ,:dead     ,:none)
    (--               1 --               #f          #x2d ,:immortal ,:none)

    (typetag          1 typetag          #f          #x11 ,:dead     ,:none)
    (structure?       1 structure?       #f            -1 ,:dead     ,:none)
    (fixnum?          1 fixnum?          #f          #x23 ,:immortal ,:none)
    (flonum?          1 flonum?          #f            -1 ,:immortal ,:none)
    (compnum?         1 compnum?         #f            -1 ,:immortal ,:none)
    (fxlognot         1 fxlognot         #f          #x2f ,:immortal ,:none)
    (bytevector?      1 bytevector?      #f          #x54 ,:immortal ,:none)
    (bytevector-length 1 bytevector-length #f        #x55 ,:immortal ,:none)
    (bytevector-fill! 2 bytevector-fill! #f            -1 ,:dead     ,:string)
    (make-bytevector  1 make-bytevector  #f          #x56 ,:dead     ,:none)
    (procedure-length 1 procedure-length #f          #x59 ,:dead     ,:none)
    (make-procedure   1 make-procedure   #f          #x5a ,:dead     ,:none)
    (creg-set!        1 creg-set!        #f          #x71 ,:dead     ,:none)
    (typetag-set!     2 typetag-set! ,valid-typetag? #xa0 ,:dead     ,:all)
    (fxlogand         2 fxlogand         #f          #xc0 ,:immortal ,:none)
    (fxlogior         2 fxlogior         #f          #xc1 ,:immortal ,:none)
    (fxlogxor         2 fxlogxor         #f          #xc2 ,:immortal ,:none)
    (fxlsh            2 fxlsh            #f          #xc3 ,:immortal ,:none)
    (fxrsha           2 fxrsha           #f            -1 ,:immortal ,:none)
    (fxrshl           2 fxrshl           #f            -1 ,:immortal ,:none)
    (rot              2 rot              #f          #xc4 ,:immortal ,:none)
    (bytevector-ref   2 bytevector-ref   ,sparc-imm? #xd5 ,:string   ,:none)
    (procedure-ref    2 procedure-ref    #f          #xd7 ,:dead     ,:none)
    
    (sys$partial-list->vector 2 sys$partial-list->vector #f -1 ,:dead ,:all)
    (bytevector-set!  3 bytevector-set!  #f          #xf2 ,:dead     ,:string)
    (procedure-set!   3 procedure-set!   #f          #xf3 ,:dead     ,:all)
    (bytevector-like? 1 bytevector-like? #f            -1 ,:immortal ,:none)
    (vector-like?     1 vector-like?     #f            -1 ,:immortal ,:none)
    (bytevector-like-ref 2 bytevector-like-ref #f      -1 ,:string   ,:none)
    (bytevector-like-set! 3 bytevector-like-set! #f    -1 ,:dead     ,:string)
    (sys$bvlcmp       2 sys$bvlcmp       #f            -1 ,:dead     ,:all)
    (vector-like-ref  2 vector-like-ref  #f            -1 ,:vector   ,:none)
    (vector-like-set! 3 vector-like-set! #f            -1 ,:dead     ,:vector)
    (vector-like-length 1 vector-like-length #f        -1 ,:immortal ,:none)
    (bytevector-like-length 1 bytevector-like-length #f -1 ,:immortal ,:none)
    (sys$read-char    1 sys$read-char    #f            -1 ,:dead     ,:io)
    (gc-counter       0 gc-counter       #f            -1 ,:dead     ,:none)
    ,@(if (fixnum-primitives)
	  `((most-positive-fixnum
                          0 most-positive-fixnum
                                         #f            -1 ,:immortal ,:none)
	    (most-negative-fixnum
                          0 most-negative-fixnum
                                         #f            -1 ,:immortal ,:none)
	    (fx+          2 fx+          ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx-          2 fx-          ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx--         1 fx--         #f            -1 ,:immortal ,:none)
	    (fx*          2 fx*          #f            -1 ,:immortal ,:none)
	    (fx=          2 fx=          ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx<          2 fx<          ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx<=         2 fx<=         ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx>          2 fx>          ,sparc-imm?   -1 ,:immortal ,:none)
	    (fx>=         2 fx>=         ,sparc-imm?   -1 ,:immortal ,:none)
	    (fxzero?      1 fxzero?      #f            -1 ,:immortal ,:none)
	    (fxpositive?  1 fxpositive?  #f            -1 ,:immortal ,:none)
	    (fxnegative?  1 fxnegative?  #f            -1 ,:immortal ,:none))
	  '())
    ,@(if (flonum-primitives)
          `((fl+          2 +            #f            -1 ,:immortal ,:none)
	    (fl-          2 -            #f            -1 ,:immortal ,:none)
	    (fl--         1 --           #f            -1 ,:immortal ,:none)
	    (fl*          2 *            #f            -1 ,:immortal ,:none)
	    (fl=          2 =            #f            -1 ,:immortal ,:none)
	    (fl<          2 <            #f            -1 ,:immortal ,:none)
	    (fl<=         2 <=           #f            -1 ,:immortal ,:none)
	    (fl>          2 >            #f            -1 ,:immortal ,:none)
	    (fl>=         2 >=           #f            -1 ,:immortal ,:none))
          '())

    )
  $r5rs-integrable-procedures$)))

; Not used by the Sparc assembler; for information only.

(define $immediate-primops$
  '((typetag-set! #x80)
    (eq? #x81)
    (+ #x82)
    (- #x83)
    (< #x84)
    (<= #x85)
    (= #x86)
    (> #x87)
    (>= #x88)
    (char<? #x89)
    (char<=? #x8a)
    (char=? #x8b)
    (char>? #x8c)
    (char>=? #x8d)
    (string-ref #x90)
    (vector-ref #x91)
    (bytevector-ref #x92)
    (bytevector-like-ref -1)
    (vector-like-ref -1)
    (fx+ -1)
    (fx- -1)
    (fx-- -1)
    (fx= -1)
    (fx< -1)
    (fx<= -1)
    (fx> -1)
    (fx>= -1)))

; Operations introduced by peephole optimizer.

(define $reg/op1/branchf                  ; reg/op1/branchf    prim,k1,L
  (make-mnemonic 'reg/op1/branchf))
(define $reg/op2/branchf                  ; reg/op2/branchf    prim,k1,k2,L
  (make-mnemonic 'reg/op2/branchf))
(define $reg/op2imm/branchf               ; reg/op2imm/branchf prim,k1,x,L
  (make-mnemonic 'reg/op2imm/branchf))
(define $reg/op1/check             ; reg/op1/check      prim,k1,k2,k3,k4,exn
  (make-mnemonic 'reg/op1/check))
(define $reg/op2/check             ; reg/op2/check      prim,k1,k2,k3,k4,k5,exn
  (make-mnemonic 'reg/op2/check))
(define $reg/op2imm/check          ; reg/op2imm/check   prim,k1,x,k2,k3,k4,exn
  (make-mnemonic 'reg/op2imm/check))
(define $reg/op1/setreg                   ; reg/op1/setreg     prim,k1,kr
  (make-mnemonic 'reg/op1/setreg))
(define $reg/op2/setreg                   ; reg/op2/setreg     prim,k1,k2,kr
  (make-mnemonic 'reg/op2/setreg))
(define $reg/op2imm/setreg                ; reg/op2imm/setreg  prim,k1,x,kr
  (make-mnemonic 'reg/op2imm/setreg))
(define $reg/branchf                      ; reg/branchf        k, L
  (make-mnemonic 'reg/branchf))
(define $reg/return                       ; reg/return         k
  (make-mnemonic 'reg/return))
(define $reg/setglbl                      ; reg/setglbl        k,x
  (make-mnemonic 'reg/setglbl))
(define $reg/op3                          ; reg/op3            prim,k1,k2,k3
  (make-mnemonic 'reg/op3))
(define $const/setreg                     ; const/setreg       const,k
  (make-mnemonic 'const/setreg))
(define $const/return                     ; const/return       const
  (make-mnemonic 'const/return))
(define $global/setreg                    ; global/setreg      x,k
  (make-mnemonic 'global/setreg))
(define $setrtn/branch                    ; setrtn/branch      L,doc
  (make-mnemonic 'setrtn/branch))
(define $setrtn/invoke                    ; setrtn/invoke      L
  (make-mnemonic 'setrtn/invoke))
(define $global/invoke                    ; global/invoke      global,n
  (make-mnemonic 'global/invoke))

; misc

(define $cons     'cons)
(define $car:pair 'car)
(define $cdr:pair 'cdr)

; eof
