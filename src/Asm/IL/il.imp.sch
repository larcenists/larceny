; Copyright 1991 William Clinger    -*- indent-tabs-mode: nil -*-
;
; $Id$
;
; Larceny -- target-specific information for Twobit's Standard-C backend.

; 2002-11-17 / lth
;
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

; The number of MacScheme machine registers.
; (They do not necessarily correspond to hardware registers.)

(define *nregs* 32)
(define *lastreg* (- *nregs* 1))
(define *fullregs* (quotient *nregs* 2))

; The number of argument registers that are represented by hardware
; registers.

(define *nhwregs* 32)

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
(define (prim-implicit-continuation? entry) (cadddr (cddddr entry)))

(define (op1-primcode name)
  (prim-primcode (prim-entry-by-opcodename name)))

(define op2-primcode op1-primcode)
(define op3-primcode op1-primcode)

(define (op1-implicit-continuation? name)
  (prim-implicit-continuation? (prim-entry-by-opcodename name)))

(define op2-implicit-continuation? op1-implicit-continuation?)
(define op3-implicit-continuation? op1-implicit-continuation?)

(define (op2imm-primcode name)
  (cadr (assq name $immediate-primops$)))

(define (op2imm-implicit-continuation? name)
  (caddr (assq name $immediate-primops$)))

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

(define (stdc-imm? x)
  (and (fixnum? x)
       (<= -65536 x 65535)))

(define (stdc-eq-imm? x)
  (or (stdc-imm? x)
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
  (let ((ak:globals  available:killer:globals)
        (ak:car      available:killer:car)
        (ak:cdr      available:killer:cdr)
        (ak:string   available:killer:string)
        (ak:vector   available:killer:vector)
        (ak:cell     available:killer:cell)
        (ak:io       available:killer:io)
        (ak:none     available:killer:none)     ; none of the above
        (ak:all      available:killer:all)      ; all of the above
        (ak:immortal available:killer:immortal) ; never killed
        (ak:dead     available:killer:dead)     ; never available
        )

;    external     arity  internal    immediate   primcode  killed     kills cont
;    name                name        predicate             by what
;                                                          kind of
;                                                          effect
  `((,name:CAR        1 car              #f            15 ,ak:car      ,ak:none #f)
    (,name:CDR        1 cdr              #f            16 ,ak:cdr      ,ak:none #f)
    (,name:MAKE-CELL  1 make-cell        #f            52 ,ak:dead     ,ak:none #f)
    (,name:CELL-REF   1 cell-ref         #f            54 ,ak:cell     ,ak:none #f)
    (,name:CELL-SET!  2 cell-set!        #f            84 ,ak:dead     ,ak:cell #f)
    (.cell-set!:nwb   2 cell-set!        #f            -1 ,ak:dead     ,ak:cell #f)  ; FIXME
    (,name:CONS       2 cons             #f            58 ,ak:dead     ,ak:none #f)

    (.unspecified     0 unspecified      #f             3 ,ak:dead     ,ak:none #f)
    (.undefined       0 undefined        #f             4 ,ak:dead     ,ak:none #f)
    (.fixnum?         1 fixnum?          #f            23 ,ak:immortal ,ak:none #f)
    (.symbol?         1 symbol?          #f            17 ,ak:immortal ,ak:none #f)
    (.char?           1 char?            #f            36 ,ak:immortal ,ak:none #f)
    (.char->integer   1 char->integer    #f            37 ,ak:immortal ,ak:none #f)
    (.char->integer:chr 1 char->integer  #f            -1 ,ak:immortal ,ak:none #f)
    (.--              1 --               #f            32 ,ak:immortal ,ak:none #t)

    ; FIXME: unspecified, undefined and -- should not be here with these 
    ; names but are introduced by the compiler, macro expander, or standard 
    ; macros.

;   (unspecified      0 unspecified      #f            -1 ,ak:dead     ,ak:none #f)
;   (undefined        0 undefined        #f             4 ,ak:dead     ,ak:none #t)
;   (--               1 --               #f            32 ,ak:immortal ,ak:none #t)

    ; Added for CSE, representation analysis.

    (,name:CHECK!    -1 check!                  #f         -1 ,ak:dead     ,ak:none #f)
    (.vector-length:vec 1 vector-length:vec     #f        401 ,ak:immortal ,ak:none #f)
    (.vector-ref:trusted 2 vector-ref:trusted ,stdc-imm?  402 ,ak:vector   ,ak:none #f)
    (.vector-set!:trusted 3 vector-set!:trusted #f        403 ,ak:dead     ,ak:vector #f)
    (.vector-set!:trusted:nwb 3 vector-set!:trusted #f     -1 ,ak:dead     ,ak:vector #f)   ; FIXME
    (.string-length:str 1 string-length:str     #f         40 ,ak:immortal ,ak:none #f)
    (.string-ref:trusted 2 string-ref:trusted   #f         78 ,ak:string   ,ak:none #f)
    (.string-set!:trusted 3 string-set!:trusted ,stdc-imm? 79 ,ak:dead     ,ak:string #f)

    (.car:pair        1 car:pair         #f           404 ,ak:car      ,ak:none #f)
    (.cdr:pair        1 cdr:pair         #f           405 ,ak:cdr      ,ak:none #f)

    (.+:idx:idx       2 +:idx:idx        ,stdc-imm?   500 ,ak:immortal ,ak:none #f)
    (.+:fix:fix       2 +:fix:fix        #f           501 ,ak:immortal ,ak:none #f)
    (.-:idx:idx       2 -:idx:idx        ,stdc-imm?    -1 ,ak:immortal ,ak:none #f)     ; FIXME
    (.-:fix:fix       2 -:fix:fix        #f            -1 ,ak:immortal ,ak:none #f)     ; FIXME

    (.=:fix:fix       2 =:fix:fix        ,stdc-imm?   406 ,ak:immortal ,ak:none #f)
    (.<:fix:fix       2 <:fix:fix        ,stdc-imm?   407 ,ak:immortal ,ak:none #f)
    (.<=:fix:fix      2 <=:fix:fix       ,stdc-imm?   408 ,ak:immortal ,ak:none #f)
    (.>=:fix:fix      2 >=:fix:fix       ,stdc-imm?   409 ,ak:immortal ,ak:none #f)
    (.>:fix:fix       2 >:fix:fix        ,stdc-imm?   410 ,ak:immortal ,ak:none #f)

    ; Not yet implemented.

;    (.+:flo:flo       2 +:flo:flo        #f            -1 ,ak:immortal ,ak:none)
;    (.-:flo:flo       2 -:flo:flo        #f            -1 ,ak:immortal ,ak:none)
;    (.*:flo:flo       2 *:flo:flo        #f            -1 ,ak:immortal ,ak:none)
;    (./:flo:flo       2 /:flo:flo        #f            -1 ,ak:immortal ,ak:none)

;    (.=:flo:flo       2 =:flo:flo        #f            -1 ,ak:immortal ,ak:none)
;    (.=:obj:flo       2 =:obj:flo        #f            -1 ,ak:immortal ,ak:none)
;    (.=:flo:obj       2 =:flo:obj        #f            -1 ,ak:immortal ,ak:none)
;    (.<:flo:flo       2 =:flo:flo        #f            -1 ,ak:immortal ,ak:none)
;    (.<=:flo:flo      2 =:flo:flo        #f            -1 ,ak:immortal ,ak:none)
;    (.>:flo:flo       2 =:flo:flo        #f            -1 ,ak:immortal ,ak:none)
;    (.>=:flo:flo      2 =:flo:flo        #f            -1 ,ak:immortal ,ak:none)
    )))

(define $r4rs-integrable-procedures$
  (let ((ak:globals  available:killer:globals)
        (ak:car      available:killer:car)
        (ak:cdr      available:killer:cdr)
        (ak:string   available:killer:string)
        (ak:vector   available:killer:vector)
        (ak:cell     available:killer:cell)
        (ak:io       available:killer:io)
        (ak:none     available:killer:none)     ; none of the above
        (ak:all      available:killer:all)      ; all of the above
        (ak:immortal available:killer:immortal) ; never killed
        (ak:dead     available:killer:dead)     ; never available
        )

;    external     arity  internal    immediate   primcode  killed     kills cont
;    name                name        predicate             by what
;                                                          kind of
;                                                          effect
 (append
  `((eof-object       0 eof-object       #f             5 ,ak:dead     ,ak:none #f)
    (not              1 not              #f             9 ,ak:immortal ,ak:none #f)
    (null?            1 null?            #f            10 ,ak:immortal ,ak:none #f)
    (pair?            1 pair?            #f            11 ,ak:immortal ,ak:none #f)
    (eof-object?      1 eof-object?      #f            12 ,ak:immortal ,ak:none #f)
    (car              1 car              #f            15 ,ak:car      ,ak:none #f)
    (cdr              1 cdr              #f            16 ,ak:cdr      ,ak:none #f)
    (symbol?          1 symbol?          #f            17 ,ak:immortal ,ak:none #f)
    (number?          1 complex?         #f            18 ,ak:immortal ,ak:none #f)
    (complex?         1 complex?         #f            18 ,ak:immortal ,ak:none #f)
    (real?            1 rational?        #f            20 ,ak:immortal ,ak:none #f)
    (rational?        1 rational?        #f            20 ,ak:immortal ,ak:none #f)
    (integer?         1 integer?         #f            22 ,ak:immortal ,ak:none #f)
    (exact?           1 exact?           #f            25 ,ak:immortal ,ak:none #f)
    (inexact?         1 inexact?         #f            26 ,ak:immortal ,ak:none #f)
    (exact->inexact   1 exact->inexact   #f            27 ,ak:immortal ,ak:none #t)
    (inexact->exact   1 inexact->exact   #f            28 ,ak:immortal ,ak:none #t)
    (round            1 round            #f            29 ,ak:immortal ,ak:none #t)
    (truncate         1 truncate         #f            30 ,ak:immortal ,ak:none #t)
    (zero?            1 zero?            #f            31 ,ak:immortal ,ak:none #t)
    (real-part        1 real-part        #f            34 ,ak:immortal ,ak:none #f)
    (imag-part        1 imag-part        #f            35 ,ak:immortal ,ak:none #f)
    (char?            1 char?            #f            36 ,ak:immortal ,ak:none #f)
    (char->integer    1 char->integer    #f            37 ,ak:immortal ,ak:none #f)
    (integer->char    1 integer->char    #f            38 ,ak:immortal ,ak:none #f)
    (string?          1 string?          #f            39 ,ak:immortal ,ak:none #f)
    (string-length    1 string-length    #f            40 ,ak:immortal ,ak:none #f)
    (ustring?         1 ustring?         #f          8039 ,ak:immortal ,ak:none #f)
    (.ustring-length:str 1 ustring-length:str  #f    8040 ,ak:immortal ,ak:none #f)
    (vector?          1 vector?          #f            41 ,ak:immortal ,ak:none #f)
    (vector-length    1 vector-length    #f            42 ,ak:immortal ,ak:none #f)
    (procedure?       1 procedure?       #f            47 ,ak:immortal ,ak:none #f)
    (eq?              2 eq?           ,stdc-eq-imm?    56 ,ak:immortal ,ak:none #f)
    (eqv?             2 eqv?             #f            57 ,ak:immortal ,ak:none #t)
    (cons             2 cons             #f            58 ,ak:dead     ,ak:none #f)
    (set-car!         2 set-car!         #f            59 ,ak:dead     ,ak:car  #f)
    (set-cdr!         2 set-cdr!         #f            60 ,ak:dead     ,ak:cdr  #f)
    (+                2 +                ,stdc-imm?    61 ,ak:immortal ,ak:none #t)
    (-                2 -                ,stdc-imm?    62 ,ak:immortal ,ak:none #t)
    (*                2 *                ,stdc-imm?    63 ,ak:immortal ,ak:none #t)
;    (*                2 *                #f            63 ,ak:immortal ,ak:none #t)
    (/                2 /                #f            64 ,ak:immortal ,ak:none #t)
    (quotient         2 quotient         #f            65 ,ak:immortal ,ak:none #t)
    (<                2 <                ,stdc-imm?    66 ,ak:immortal ,ak:none #t)
    (<=               2 <=               ,stdc-imm?    67 ,ak:immortal ,ak:none #t)
    (=                2 =                ,stdc-imm?    68 ,ak:immortal ,ak:none #t)
    (>                2 >                ,stdc-imm?    69 ,ak:immortal ,ak:none #t)
    (>=               2 >=               ,stdc-imm?    70 ,ak:immortal ,ak:none #t)
    (make-string      2 make-string      #f           109 ,ak:dead     ,ak:none #f)
    (string-ref       2 string-ref       ,stdc-imm?    78 ,ak:string   ,ak:none #f)
    (string-set!      3 string-set!      ,stdc-imm?    79 ,ak:dead     ,ak:string #f)
    (make-ustring     2 make-ustring     #f          8109 ,ak:dead     ,ak:none #f)
    (.ustring-ref:trusted  2 ustring-ref:trusted  #f 8078 ,ak:string   ,ak:none #f)
    (.ustring-set!:trusted 3 ustring-set!:trusted #f 8079 ,ak:dead     ,ak:string #f)
    (make-vector      2 make-vector      #f            80 ,ak:dead     ,ak:none #f)
    (vector-ref       2 vector-ref       ,stdc-imm?    81 ,ak:vector   ,ak:none #f)
    (char<?           2 char<?           ,char?        85 ,ak:immortal ,ak:none #f)
    (char<=?          2 char<=?          ,char?        86 ,ak:immortal ,ak:none #f)
    (char=?           2 char=?           ,char?        87 ,ak:immortal ,ak:none #f)
    (char>?           2 char>?           ,char?        88 ,ak:immortal ,ak:none #f)
    (char>=?          2 char>=?          ,char?        89 ,ak:immortal ,ak:none #f)
    (vector-set!      3 vector-set!      #f            91 ,ak:dead     ,ak:vector #f)
    (remainder        2 remainder        #f           103 ,ak:immortal ,ak:none #t))
  $minimal-integrable-procedures$)))

(define $r5rs-integrable-procedures$
  $r4rs-integrable-procedures$)

(define $usual-integrable-procedures$
  (let ((ak:globals  available:killer:globals)
        (ak:car      available:killer:car)
        (ak:cdr      available:killer:cdr)
        (ak:string   available:killer:string)
        (ak:vector   available:killer:vector)
        (ak:cell     available:killer:cell)
        (ak:io       available:killer:io)
        (ak:none     available:killer:none)     ; none of the above
        (ak:all      available:killer:all)      ; all of the above
        (ak:immortal available:killer:immortal) ; never killed
        (ak:dead     available:killer:dead)     ; never available
        )

;    external     arity  internal    immediate   primcode  killed    kills cont
;    name                name        predicate             by what
;                                                          kind of
;                                                          effect

  (append
  `(
    (larceny-break    0 break            #f             1 ,ak:dead     ,ak:all  #f)
    (.creg            0 creg             #f           106 ,ak:dead     ,ak:all  #f)
    (enable-interrupts 1 enable-interrupts #f           6 ,ak:dead     ,ak:all  #t)
    (disable-interrupts 0 disable-interrupts #f         7 ,ak:dead     ,ak:all  #t)
    (typetag          1 typetag          #f             8 ,ak:dead     ,ak:none #f)
    (port?            1 port?            #f            13 ,ak:dead     ,ak:none #f)
    (structure?       1 structure?       #f            14 ,ak:dead     ,ak:none #f)
    (fixnum?          1 fixnum?          #f            23 ,ak:immortal ,ak:none #f)
    (flonum?          1 flonum?          #f            24 ,ak:immortal ,ak:none #f)
    (compnum?         1 compnum?         #f            21 ,ak:immortal ,ak:none #f)
    (fxlognot         1 fxlognot         #f            33 ,ak:immortal ,ak:none #f)
    (bytevector?      1 bytevector?      #f            43 ,ak:immortal ,ak:none #f)
    (bytevector-length 1 bytevector-length #f          44 ,ak:immortal ,ak:none #f)
    (bytevector-fill! 2 bytevector-fill! #f            45 ,ak:dead     ,ak:string #f)
    (make-bytevector  1 make-bytevector  #f            46 ,ak:dead     ,ak:none #f)
    (procedure-length 1 procedure-length #f            48 ,ak:dead     ,ak:none #f)
    (make-procedure   1 make-procedure   #f            49 ,ak:dead     ,ak:none #f)
    (.creg-set!       1 creg-set!        #f           107 ,ak:dead     ,ak:none #f)
    (typetag-set!     2 typetag-set! ,valid-typetag?   55 ,ak:dead     ,ak:all #f)
    (fxlogand         2 fxlogand         #f            71 ,ak:immortal ,ak:none #f)
    (fxlogior         2 fxlogior         #f            72 ,ak:immortal ,ak:none #f)
    (fxlogxor         2 fxlogxor         #f            73 ,ak:immortal ,ak:none #f)
    (fxlsh            2 fxlsh            #f            74 ,ak:immortal ,ak:none #f)
    (fxrsha           2 fxrsha           #f            75 ,ak:immortal ,ak:none #f)
    (fxrshl           2 fxrshl           #f            76 ,ak:immortal ,ak:none #f)
    (rot              2 rot              #f            77 ,ak:immortal ,ak:none #f)
    (bytevector-ref   2 bytevector-ref   ,stdc-imm?    82 ,ak:string   ,ak:none #f)
    (procedure-ref    2 procedure-ref    #f            83 ,ak:dead     ,ak:none #f)
    (sys$partial-list->vector 2 sys$partial-list->vector #f 90 ,ak:dead ,ak:all #f)
    (bytevector-set!  3 bytevector-set!  #f            92 ,ak:dead     ,ak:string #f)
    (procedure-set!   3 procedure-set!   #f            93 ,ak:dead     ,ak:all  #f)
    (bytevector-like? 1 bytevector-like? #f            94 ,ak:immortal ,ak:none #f)
    (vector-like?     1 vector-like?     #f            95 ,ak:immortal ,ak:none #f)
    (bytevector-like-ref 2 bytevector-like-ref ,stdc-imm?  96 ,ak:string   ,ak:none #f)
    (bytevector-like-set! 3 bytevector-like-set! #f    97 ,ak:dead     ,ak:string #f)
    (sys$bvlcmp       2 sys$bvlcmp       #f            98 ,ak:dead     ,ak:all #f)
    (vector-like-ref  2 vector-like-ref  ,stdc-imm?    99 ,ak:vector   ,ak:none #f)
    (vector-like-set! 3 vector-like-set! #f           100 ,ak:dead     ,ak:vector #f)
    (vector-like-length 1 vector-like-length #f       101 ,ak:immortal ,ak:none #f)
    (bytevector-like-length 1 bytevector-like-length #f 102 ,ak:immortal ,ak:none #f)
    (#f               1 petit-patch-boot-code #f       104 #f         #f     #f)
    (#f               1 syscall          #f            105 #f         #f     #t)
    (gc-counter       0 gc-counter       #f            108 ,ak:dead     ,ak:none #f)

    (most-positive-fixnum
                      0 most-positive-fixnum
                                         #f            200 ,ak:immortal ,ak:none #f)
    (most-negative-fixnum
                      0 most-negative-fixnum
                                         #f            201 ,ak:immortal ,ak:none #f)
    (fx+          2 fx+          ,stdc-imm?    202 ,ak:immortal ,ak:none #f)
    (fx-          2 fx-          ,stdc-imm?    203 ,ak:immortal ,ak:none #f)
    (fx--         1 fx--         #f            204 ,ak:immortal ,ak:none #f)
    (fx*          2 fx*          #f            205 ,ak:immortal ,ak:none #f)
    (fx=          2 fx=          ,stdc-imm?    206 ,ak:immortal ,ak:none #f)
    (fx<          2 fx<          ,stdc-imm?    207 ,ak:immortal ,ak:none #f)
    (fx<=         2 fx<=         ,stdc-imm?    208 ,ak:immortal ,ak:none #f)
    (fx>          2 fx>          ,stdc-imm?    209 ,ak:immortal ,ak:none #f)
    (fx>=         2 fx>=         ,stdc-imm?    210 ,ak:immortal ,ak:none #f)
    (fxzero?      1 fxzero?      #f            211 ,ak:immortal ,ak:none #f)
    (fxpositive?  1 fxpositive?  #f            212 ,ak:immortal ,ak:none #f)
    (fxnegative?  1 fxnegative?  #f            213 ,ak:immortal ,ak:none #f)
; FIXME: not implemented
;    (fl+          2 +            #f            300 ,ak:immortal ,ak:none #f)
;    (fl-          2 -            #f            301 ,ak:immortal ,ak:none #f)
;    (fl--         1 --           #f            302 ,ak:immortal ,ak:none #f)
;    (fl*          2 *            #f            303 ,ak:immortal ,ak:none #f)
;    (fl=          2 =            #f            304 ,ak:immortal ,ak:none #f)
;    (fl<          2 <            #f            305 ,ak:immortal ,ak:none #f)
;    (fl<=         2 <=           #f            306 ,ak:immortal ,ak:none #f)
;    (fl>          2 >            #f            307 ,ak:immortal ,ak:none #f)
;    (fl>=         2 >=           #f            308 ,ak:immortal ,ak:none #f)

    ; Temporary because they are used for bootstrapping

    ; Added for CSE, representation analysis.

    (vector-length:vec 1 vector-length:vec #f          401 ,ak:immortal ,ak:none #f)
    (vector-ref:trusted 2 vector-ref:trusted ,stdc-imm? 402 ,ak:vector   ,ak:none #f)
    (vector-set!:trusted 3 vector-set!:trusted #f      403 ,ak:dead     ,ak:vector #f)
    (car:pair         1 car:pair         #f            404 ,ak:car      ,ak:none #f)
    (cdr:pair         1 cdr:pair         #f            405 ,ak:cdr      ,ak:none #f)
    (=:fix:fix        2 =:fix:fix        ,stdc-imm?    406 ,ak:immortal ,ak:none #f)
    (<:fix:fix        2 <:fix:fix        ,stdc-imm?    407 ,ak:immortal ,ak:none #f)
    (<=:fix:fix       2 <=:fix:fix       ,stdc-imm?    408 ,ak:immortal ,ak:none #f)
    (>=:fix:fix       2 >=:fix:fix       ,stdc-imm?    409 ,ak:immortal ,ak:none #f)
    (>:fix:fix        2 >:fix:fix        ,stdc-imm?    410 ,ak:immortal ,ak:none #f)

    ; FIXME: Not yet implemented in twobit.h
    (+:idx:idx        2 +:idx:idx        #f            500 ,ak:immortal ,ak:none #f)
    (+:fix:fix        2 +:idx:idx        #f            501 ,ak:immortal ,ak:none #f)
    (+:exi:exi        2 +:idx:idx        #f            502 ,ak:immortal ,ak:none #f)
    (+:flo:flo        2 +:idx:idx        #f            503 ,ak:immortal ,ak:none #f)
    (=:flo:flo        2 =:flo:flo        #f            504 ,ak:immortal ,ak:none #f)
    (=:obj:flo        2 =:obj:flo        #f            505 ,ak:immortal ,ak:none #f)
    (=:flo:obj        2 =:flo:obj        #f            506 ,ak:immortal ,ak:none #f)

    ; Introduced by peephole optimization
    ; External name, immediate predicate, killed, and kills are not used for these.

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
    (#f               2 internal:check-<=:fix:fix/imm #f 659_         _ #f)
    (#f               2 internal:check->=:fix:fix/imm #f 660 _        _ #f)
    (#f               2 internal:check-range #f        661 _          _ #f)
    (#f               2 internal:check-vector?/vector-length:vec #f 662 _ _ #f)
    (#f               2 internal:check-string?/string-length:str #f 663 _ _ #f)
    (--               1 --               #f            32 ,ak:immortal ,ak:none #t)
    (unspecified      0 unspecified      #f             3 ,ak:dead     ,ak:none #f)
    (undefined        0 undefined        #f             4 ,ak:dead     ,ak:none #f)

    )
  $r5rs-integrable-procedures$)))

(define $immediate-primops$
  '((typetag-set! 128 #f)
    (eq? 129 #f)
    (+ 130 #t)
    (- 131 #t)
    (< 132 #t)
    (<= 133 #t)
    (= 134 #t)
    (> 135 #t)
    (>= 136 #t)
    (char<? 137 #f)
    (char<=? 138 #f)
    (char=? 139 #f)
    (char>? 140 #f)
    (char>=? 141 #f)
    (string-ref 142 #f)
    (vector-ref 143 #f)
    (bytevector-ref 144 #f)
    (bytevector-like-ref 145 #f)
    (vector-like-ref 146 #f)
    (fx+ 250 #f)
    (fx- 251 #f)
    (fx= 253 #f)
    (fx< 254 #f)
    (fx<= 255 #f)
    (fx> 256 #f)
    (fx>= 257 #f)
    (vector-ref:trusted 450 #f)
    (=:fix:fix 451 #f)
    (<:fix:fix 452 #f)
    (<=:fix:fix 453 #f)
    (>:fix:fix 454 #f)
    (>=:fix:fix 455 #f)
    (internal:branchf-</imm 635 #t)
    (internal:branchf->/imm 636 #t)
    (internal:branchf->=/imm 637 #t)
    (internal:branchf-<=/imm 638 #t)
    (internal:branchf-=/imm 639 #t)
    (internal:branchf-eq?/imm 640 #f)
    (internal:branchf-char=?/imm 641 #f)
    (internal:branchf-char>=?/imm 642 #f)
    (internal:branchf-char>?/imm 643 #f)
    (internal:branchf-char<=?/imm 644 #f)
    (internal:branchf-char<?/imm 645 #f)
    (internal:branchf-fx=/imm 646 #f)
    (internal:branchf-fx>/imm 647 #f)
    (internal:branchf-fx>=/imm 648 #f)
    (internal:branchf-fx</imm 649 #f)
    (internal:branchf-fx<=/imm 650 #f)
    (internal:check-<:fix:fix/imm 658 #f)
    (internal:check-<=:fix:fix/imm 659 #f)
    (internal:check->=:fix:fix/imm 660 #f)
    ))

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
(define $reg/op1/check                    ; reg/op1/check      prim,k1,k2,k3,k4,exn
  (make-mnemonic 'reg/op1/check))
(define $reg/op2/check                    ; reg/op2/check      prim,k1,k2,k3,k4,k5,exn
  (make-mnemonic 'reg/op2/check))
(define $reg/op2imm/check                 ; reg/op2imm/check   prim,k1,x,k2,k3,k4,exn
  (make-mnemonic 'reg/op2imm/check))

; misc

(define $cons     'cons)
(define $car:pair 'car)
(define $cdr:pair 'cdr)

; eof
