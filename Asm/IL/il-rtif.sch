;; IL Runtime Interface

;; Contains definitions for classes, types, methods, etc used to refer to 
;; the runtime.

;; Load AFTER il-gen.sch

(define il:scheme-assembly-name "Scheme")

(define-syntax define-il-class/type*
  (syntax-rules ()
    ((_ def ...)
     (begin (define-il-class/type . def) ...))))
(define-syntax define-il-class/type
  (syntax-rules ()
    ((_ realm class-id type-id class-string)
     (begin
       (define-il-class class-id realm class-string)
       (define type-id (il-class-type class-id))))
    ((_ realm class-id class-string)
     (define-il-class class-id realm class-string))))
(define-syntax define-il-class 
  (syntax-rules (mac so)
    ((_ class-id mac class-string)
     (define class-id 
       (il-class il:scheme-assembly-name '("Scheme" "RT") class-string)))
    ((_ class-id so class-string)
     (define class-id 
       (il-class il:scheme-assembly-name '("Scheme" "Rep") class-string)))))

(define-il-class/type*
 (mac il-reg          "Reg")
 (mac il-call         "Call")
 (mac il-cont         "Cont")
 (mac il-exn          "Exn")
 (mac il-ops          "Ops")
 (mac il-ops-special  "OpsSpecial")
 (mac il-load         "Load")
 (mac il-instructions "Instructions")

 (so il-schemefactory                    "Factory")
 (so il-schemeobject iltype-schemeobject "SObject")
 (so il-codevector   iltype-codevector   "CodeVector")
 (so il-fixnum       iltype-fixnum       "SFixnum")
 (so il-immediate    iltype-immediate    "SImmediate")
 (so il-schemechar   iltype-schemechar   "SChar")
 (so il-procedure    iltype-procedure    "Procedure")
 (so il-schemepair   iltype-schemepair   "SPair")

 (so il-svl               iltype-svl               "SVL")
 (so il-sbytevl           iltype-sbytevl           "SByteVL")
 (so il-continuation      iltype-continuation      "Continuation")
 
 (so il-loader-thunk iltype-loader-thunk "Twobit_Loader_Thunk")
 (mac il-cache-link         iltype-cache-link         "CacheLink")
 (mac il-cache-frame        iltype-cache-frame        "StackCacheFrame")
 (mac il-continuation-frame iltype-continuation-frame "ContinuationFrame"))

(define (iltype:array iltype) (string-append iltype "[]"))
(define iltype-schemeobject-array (iltype:array iltype-schemeobject))
(define iltype-fixnum-array (iltype:array iltype-fixnum))
(define iltype-schemechar-array (iltype:array iltype-schemechar))
(define iltype-schemepair-array (iltype:array iltype-schemepair))
(define iltype-procedure-array (iltype:array iltype-procedure))

(define iltype-byte "unsigned int8")
(define iltype-char "char")
(define iltype-int16 "int16")
(define iltype-int32 "int32")
(define iltype-int64 "int64")
(define iltype-uint32 "unsigned int32")
(define iltype-uint64 "unsigned int64")
(define iltype-double "float64")
(define iltype-string "string")
(define iltype-void "void")
(define iltype-bool "bool")
(define iltype-byte-array (iltype:array iltype-byte))
(define iltype-string-array (iltype:array iltype-string))
(define iltype-int16-array (iltype:array iltype-int16))

(define iltype-object "object")
(define il-object "[mscorlib]System.Object")

(define (il:make-fixnum)
  (il:call '() iltype-fixnum il-schemefactory "makeFixnum" 
           (list iltype-int32)))
(define (il:make-boolean)
  (il:call '() iltype-immediate il-schemefactory "makeBoolean"
           (list iltype-bool)))
(define (il:make-flonum)
  (il:call '() iltype-sbytevl il-schemefactory "makeFlonum" 
           (list iltype-double)))
(define (il:make-flonum-from-int8)
  (il:call '() iltype-sbytevl il-schemefactory "makeFlonum"
           (list iltype-int64)))
(define (il:make-bignum/ulong/sign)
  (il:call '() iltype-sbytevl il-schemefactory "makeBignum"
           (list iltype-uint64 iltype-bool)))
(define (il:make-bignum/data)
  (il:call '() iltype-sbytevl il-schemefactory "makeBignum"
           (list iltype-int16-array iltype-bool)))

(define (il:make-ratnum)
  (il:call '() iltype-svl il-schemefactory "makeRatnum"
           (list iltype-schemeobject iltype-schemeobject)))
(define (il:make-rectnum)
  (il:call '() iltype-svl il-schemefactory "makeRectnum"
           (list iltype-schemeobject iltype-schemeobject)))
(define (il:make-compnum)
  (il:call '() iltype-sbytevl il-schemefactory "makeCompnum"
           (list iltype-double iltype-double)))
(define (il:make-compnum-from-int8)
  (il:call '() iltype-sbytevl il-schemefactory "makeCompnum"
           (list iltype-int64 iltype-int64)))

(define (il:make-string)
  (il:call '() iltype-sbytevl il-schemefactory "makeString" 
           (list iltype-string)))
(define (il:make-char)
  (il:call '() iltype-schemechar il-schemechar "makeChar" 
           (list iltype-int32)))
(define (il:make-interned-symbol)
  (il:call '() iltype-svl il-schemefactory "makeSymbol" 
           (list iltype-string)))
(define (il:make-pair)
  (il:call '() iltype-schemepair il-schemefactory "makePair"
           (list iltype-schemeobject iltype-schemeobject)))
(define (il:make-pair-reversed)
  (il:call '() iltype-schemepair il-schemefactory "makePairReversed"
           (list iltype-schemeobject iltype-schemeobject)))
(define (il:make-vector)
  (il:call '() iltype-svl il-schemefactory "makeVector" 
           (list iltype-schemeobject-array)))
(define (il:make-constantvector)
  (il:call '(new instance) iltype-void il-constantvector ".ctor" 
           (list iltype-schemepair-array 
                 iltype-constantvector-array
                 iltype-schemeobject-array)))

(define bignum/ulong/sign?
  (let ((2^64 (expt 2 64)))
    (lambda (n)
      (and (number? n) (exact? n) (integer? n) 
           (<= (abs n) (- 2^64 1))))))

(define (bignum? x)
  (and (number? x) (exact? x) (integer? x) (not (fixnum? x))))
