; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; The interpreter's top-level environment -- the common bits.

($$trace "toplevel")

(define (init-toplevel-environment)
  (let* ((null    (initialize-null-environment-target-specific
		   (initialize-null-environment
		    (make-environment "null-environment"))))
         (r4rs    (initialize-r4rs-environment-target-specific
		   (initialize-r4rs-environment
		    (environment-copy null "report-environment-4"))))
         (r5rs    (initialize-r5rs-environment-target-specific
		   (initialize-r5rs-environment
		    (environment-copy r4rs "report-environment-5"))))
         (larceny (initialize-larceny-environment-target-specific
		   (initialize-larceny-environment
		    (environment-copy r5rs "larceny-environment")))))
    (install-environments! null r4rs r5rs larceny)
    (unspecified)))

(define (initialize-null-environment null)

  ;; macros

  (environment-set-macro! null 'quote (usual-syntax 'quote))
  (environment-set-macro! null 'lambda (usual-syntax 'lambda))
  (environment-set-macro! null 'if (usual-syntax 'if))
  (environment-set-macro! null 'set! (usual-syntax 'set!))
  (environment-set-macro! null 'begin (usual-syntax 'begin))
  (environment-set-macro! null 'define (usual-syntax 'define))
  (environment-set-macro! null 'define-syntax (usual-syntax 'define-syntax))
  (environment-set-macro! null 'let-syntax (usual-syntax 'let-syntax))
  (environment-set-macro! null 'letrec-syntax (usual-syntax 'letrec-syntax))
  (environment-set-macro! null 'let*-syntax (usual-syntax 'let*-syntax))
  (environment-set-macro! null 'let (usual-syntax 'let))
  (environment-set-macro! null 'let* (usual-syntax 'let*))
  (environment-set-macro! null 'letrec (usual-syntax 'letrec))
  (environment-set-macro! null 'and (usual-syntax 'and))
  (environment-set-macro! null 'or (usual-syntax 'or))
  (environment-set-macro! null 'cond (usual-syntax 'cond))
  (environment-set-macro! null 'do (usual-syntax 'do))
  (environment-set-macro! null 'case (usual-syntax 'case))
  (environment-set-macro! null 'delay (usual-syntax 'delay))
  (environment-set-macro! null 'quasiquote (usual-syntax 'quasiquote))
  (environment-set-macro! null 'syntax-rules (usual-syntax 'syntax-rules))

  ;; for macro expansion

  (environment-set-macro! null name:CALL (usual-syntax name:CALL))

  ;; support procedures for compiled code and macro expansion

  ; Some of these are that are introduced by the compiler.  The
  ; compiler should always open-codes them, and it should be possible
  ; to remove them from here.
  ;
  ; The rest are introduced by the macro expander; the compiler will
  ; open-code them as well, but the interpreter will not, so the
  ; definitions must be here.

  (environment-set! null 'car:pair car:pair)
  (environment-set! null 'cdr:pair cdr:pair)
  (environment-set! null '=:fix:fix  =:fix:fix)
  (environment-set! null '<:fix:fix  <:fix:fix)
  (environment-set! null '<=:fix:fix <=:fix:fix)
  (environment-set! null '>:fix:fix  >:fix:fix)
  (environment-set! null '>=:fix:fix >=:fix:fix)
  (environment-set! null 'vector-length:vec vector-length:vec)
  (environment-set! null 'vector-ref:trusted vector-ref:trusted)
  (environment-set! null 'vector-set!:trusted vector-set!:trusted)
  (environment-set! null '.list .list)
  (environment-set! null '.list->vector .list->vector)
  (environment-set! null '.cons .cons)
  (environment-set! null '.car .car)
  (environment-set! null '.cdr .cdr)
  (environment-set! null '.append .append)
  (environment-set! null '.make-promise .make-promise)
  (environment-set! null '.make-cell .make-cell)
  (environment-set! null '.cell-ref .cell-ref)
  (environment-set! null '.cell-set! .cell-set!)
  (environment-set! null '.check! .check!)
  (environment-set! null '.unspecified unspecified)
  (environment-set! null '.undefined undefined)

  null)

; The input environment should be an initialized null environment.

(define (initialize-r4rs-environment r4rs)

  ;; booleans

  (environment-set! r4rs 'not not)
  (environment-set! r4rs 'boolean? boolean?)

  ;; equivalence predicates

  (environment-set! r4rs 'eq? eq?)
  (environment-set! r4rs 'eqv? eqv?)
  (environment-set! r4rs 'equal? equal?)

  ;; pairs and lists

  (environment-set! r4rs 'pair? pair?)
  (environment-set! r4rs 'cons cons)
  (environment-set! r4rs 'car car)
  (environment-set! r4rs 'cdr cdr)
  (environment-set! r4rs 'set-car! set-car!)
  (environment-set! r4rs 'set-cdr! set-cdr!)
  (environment-set! r4rs 'caar caar)
  (environment-set! r4rs 'cadr cadr)
  (environment-set! r4rs 'cdar cdar)
  (environment-set! r4rs 'cddr cddr)
  (environment-set! r4rs 'caaar caaar)
  (environment-set! r4rs 'caadr caadr)
  (environment-set! r4rs 'cadar cadar)
  (environment-set! r4rs 'caddr caddr)
  (environment-set! r4rs 'cdaar cdaar)
  (environment-set! r4rs 'cdadr cdadr)
  (environment-set! r4rs 'cddar cddar)
  (environment-set! r4rs 'cdddr cdddr)
  (environment-set! r4rs 'caaaar caaaar)
  (environment-set! r4rs 'caaadr caaadr)
  (environment-set! r4rs 'caadar caadar)
  (environment-set! r4rs 'caaddr caaddr)
  (environment-set! r4rs 'cadaar cadaar)
  (environment-set! r4rs 'cadadr cadadr)
  (environment-set! r4rs 'caddar caddar)
  (environment-set! r4rs 'cadddr cadddr)
  (environment-set! r4rs 'cdaaar cdaaar)
  (environment-set! r4rs 'cdaadr cdaadr)
  (environment-set! r4rs 'cdadar cdadar)
  (environment-set! r4rs 'cdaddr cdaddr)
  (environment-set! r4rs 'cddaar cddaar)
  (environment-set! r4rs 'cddadr cddadr)
  (environment-set! r4rs 'cdddar cdddar)
  (environment-set! r4rs 'cddddr cddddr)
  (environment-set! r4rs 'null? null?)
  (environment-set! r4rs 'list? list?)
  (environment-set! r4rs 'list list)
  (environment-set! r4rs 'length length)
  (environment-set! r4rs 'append append)
  (environment-set! r4rs 'reverse reverse)
  (environment-set! r4rs 'list-tail list-tail)
  (environment-set! r4rs 'list-ref list-ref)
  (environment-set! r4rs 'memq memq)
  (environment-set! r4rs 'memv memv)
  (environment-set! r4rs 'member member)
  (environment-set! r4rs 'assq assq)
  (environment-set! r4rs 'assv assv)
  (environment-set! r4rs 'assoc assoc)

  ;; symbols

  (environment-set! r4rs 'symbol? symbol?)
  (environment-set! r4rs 'symbol->string symbol->string)
  (environment-set! r4rs 'string->symbol string->symbol)

  ;; numbers

  (environment-set! r4rs 'number? number?)
  (environment-set! r4rs 'complex? complex?)
  (environment-set! r4rs 'real? real?)
  (environment-set! r4rs 'rational? rational?)
  (environment-set! r4rs 'integer? integer?)
  (environment-set! r4rs 'exact? exact?)
  (environment-set! r4rs 'inexact? inexact?)
  (environment-set! r4rs '= =)
  (environment-set! r4rs '< <)
  (environment-set! r4rs '> >)
  (environment-set! r4rs '<= <=)
  (environment-set! r4rs '>= >=)
  (environment-set! r4rs 'zero? zero?)
  (environment-set! r4rs 'positive? positive?)
  (environment-set! r4rs 'negative? negative?)
  (environment-set! r4rs 'odd? odd?)
  (environment-set! r4rs 'even? even?)
  (environment-set! r4rs 'max max)
  (environment-set! r4rs 'min min)
  (environment-set! r4rs '+ +)
  (environment-set! r4rs '* *)
  (environment-set! r4rs '- -)
  (environment-set! r4rs '/ /)
  (environment-set! r4rs 'abs abs)
  (environment-set! r4rs 'quotient quotient)
  (environment-set! r4rs 'remainder remainder)
  (environment-set! r4rs 'modulo modulo)
  (environment-set! r4rs 'gcd gcd)
  (environment-set! r4rs 'lcm lcm)
  (environment-set! r4rs 'numerator numerator)
  (environment-set! r4rs 'denominator denominator)
  (environment-set! r4rs 'floor floor)
  (environment-set! r4rs 'ceiling ceiling)
  (environment-set! r4rs 'truncate truncate)
  (environment-set! r4rs 'round round)
  (environment-set! r4rs 'rationalize rationalize)
  (environment-set! r4rs 'exp exp)
  (environment-set! r4rs 'log log)
  (environment-set! r4rs 'sin sin)
  (environment-set! r4rs 'cos cos)
  (environment-set! r4rs 'tan tan)
  (environment-set! r4rs 'asin asin)
  (environment-set! r4rs 'acos acos)
  (environment-set! r4rs 'atan atan)
  (environment-set! r4rs 'sqrt sqrt)
  (environment-set! r4rs 'expt expt)
  (environment-set! r4rs 'make-rectangular make-rectangular)
  (environment-set! r4rs 'real-part real-part)
  (environment-set! r4rs 'imag-part imag-part)
  (environment-set! r4rs 'make-polar make-polar)
  (environment-set! r4rs 'magnitude magnitude)
  (environment-set! r4rs 'angle angle)
  (environment-set! r4rs 'exact->inexact exact->inexact)
  (environment-set! r4rs 'inexact->exact inexact->exact)
  (environment-set! r4rs 'number->string number->string)
  (environment-set! r4rs 'string->number string->number)

  ;; Characters

  (environment-set! r4rs 'char? char?)
  (environment-set! r4rs 'char=? char=?)
  (environment-set! r4rs 'char<? char<?)
  (environment-set! r4rs 'char>? char>?)
  (environment-set! r4rs 'char<=? char<=?)
  (environment-set! r4rs 'char>=? char>=?)
  (environment-set! r4rs 'char-ci=? char-ci=?)
  (environment-set! r4rs 'char-ci<? char-ci<?)
  (environment-set! r4rs 'char-ci>? char-ci>?)
  (environment-set! r4rs 'char-ci<=? char-ci<=?)
  (environment-set! r4rs 'char-ci>=? char-ci>=?)
  (environment-set! r4rs 'char-alphabetic? char-alphabetic?)
  (environment-set! r4rs 'char-numeric? char-numeric?)
  (environment-set! r4rs 'char-whitespace? char-whitespace?)
  (environment-set! r4rs 'char-upper-case? char-upper-case?)
  (environment-set! r4rs 'char-lower-case? char-lower-case?)
  (environment-set! r4rs 'char->integer char->integer)
  (environment-set! r4rs 'integer->char integer->char)
  (environment-set! r4rs 'char-upcase char-upcase)
  (environment-set! r4rs 'char-downcase char-downcase)

  ;; strings

  (environment-set! r4rs 'string? string?)
  (environment-set! r4rs 'make-string make-string)
  (environment-set! r4rs 'string string)
  (environment-set! r4rs 'string-length string-length)
  (environment-set! r4rs 'string-ref string-ref)
  (environment-set! r4rs 'string-set! string-set!)
  (environment-set! r4rs 'string=? string=?)
  (environment-set! r4rs 'string<? string<?)
  (environment-set! r4rs 'string>? string>?)
  (environment-set! r4rs 'string<=? string<=?)
  (environment-set! r4rs 'string>=? string>=?)
  (environment-set! r4rs 'string-ci=? string-ci=?)
  (environment-set! r4rs 'string-ci<? string-ci<?)
  (environment-set! r4rs 'string-ci>? string-ci>?)
  (environment-set! r4rs 'string-ci<=? string-ci<=?)
  (environment-set! r4rs 'string-ci>=? string-ci>=?)
  (environment-set! r4rs 'substring substring)
  (environment-set! r4rs 'string-append string-append)
  (environment-set! r4rs 'string->list string->list)
  (environment-set! r4rs 'list->string list->string)
  (environment-set! r4rs 'string-copy string-copy)
  (environment-set! r4rs 'string-fill! string-fill!)

  ;; vectors

  (environment-set! r4rs 'vector? vector?)
  (environment-set! r4rs 'make-vector make-vector)
  (environment-set! r4rs 'vector vector)
  (environment-set! r4rs 'vector-length vector-length)
  (environment-set! r4rs 'vector-ref vector-ref)
  (environment-set! r4rs 'vector-set! vector-set!)
  (environment-set! r4rs 'vector->list vector->list)
  (environment-set! r4rs 'list->vector list->vector)
  (environment-set! r4rs 'vector-fill! vector-fill!)

  ;; control features

  (environment-set! r4rs 'procedure? procedure?)
  (environment-set! r4rs 'apply apply)
  (environment-set! r4rs 'map map)
  (environment-set! r4rs 'for-each for-each)
  (environment-set! r4rs 'force force)
  (environment-set! r4rs 'call-with-current-continuation
                    call-with-current-continuation)

  ;; i/o

  (environment-set! r4rs 'call-with-input-file call-with-input-file)
  (environment-set! r4rs 'call-with-output-file call-with-output-file)
  (environment-set! r4rs 'input-port? input-port?)
  (environment-set! r4rs 'output-port? output-port?)
  (environment-set! r4rs 'current-input-port current-input-port)
  (environment-set! r4rs 'current-output-port current-output-port)
  (environment-set! r4rs 'with-input-from-file with-input-from-file)
  (environment-set! r4rs 'with-output-to-file with-output-to-file)
  (environment-set! r4rs 'open-input-file open-input-file)
  (environment-set! r4rs 'open-output-file open-output-file)
  (environment-set! r4rs 'close-input-port close-input-port)
  (environment-set! r4rs 'close-output-port close-output-port)
  (environment-set! r4rs 'read read)
  (environment-set! r4rs 'read-char read-char)
  (environment-set! r4rs 'peek-char peek-char)
  (environment-set! r4rs 'eof-object? eof-object?)
  (environment-set! r4rs 'char-ready? char-ready?)
  (environment-set! r4rs 'write write)
  (environment-set! r4rs 'display display)
  (environment-set! r4rs 'newline newline)
  (environment-set! r4rs 'write-char write-char)

  ;; general

  (environment-set! r4rs 'load load)
  ; transcript-on and transcript-off are not in the primitive heap.

  r4rs)

; The input should be an initialized r4rs-environment

(define (initialize-r5rs-environment r5rs)

  ;; environments

  (environment-set! r5rs 'interaction-environment interaction-environment)
  (environment-set! r5rs 'scheme-report-environment
                    scheme-report-environment)
  (environment-set! r5rs 'null-environment null-environment)

  ;; control features

  (environment-set! r5rs 'dynamic-wind dynamic-wind)
  (environment-set! r5rs 'eval eval)
  (environment-set! r5rs 'values values)
  (environment-set! r5rs 'call-with-values call-with-values)

  r5rs)

; The input should be an initialized r5rs environment

(define (initialize-larceny-environment larc)

  ;; syntax

  (environment-set-macro! larc 'parameterize (usual-syntax 'parameterize))
  (environment-set-macro! larc 'let-values (usual-syntax 'let-values))
  (environment-set-macro! larc 'let*-values (usual-syntax 'let*-values))

  ;; JavaDot and case-sensitivity
  (environment-set-macro! larc '.javadot (usual-syntax '.javadot))
  
  (environment-set! larc 'case-sensitive? case-sensitive?)
  (environment-set! larc
                    'recognize-javadot-symbols?
                    recognize-javadot-symbols?)
  (environment-set! larc 'javadot-symbol? javadot-symbol?)
  (environment-set! larc 'javadot-symbol->symbol javadot-symbol->symbol)
  (environment-set! larc 'symbol->javadot-symbol symbol->javadot-symbol)

  
  ;; pairs and lists

  (environment-set! larc 'list-copy list-copy)
  (environment-set! larc 'last-pair last-pair)
  (environment-set! larc 'remq remq)
  (environment-set! larc 'remv remv)
  (environment-set! larc 'remove remove)
  (environment-set! larc 'remq! remq!)
  (environment-set! larc 'remv! remv!)
  (environment-set! larc 'remove! remove!)
  (environment-set! larc 'append! append!)
  (environment-set! larc 'reverse! reverse!)
  (environment-set! larc 'some? some?)
  (environment-set! larc 'every? every?)

  ;; hash functions and hashtables

  (environment-set! larc 'object-hash object-hash)
  (environment-set! larc 'equal-hash equal-hash)
  (environment-set! larc 'make-hashtable make-hashtable)
  (environment-set! larc 'hashtable-contains? hashtable-contains?)
  (environment-set! larc 'hashtable-fetch hashtable-fetch)
  (environment-set! larc 'hashtable-get hashtable-get)
  (environment-set! larc 'hashtable-put! hashtable-put!)
  (environment-set! larc 'hashtable-remove! hashtable-remove!)
  (environment-set! larc 'hashtable-clear! hashtable-clear!)
  (environment-set! larc 'hashtable-size hashtable-size)
  (environment-set! larc 'hashtable-for-each hashtable-for-each)
  (environment-set! larc 'hashtable-map hashtable-map)
  (environment-set! larc 'hashtable-copy hashtable-copy)

  ;; symbols

  (environment-set! larc 'symbol-hash symbol-hash)
  (environment-set! larc 'gensym gensym)
  (environment-set! larc 'oblist oblist)
  (environment-set! larc 'oblist-set! oblist-set!)

  ;; environments

  (environment-set! larc 'make-environment make-environment)
  (environment-set! larc 'environment? environment?)
  (environment-set! larc 'environment-name environment-name)
  (environment-set! larc 'environment-variables environment-variables)
  (environment-set! larc 'environment-variable? environment-variable?)
  (environment-set! larc 'environment-get environment-get)
  (environment-set! larc 'environment-get-cell environment-get-cell)
  (environment-set! larc 'environment-set! environment-set!)
  (environment-set! larc 'environment-syntax-environment 
                    environment-syntax-environment)
  (environment-set! larc 'environment-copy environment-copy)
  (environment-set! larc 'environment-macros environment-macros)
  (environment-set! larc 'environment-get-macro environment-get-macro)
  (environment-set! larc 'environment-set-macro! environment-set-macro!)
  (environment-set! larc 'environment-macro? environment-macro?)

  ;; numbers

  (environment-set! larc 'random random)
  (environment-set! larc 'fixnum? fixnum?)
  (environment-set! larc 'bignum? bignum?)
  (environment-set! larc 'ratnum? ratnum?)
  (environment-set! larc 'flonum? flonum?)
  (environment-set! larc 'compnum? compnum?)
  (environment-set! larc 'rectnum? rectnum?)
  (environment-set! larc 'logand logand)
  (environment-set! larc 'logior logior)
  (environment-set! larc 'logxor logxor)
  (environment-set! larc 'lognot lognot)
  (environment-set! larc 'rsha rsha)
  (environment-set! larc 'rshl rshl)
  (environment-set! larc 'lsh lsh)
  (environment-set! larc 'most-positive-fixnum most-positive-fixnum)
  (environment-set! larc 'most-negative-fixnum most-negative-fixnum)

  ;; strings

  (environment-set! larc 'string-hash string-hash)
  (environment-set! larc 'substring-fill! substring-fill!)
  (environment-set! larc 'string-downcase! string-downcase!)
  (environment-set! larc 'string-upcase! string-upcase!)
  (environment-set! larc 'string-downcase string-downcase)
  (environment-set! larc 'string-upcase string-upcase)

  ;; vectors

  (environment-set! larc 'vector-copy vector-copy)

  ;; control features

  (environment-set! larc 'syscall syscall)
  (environment-set! larc 'make-trampoline make-trampoline)
  (environment-set! larc 'procedure-copy procedure-copy)
  (environment-set! larc 'evaluator evaluator)
  (environment-set! larc 'procedure-length procedure-length)
  (environment-set! larc 'procedure-ref procedure-ref)
  (environment-set! larc 'procedure-set! procedure-set!)
  (environment-set! larc 'make-procedure make-procedure)
  (environment-set! larc 'enable-interrupts enable-interrupts)
  (environment-set! larc 'disable-interrupts disable-interrupts)
  (environment-set! larc 'timer-interrupt-handler timer-interrupt-handler)
  (environment-set! larc 'keyboard-interrupt-handler 
                    keyboard-interrupt-handler)
  (environment-set! larc 'break-handler break-handler)
  (environment-set! larc 'call-without-interrupts call-without-interrupts)
  (environment-set! larc 'standard-timeslice standard-timeslice)
  (environment-set! larc 'procedure-arity procedure-arity)
  (environment-set! larc 'procedure-documentation procedure-documentation)
  (environment-set! larc 'procedure-documentation-string 
                    procedure-documentation-string)
  (environment-set! larc 'procedure-name procedure-name)
  (environment-set! larc 'procedure-source-file procedure-source-file)
  (environment-set! larc 'procedure-source-position 
                    procedure-source-position)
  (environment-set! larc 'procedure-expression procedure-expression)
  (environment-set! larc 'procedure-environment procedure-environment)
  (environment-set! larc 'interpreted-procedure? interpreted-procedure?)
  (environment-set! larc 'interpreted-expression? interpreted-expression?)
  (environment-set! larc 'interpreted-primitive? interpreted-primitive?)
  (environment-set! larc 'interpreted-expression-source 
                    interpreted-expression-source)

  ;; io

  (environment-set! larc 'open-binary-input-file open-binary-input-file)
  (environment-set! larc 'open-binary-output-file open-binary-output-file)
  (environment-set! larc 'call-with-binary-input-file 
                    call-with-binary-input-file)
  (environment-set! larc 'call-with-binary-output-file 
                    call-with-binary-output-file)
  (environment-set! larc 'with-input-from-binary-file
                    with-input-from-binary-file)
  (environment-set! larc 'with-output-to-binary-file 
                    with-output-to-binary-file)
  (environment-set! larc 'open-input-string open-input-string)
  (environment-set! larc 'open-output-string open-output-string)
  (environment-set! larc 'get-output-string get-output-string)
  (environment-set! larc 'reset-output-string reset-output-string)
  (environment-set! larc 'format format)
  (environment-set! larc 'port? port?)
  (environment-set! larc 'port-name port-name)
  (environment-set! larc 'flush-output-port flush-output-port)
  (environment-set! larc 'eof-object eof-object)
  (environment-set! larc 'delete-file delete-file)
  (environment-set! larc 'rename-file rename-file)
  (environment-set! larc 'file-exists? file-exists?)
  (environment-set! larc 'file-modification-time file-modification-time)
  (environment-set! larc 'close-open-files close-open-files)
  (environment-set! larc 'with-input-from-port with-input-from-port)
  (environment-set! larc 'with-output-to-port with-output-to-port)
  (environment-set! larc 'write-bytevector-like write-bytevector-like)
  (environment-set! larc 'lowlevel-write lowlevel-write)
  (environment-set! larc 'port-position port-position)
  (environment-set! larc 'readtable-ref readtable-ref)
  (environment-set! larc 'readtable-set! readtable-set!)
  (environment-set! larc 'console-input-port console-input-port)
  (environment-set! larc 'console-output-port console-output-port)
  (environment-set! larc 'console-input-port-factory 
                    console-input-port-factory)
  (environment-set! larc 'console-output-port-factory
                    console-output-port-factory)
  (environment-set! larc 'print-length print-length)
  (environment-set! larc 'print-level print-level)

  ;; common and less common extensions to R4RS

  (environment-set! larc 'error error)
  (environment-set! larc 'error-handler error-handler)
  (environment-set! larc 'decode-error decode-error)
  (environment-set! larc 'call-with-error-handler call-with-error-handler)
  (environment-set! larc 'call-without-errors call-without-errors)
  (environment-set! larc 'reset reset)
  (environment-set! larc 'reset-handler reset-handler)
  (environment-set! larc 'call-with-reset-handler call-with-reset-handler)
  (environment-set! larc 'exit exit)
  (environment-set! larc 'add-exit-procedure! add-exit-procedure!)
  (environment-set! larc 'add-init-procedure! add-init-procedure!)
  (environment-set! larc 'quit quit)
  (environment-set! larc 'quit-handler quit-handler)
  (environment-set! larc 'sort sort)
  (environment-set! larc 'sort! sort!)
  (environment-set! larc 'make-parameter make-parameter)

  ;; debugging.

  (environment-set! larc 'current-continuation-structure 
                    current-continuation-structure)
  (environment-set! larc 'break break)

  ;; property lists

  (environment-set! larc 'getprop getprop)
  (environment-set! larc 'putprop putprop)
  (environment-set! larc 'remprop remprop)

  ;; vector-like

  (environment-set! larc 'vector-like? vector-like?)
  (environment-set! larc 'vector-like-length vector-like-length)
  (environment-set! larc 'vector-like-ref vector-like-ref)
  (environment-set! larc 'vector-like-set! vector-like-set!)

  ;; bytevectors

  (environment-set! larc 'bytevector? bytevector?)
  (environment-set! larc 'bytevector-length bytevector-length)
  (environment-set! larc 'make-bytevector make-bytevector)
  (environment-set! larc 'bytevector-ref bytevector-ref)
  (environment-set! larc 'bytevector-set! bytevector-set!)
  (environment-set! larc 'bytevector-equal? bytevector-equal?)
  (environment-set! larc 'bytevector-fill! bytevector-fill!)
  (environment-set! larc 'bytevector-copy bytevector-copy)

  (environment-set! larc 'bytevector-like? bytevector-like?)
  (environment-set! larc 'bytevector-like-length bytevector-like-length)
  (environment-set! larc 'bytevector-like-ref bytevector-like-ref)
  (environment-set! larc 'bytevector-like-set! bytevector-like-set!)
  (environment-set! larc 'bytevector-like-equal? bytevector-like-equal?)
  (environment-set! larc 'bytevector-like-copy bytevector-like-copy)

  ;; structures

  (environment-set! larc 'make-structure make-structure)
  (environment-set! larc 'structure? structure?)
  (environment-set! larc 'structure-printer structure-printer)
  (environment-set! larc 'structure-comparator structure-comparator)

  ;; system performance and interface

  (environment-set! larc 'collect collect)
  (environment-set! larc 'gcctl gcctl)
  (environment-set! larc 'sro sro)
  (environment-set! larc 'system-features system-features)
  (environment-set! larc 'stats-dump-on stats-dump-on)
  (environment-set! larc 'stats-dump-off stats-dump-off)
  (environment-set! larc 'stats-dump-stdout stats-dump-stdout)
  (environment-set! larc 'system-function system-function)
  (environment-set! larc 'gc-counter gc-counter)
  (environment-set! larc 'run-with-stats run-with-stats)
  (environment-set! larc 'run-benchmark run-benchmark)
  (environment-set! larc 'display-memstats display-memstats)
  (environment-set! larc 'memstats memstats)
  (environment-set! larc 'memstats-allocated memstats-allocated)
  (environment-set! larc 'memstats-gc-reclaimed memstats-gc-reclaimed)
  (environment-set! larc 'memstats-gc-copied memstats-gc-copied)
  (environment-set! larc 'memstats-gc-total-elapsed-time 
                    memstats-gc-total-elapsed-time)
  (environment-set! larc 'memstats-gc-total-cpu-time
                    memstats-gc-total-cpu-time)
  (environment-set! larc 'memstats-gc-promotion-elapsed-time 
                    memstats-gc-promotion-elapsed-time)
  (environment-set! larc 'memstats-gc-promotion-cpu-time
                    memstats-gc-promotion-cpu-time)
  (environment-set! larc 'memstats-heap-allocated-now 
                    memstats-heap-allocated-now)
  (environment-set! larc 'memstats-heap-allocated-max 
                    memstats-heap-allocated-max)
  (environment-set! larc 'memstats-heap-live-now memstats-heap-live-now)
  (environment-set! larc 'memstats-remsets-allocated-now 
                    memstats-remsets-allocated-now)
  (environment-set! larc 'memstats-remsets-allocated-max 
                    memstats-remsets-allocated-max)
  (environment-set! larc 'memstats-rts-allocated-now 
                    memstats-rts-allocated-now)
  (environment-set! larc 'memstats-rts-allocated-max 
                    memstats-rts-allocated-max)
  (environment-set! larc 'memstats-heap-fragmentation-now 
                    memstats-heap-fragmentation-now)
  (environment-set! larc 'memstats-heap-fragmentation-max 
                    memstats-heap-fragmentation-max)
  (environment-set! larc 'memstats-mem-allocated-now 
                    memstats-mem-allocated-now)
  (environment-set! larc 'memstats-mem-allocated-max
                    memstats-mem-allocated-max)
  (environment-set! larc 'memstats-generations memstats-generations)
  (environment-set! larc 'memstats-remsets memstats-remsets)
  (environment-set! larc 'memstats-frames-flushed memstats-frames-flushed)
  (environment-set! larc 'memstats-words-flushed memstats-words-flushed)
  (environment-set! larc 'memstats-stacks-created memstats-stacks-created)
  (environment-set! larc 'memstats-frames-restored memstats-frames-restored)
  (environment-set! larc 'memstats-swb-total-assignments 
                    memstats-swb-total-assignments)
  (environment-set! larc 'memstats-swb-vector-assignments 
                    memstats-swb-vector-assignments)
  (environment-set! larc 'memstats-swb-lhs-young-or-remembered 
                    memstats-swb-lhs-young-or-remembered)
  (environment-set! larc 'memstats-swb-rhs-immediate 
                    memstats-swb-rhs-immediate)
  (environment-set! larc 'memstats-swb-not-intergenerational 
                    memstats-swb-not-intergenerational)
  (environment-set! larc 'memstats-swb-transactions 
                    memstats-swb-transactions)
  (environment-set! larc 'memstats-elapsed-time memstats-elapsed-time)
  (environment-set! larc 'memstats-system-time memstats-system-time)
  (environment-set! larc 'memstats-user-time memstats-user-time)
  (environment-set! larc 'memstats-minor-faults memstats-minor-faults)
  (environment-set! larc 'memstats-major-faults memstats-major-faults)
  (environment-set! larc 'memstats-fullgc-collections 
                    memstats-fullgc-collections)
  (environment-set! larc 'memstats-fullgc-elapsed-time 
                    memstats-fullgc-elapsed-time)
  (environment-set! larc 'memstats-fullgc-cpu-time memstats-fullgc-cpu-time)
  (environment-set! larc 'memstats-fullgc-copied memstats-fullgc-copied)
  (environment-set! larc 'memstats-fullgc-moved memstats-fullgc-moved)
  (environment-set! larc 'memstats-fullgc-marked memstats-fullgc-marked)
  (environment-set! larc 'memstats-fullgc-traced memstats-fullgc-traced)
  (environment-set! larc 'memstats-dofgc-resets memstats-dofgc-resets)
  (environment-set! larc 'memstats-dofgc-repeats memstats-dofgc-repeats)
  (environment-set! larc 'memstats-gc-accounting memstats-gc-accounting)
  (environment-set! larc 'memstats-acc-gc memstats-acc-gc)
  (environment-set! larc 'memstats-acc-promotion memstats-acc-promotion)
  (environment-set! larc 'memstats-acc-free-unused memstats-acc-free-unused)
  (environment-set! larc 'memstats-acc-root-scan-gc
                    memstats-acc-root-scan-gc)
  (environment-set! larc 'memstats-acc-root-scan-promotion
                    memstats-acc-root-scan-promotion)
  (environment-set! larc 'memstats-acc-los-sweep-gc
                    memstats-acc-los-sweep-gc)
  (environment-set! larc 'memstats-acc-los-sweep-promotion
                    memstats-acc-los-sweep-promotion)
  (environment-set! larc 'memstats-acc-remset-scan-gc
                    memstats-acc-remset-scan-gc)
  (environment-set! larc 'memstats-acc-remset-scan-promotion
                    memstats-acc-remset-scan-promotion)
  (environment-set! larc 'memstats-acc-tospace-scan-gc
                    memstats-acc-tospace-scan-gc)
  (environment-set! larc 'memstats-acc-tospace-scan-promotion
                    memstats-acc-tospace-scan-promotion)
  (environment-set! larc 'memstats-acc-reset-after-gc
                    memstats-acc-reset-after-gc)
  (environment-set! larc 'memstats-acc-decrement-after-gc
                    memstats-acc-decrement-after-gc)
  (environment-set! larc 'memstats-acc-dof-remset-scan
                    memstats-acc-dof-remset-scan)
  (environment-set! larc 'memstats-acc-sweep-shadow
                    memstats-acc-sweep-shadow)
  (environment-set! larc 'memstats-acc-msgc-mark
                    memstats-acc-msgc-mark)
  (environment-set! larc 'memstats-acc-sweep-dof-sets
                    memstats-acc-sweep-dof-sets)
  (environment-set! larc 'memstats-acc-sweep-remset
                    memstats-acc-sweep-remset)
  (environment-set! larc 'memstats-acc-sweep-los
                    memstats-acc-sweep-los)
  (environment-set! larc 'memstats-acc-assimilate-promotion
                    memstats-acc-assimilate-promotion)
  (environment-set! larc 'memstats-acc-assimilate-gc
                    memstats-acc-assimilate-gc)
  (environment-set! larc 'memstats-acc-words-copied-by-gc
                    memstats-acc-words-copied-by-gc)
  (environment-set! larc 'memstats-acc-words-copied-by-promotion
                    memstats-acc-words-copied-by-promotion)
  (environment-set! larc 'memstats-acc-words-forwarded
                    memstats-acc-words-forwarded)
  (environment-set! larc 'memstats-acc-pointers-forwarded
                    memstats-acc-pointers-forwarded)
  (environment-set! larc 'memstats-acc-gc-barrier-hits
                    memstats-acc-gc-barrier-hits)
  (environment-set! larc 'memstats-acc-remset-large-objects-scanned
                    memstats-acc-remset-large-objects-scanned)
  (environment-set! larc 'memstats-acc-remset-large-object-words-scanned
                    memstats-acc-remset-large-object-words-scanned)
  (environment-set! larc 'memstats-gen-major-id memstats-gen-major-id)
  (environment-set! larc 'memstats-gen-minor-id memstats-gen-minor-id)
  (environment-set! larc 'memstats-gen-collections memstats-gen-collections)
  (environment-set! larc 'memstats-gen-promotions memstats-gen-promotions)
  (environment-set! larc 'memstats-gen-total-elapsed-time 
                    memstats-gen-total-elapsed-time)
  (environment-set! larc 'memstats-gen-total-cpu-time
                    memstats-gen-total-cpu-time)
  (environment-set! larc 'memstats-gen-promotion-elapsed-time 
                    memstats-gen-promotion-elapsed-time)
  (environment-set! larc 'memstats-gen-promotion-cpu-time
                    memstats-gen-promotion-cpu-time)
  (environment-set! larc 'memstats-gen-target-size-now 
                    memstats-gen-target-size-now)
  (environment-set! larc 'memstats-gen-allocated-now 
                    memstats-gen-allocated-now)
  (environment-set! larc 'memstats-gen-live-now memstats-gen-live-now)
  (environment-set! larc 'memstats-remset-major-id memstats-remset-major-id)
  (environment-set! larc 'memstats-remset-minor-id memstats-remset-minor-id)
  (environment-set! larc 'memstats-remset-allocated-max 
                    memstats-remset-allocated-max)
  (environment-set! larc 'memstats-remset-allocated-now 
                    memstats-remset-allocated-now)
  (environment-set! larc 'memstats-remset-used-now memstats-remset-used-now)
  (environment-set! larc 'memstats-remset-live-now memstats-remset-live-now)
  (environment-set! larc 'memstats-remset-recorded memstats-remset-recorded)
  (environment-set! larc 'memstats-remset-removed memstats-remset-removed)
  (environment-set! larc 'memstats-remset-scanned memstats-remset-scanned)
  (environment-set! larc 'memstats-remset-object-words-scanned 
                    memstats-remset-object-words-scanned)
  (environment-set! larc 'memstats-remset-transactions 
                    memstats-remset-transactions)
  (environment-set! larc 'memstats-remset-times-cleared 
                    memstats-remset-times-cleared)
  (environment-set! larc 'memstats-remset-times-scanned 
                    memstats-remset-times-scanned)
  (environment-set! larc 'memstats-remset-times-compacted 
                    memstats-remset-times-compacted)

  ;; environment interface

  (environment-set! larc 'command-line-arguments command-line-arguments)
  (environment-set! larc 'getenv getenv)
  (environment-set! larc 'system system)
  (environment-set! larc 'current-directory current-directory)

  ;; miscellaneous extensions and hacks

  (environment-set! larc 'repl repl)
  (environment-set! larc 'repl-printer repl-printer)
  (environment-set! larc 'repl-evaluator repl-evaluator)
  (environment-set! larc 'repl-prompt repl-prompt)
  (environment-set! larc 'repl-level repl-level)
  (environment-set! larc 'herald herald)
  (environment-set! larc 'load-evaluator load-evaluator)
  (environment-set! larc 'macro-expand toplevel-macro-expand)
  (environment-set! larc 'macro-expander macro-expander)
  (environment-set! larc 'typetag typetag)
  (environment-set! larc 'typetag-set! typetag-set!)
  (environment-set! larc 'unspecified unspecified)
  (environment-set! larc 'undefined undefined)

  ;; INSTALL-ENVIRONMENTS! is used by Util/std-heap.sch, at least.

  (environment-set! larc 'install-environments! install-environments!)

  larc)

; eof
