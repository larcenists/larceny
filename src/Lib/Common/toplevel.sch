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
  (environment-set! r4rs 'make-case-lambda make-case-lambda)

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

  (environment-set-macro! larc 'define-values (usual-syntax 'define-values))
  (environment-set-macro! larc 'case-lambda (usual-syntax 'case-lambda))

  (environment-set-macro! larc 'letrec* (usual-syntax 'letrec*))
  (environment-set-macro! larc 'assert (usual-syntax 'assert))
  (environment-set-macro! larc 'define-condition-type
                          (usual-syntax 'define-condition-type))

; Deprecated R6RS syntax.
;
; (environment-set-macro! larc 'endianness (usual-syntax 'endianness))
; (environment-set-macro! larc 'file-options (usual-syntax 'file-options))
; (environment-set-macro! larc 'buffer-mode (usual-syntax 'buffer-mode))
; (environment-set-macro! larc 'eol-style (usual-syntax 'eol-style))
; (environment-set-macro! larc 'error-handling-mode
;                         (usual-syntax 'error-handling-mode))

  (environment-set! larc 'file-options file-options)
  (environment-set! larc 'no-create    'no-create)
  (environment-set! larc 'no-fail      'no-fail)
  (environment-set! larc 'no-truncate  'no-truncate)

  ;; JavaDot and case-sensitivity
  ;;(environment-set-macro! larc '.javadot (usual-syntax '.javadot))

  (environment-set! larc 'case-sensitive? case-sensitive?)
  (environment-set! larc 'read-square-bracket-as-paren
                    read-square-bracket-as-paren)
  (environment-set! larc 'recognize-javadot-symbols?
                    recognize-javadot-symbols?)
  (environment-set! larc 'javadot-type-suffix javadot-type-suffix)
  (environment-set! larc 'javadot-generic-suffix javadot-generic-suffix)
  (environment-set! larc 'recognize-keywords? recognize-keywords?)
  (environment-set! larc 'javadot-symbol? javadot-symbol?)
  (environment-set! larc 'javadot-symbol->symbol! javadot-symbol->symbol!)
  (environment-set! larc 'symbol->javadot-symbol! symbol->javadot-symbol!)

  (environment-set! larc 'read-r6rs-flags? read-r6rs-flags?)
  (environment-set! larc 'read-larceny-weirdness? read-larceny-weirdness?)
  (environment-set! larc 'read-traditional-weirdness?
                    read-traditional-weirdness?)
  (environment-set! larc 'read-mzscheme-weirdness? read-mzscheme-weirdness?)
  (environment-set! larc 'read-r5rs-weirdness? read-r5rs-weirdness?)
  (environment-set! larc 'read-r6rs-weirdness? read-r6rs-weirdness?)
  (environment-set! larc 'read-r7rs-weirdness? read-r7rs-weirdness?)
  (environment-set! larc 'fasl-evaluator fasl-evaluator)

  ;; pairs and lists

  (environment-set! larc 'adjoin adjoin)
  (environment-set! larc 'append! append!)
  (environment-set! larc 'append-map append-map)                        ; FIXME
  (environment-set! larc 'append-map! append-map!)                      ; FIXME
  (environment-set! larc 'assoc assoc)
  (environment-set! larc 'assoc-string assoc-string)                    ; FIXME
  (environment-set! larc 'assoc-string-ci assoc-string-ci)              ; FIXME
  (environment-set! larc 'assp assp)
  (environment-set! larc 'assq assq)
  (environment-set! larc 'assv assv)
  (environment-set! larc 'cons* cons*)
  (environment-set! larc 'every? every?)
  (environment-set! larc 'exists exists)
  (environment-set! larc 'filter filter)
  (environment-set! larc 'find find)
  (environment-set! larc 'find-if find-if)                              ; FIXME
  (environment-set! larc 'find-if-not find-if-not)                      ; FIXME
  (environment-set! larc 'fold-left fold-left)
  (environment-set! larc 'fold-right fold-right)
  (environment-set! larc 'foldl foldl)                                  ; FIXME
  (environment-set! larc 'foldr foldr)                                  ; FIXME
  (environment-set! larc 'for-all for-all)
  (environment-set! larc 'improper-length improper-length)              ; FIXME
  (environment-set! larc 'last last)                                    ; FIXME
  (environment-set! larc 'last-pair last-pair)
  (environment-set! larc 'length<=? length<=?)                          ; FIXME
  (environment-set! larc 'length<? length<?)                            ; FIXME
  (environment-set! larc 'length=? length=?)                            ; FIXME
  (environment-set! larc 'length>=? length>=?)                          ; FIXME
  (environment-set! larc 'length>? length>?)                            ; FIXME
  (environment-set! larc 'list* list*)                                  ; FIXME
  (environment-set! larc 'make-list make-list)
  (environment-set! larc 'list-copy list-copy)
  (environment-set! larc 'list-head list-head)
  (environment-set! larc 'list-set! list-set!)                          ; FIXME
  (environment-set! larc 'longer? longer?)
  (environment-set! larc 'memp memp)
  (environment-set! larc 'memp-not memp-not)                            ; FIXME
  (environment-set! larc 'memf memp)            ; deprecated alias      ; FIXME
  (environment-set! larc 'memf-not memp-not)    ; deprecated alias      ; FIXME
  (environment-set! larc 'partition partition)
  (environment-set! larc 'position-of position-of)
  (environment-set! larc 'remove remove)
  (environment-set! larc 'remove! remove!)
  (environment-set! larc 'remq remq)
  (environment-set! larc 'remq! remq!)
  (environment-set! larc 'remv remv)
  (environment-set! larc 'remv! remv!)
  (environment-set! larc 'remp remp)
  (environment-set! larc 'remp! remp!)
  (environment-set! larc 'revappend revappend)                          ; FIXME
  (environment-set! larc 'revappend! revappend!)                        ; FIXME
  (environment-set! larc 'reverse! reverse!)
  (environment-set! larc 'set-last! set-last!)                          ; FIXME
  (environment-set! larc 'shorter? shorter?)
  (environment-set! larc 'some? some?)
  (environment-set! larc 'union union)

  (environment-set! larc 'larceny:map larceny:map)                      ; FIXME
  (environment-set! larc 'larceny:for-each larceny:for-each)            ; FIXME
  (environment-set! larc 'larceny:member larceny:member)                ; FIXME
  (environment-set! larc 'larceny:assoc larceny:assoc)                  ; FIXME

  (environment-set! larc 'larceny:list-copy larceny:list-copy)          ; FIXME
  (environment-set! larc 'larceny:fold-right larceny:fold-right)        ; FIXME
  (environment-set! larc 'larceny:remove larceny:remove)                ; FIXME
  (environment-set! larc 'larceny:remove! larceny:remove!)              ; FIXME

  ;; hash functions and hashtables

  (environment-set! larc 'object-hash object-hash)
  (environment-set! larc 'equal-hash equal-hash)
  (environment-set! larc 'procedure-hasher procedure-hasher)
  (environment-set! larc 'hashtable-implementation hashtable-implementation)

  (environment-set! larc 'make-oldstyle-hashtable make-oldstyle-hashtable)
  (environment-set! larc 'make-r6rs-hashtable make-r6rs-hashtable)
  (environment-set! larc 'make-eq-hashtable make-eq-hashtable)
  (environment-set! larc 'make-eqv-hashtable make-eqv-hashtable)
  (environment-set! larc 'make-hashtable make-hashtable)
  (environment-set! larc 'hashtable? hashtable?)
  (environment-set! larc 'hashtable-size hashtable-size)
  (environment-set! larc 'hashtable-ref hashtable-ref)
  (environment-set! larc 'hashtable-set! hashtable-set!)
  (environment-set! larc 'hashtable-delete! hashtable-delete!)
  (environment-set! larc 'hashtable-contains? hashtable-contains?)
  (environment-set! larc 'hashtable-update! hashtable-update!)
  (environment-set! larc 'hashtable-copy hashtable-copy)
  (environment-set! larc 'hashtable-clear! hashtable-clear!)
  (environment-set! larc 'hashtable-keys hashtable-keys)
  (environment-set! larc 'hashtable-entries hashtable-entries)
  (environment-set! larc 'hashtable-equivalence-function
                    hashtable-equivalence-function)
  (environment-set! larc 'hashtable-hash-function hashtable-hash-function)
  (environment-set! larc 'hashtable-mutable? hashtable-mutable?)

  (environment-set! larc 'hashtable-fetch hashtable-fetch)
  (environment-set! larc 'hashtable-get hashtable-get)
  (environment-set! larc 'hashtable-put! hashtable-put!)
  (environment-set! larc 'hashtable-remove! hashtable-remove!)
  (environment-set! larc 'hashtable-for-each hashtable-for-each)
  (environment-set! larc 'hashtable-map hashtable-map)

  (environment-set! larc 'hashtable-reset! hashtable-reset!)
  (environment-set! larc 'reset-all-hashtables! reset-all-hashtables!)

  ;; symbols

  (environment-set! larc 'symbol-hash symbol-hash)
  (environment-set! larc 'gensym gensym)
  (environment-set! larc 'uninterned-symbol? uninterned-symbol?)
  (environment-set! larc 'oblist oblist)
  (environment-set! larc 'oblist-set! oblist-set!)

  (environment-set! larc 'symbol=? symbol=?)

  ;; booleans

  (environment-set! larc 'boolean=? boolean=?)

  ;; environments

  (environment-set! larc 'make-environment make-environment)
  (environment-set! larc 'environment? environment?)
  (environment-set! larc 'environment-name environment-name)
  (environment-set! larc 'environment-variables environment-variables)
  (environment-set! larc 'environment-variable? environment-variable?)
  (environment-set! larc 'environment-get environment-get)
  (environment-set! larc 'environment-get-cell environment-get-cell)
  (environment-set! larc 'environment-set! environment-set!)
  (environment-set! larc 'environment-link-variables!
                    environment-link-variables!)
  (environment-set! larc 'environment-syntax-environment
                    environment-syntax-environment)
  (environment-set! larc 'environment-auxiliary-info
                    environment-auxiliary-info)
  (environment-set! larc 'environment-set-auxiliary-info!
                    environment-set-auxiliary-info!)
  (environment-set! larc 'environment-copy environment-copy)
  (environment-set! larc 'environment-macros environment-macros)
  (environment-set! larc 'environment-get-macro environment-get-macro)
  (environment-set! larc 'environment-set-macro! environment-set-macro!)
  (environment-set! larc 'environment-macro? environment-macro?)
  (environment-set! larc 'larceny-initialized-environment 
                    larceny-initialized-environment)

  ;; numbers

  (environment-set! larc 'random random)
  (environment-set! larc 'fixnum? fixnum?)
  (environment-set! larc 'bignum? bignum?)
  (environment-set! larc 'ratnum? ratnum?)
  (environment-set! larc 'flonum? flonum?)
  (environment-set! larc 'compnum? compnum?)
  (environment-set! larc 'rectnum? rectnum?)
  (environment-set! larc 'integer-logand integer-logand)
  (environment-set! larc 'integer-logior integer-logior)
  (environment-set! larc 'integer-logxor integer-logxor)
  (environment-set! larc 'integer-lognot integer-lognot)
  (environment-set! larc 'fxlogand fxlogand)
  (environment-set! larc 'fxlogior fxlogior)
  (environment-set! larc 'fxlogxor fxlogxor)
  (environment-set! larc 'fxlognot fxlognot)
  (environment-set! larc 'fxrsha fxrsha)
  (environment-set! larc 'fxrshl fxrshl)
  (environment-set! larc 'fxlsh fxlsh)
  (environment-set! larc 'most-positive-fixnum most-positive-fixnum)
  (environment-set! larc 'most-negative-fixnum most-negative-fixnum)

  (environment-set! larc 'fixnum-width fixnum-width)
  (environment-set! larc 'least-fixnum least-fixnum)
  (environment-set! larc 'greatest-fixnum greatest-fixnum)
  (environment-set! larc 'fx=? fx=?)
  (environment-set! larc 'fx<? fx<?)
  (environment-set! larc 'fx>? fx>?)
  (environment-set! larc 'fx<=? fx<=?)
  (environment-set! larc 'fx>=? fx>=?)
  (environment-set! larc 'fxzero? fxzero?)
  (environment-set! larc 'fxpositive? fxpositive?)
  (environment-set! larc 'fxnegative? fxnegative?)
  (environment-set! larc 'fxodd? fxodd?)
  (environment-set! larc 'fxeven? fxeven?)
  (environment-set! larc 'fxmax fxmax)
  (environment-set! larc 'fxmin fxmin)
  (environment-set! larc 'fx+ fx+)
  (environment-set! larc 'fx- fx-)
  (environment-set! larc 'fx* fx*)
  (environment-set! larc 'fxdiv-and-mod fxdiv-and-mod)
  (environment-set! larc 'fxdiv fxdiv)
  (environment-set! larc 'fxmod fxmod)
  (environment-set! larc 'fxdiv0-and-mod0 fxdiv0-and-mod0)
  (environment-set! larc 'fxdiv0 fxdiv0)
  (environment-set! larc 'fxmod0 fxmod0)
  (environment-set! larc 'fx+/carry fx+/carry)
  (environment-set! larc 'fx-/carry fx-/carry)
  (environment-set! larc 'fx*/carry fx*/carry)
  (environment-set! larc 'fxnot fxnot)
  (environment-set! larc 'fxand fxand)
  (environment-set! larc 'fxior fxior)
  (environment-set! larc 'fxxor fxxor)
  (environment-set! larc 'fxif fxif)
  (environment-set! larc 'fxbit-count fxbit-count)
  (environment-set! larc 'fxlength fxlength)
  (environment-set! larc 'fxfirst-bit-set fxfirst-bit-set)
  (environment-set! larc 'fxbit-set? fxbit-set?)
  (environment-set! larc 'fxcopy-bit fxcopy-bit)
  (environment-set! larc 'fxbit-field fxbit-field)
  (environment-set! larc 'fxcopy-bit-field fxcopy-bit-field)
  (environment-set! larc 'fxrotate-bit-field fxrotate-bit-field)
  (environment-set! larc 'fxreverse-bit-field fxreverse-bit-field)
  (environment-set! larc 'fxarithmetic-shift fxarithmetic-shift)
  (environment-set! larc 'fxarithmetic-shift-left fxarithmetic-shift-left)
  (environment-set! larc 'fxarithmetic-shift-right fxarithmetic-shift-right)
  ;; added for SRFI 143
  (environment-set! larc 'fxneg fxneg)
  (environment-set! larc 'fxquotient fxquotient)
  (environment-set! larc 'fxremainder fxremainder)
  (environment-set! larc 'fxabs fxabs)
  (environment-set! larc 'fxsquare fxsquare)
  (environment-set! larc 'fxsqrt fxsqrt)
  (environment-set! larc 'fxfirst-set-bit fxfirst-set-bit)
  (environment-set! larc 'fxbit-field-rotate fxbit-field-rotate)
  (environment-set! larc 'fxbit-field-reverse fxbit-field-reverse)

  ; FIXME: Needed by src/Compiler/common.imp.sch

  (environment-set! larc 'fx:check-result fx:check-result) ;FIXME

  (environment-set! larc 'real->flonum real->flonum)
  (environment-set! larc 'fl=? fl=?)
  (environment-set! larc 'fl<? fl<?)
  (environment-set! larc 'fl>? fl>?)
  (environment-set! larc 'fl<=? fl<=?)
  (environment-set! larc 'fl>=? fl>=?)
  (environment-set! larc 'flinteger? flinteger?)
  (environment-set! larc 'flzero? flzero?)
  (environment-set! larc 'flpositive? flpositive?)
  (environment-set! larc 'flnegative? flnegative?)
  (environment-set! larc 'flodd? flodd?)
  (environment-set! larc 'fleven? fleven?)
  (environment-set! larc 'flfinite? flfinite?)
  (environment-set! larc 'flinfinite? flinfinite?)
  (environment-set! larc 'flnan? flnan?)
  (environment-set! larc 'flmax flmax)
  (environment-set! larc 'flmin flmin)
  (environment-set! larc 'fl+ fl+)
  (environment-set! larc 'fl* fl*)
  (environment-set! larc 'fl- fl-)
  (environment-set! larc 'fl/ fl/)
  (environment-set! larc 'flabs flabs)
  (environment-set! larc 'fldiv-and-mod fldiv-and-mod)
  (environment-set! larc 'fldiv fldiv)
  (environment-set! larc 'flmod flmod)
  (environment-set! larc 'fldiv0-and-mod0 fldiv0-and-mod0)
  (environment-set! larc 'fldiv0 fldiv0)
  (environment-set! larc 'flmod0 flmod0)
  (environment-set! larc 'flnumerator flnumerator)
  (environment-set! larc 'fldenominator fldenominator)
  (environment-set! larc 'flfloor flfloor)
  (environment-set! larc 'flceiling flceiling)
  (environment-set! larc 'fltruncate fltruncate)
  (environment-set! larc 'flround flround)
  (environment-set! larc 'flexp flexp)
  (environment-set! larc 'fllog fllog)
  (environment-set! larc 'flsin flsin)
  (environment-set! larc 'flcos flcos)
  (environment-set! larc 'fltan fltan)
  (environment-set! larc 'flasin flasin)
  (environment-set! larc 'flacos flacos)
  (environment-set! larc 'flatan flatan)
  (environment-set! larc 'flsqrt flsqrt)
  (environment-set! larc 'flexpt flexpt)
  (environment-set! larc 'fixnum->flonum fixnum->flonum)
  (environment-set! larc 'fl+* fl+*)
  (environment-set! larc 'flfirst-bessel flfirst-bessel)
  (environment-set! larc 'flsecond-bessel flsecond-bessel)

  (environment-set! larc '&no-infinities &no-infinities)
  (environment-set! larc 'make-no-infinities-violation
                    make-no-infinities-violation)
  (environment-set! larc 'no-infinities-violation? no-infinities-violation?)
  (environment-set! larc '&no-nans &no-nans)
  (environment-set! larc 'make-no-nans-violation make-no-nans-violation)
  (environment-set! larc 'no-nans-violation? no-nans-violation?)

  (environment-set! larc 'bitwise-not bitwise-not)
  (environment-set! larc 'bitwise-and bitwise-and)
  (environment-set! larc 'bitwise-ior bitwise-ior)
  (environment-set! larc 'bitwise-xor bitwise-xor)
  (environment-set! larc 'bitwise-if bitwise-if)
  (environment-set! larc 'bitwise-bit-count bitwise-bit-count)
  (environment-set! larc 'bitwise-length bitwise-length)
  (environment-set! larc 'bitwise-first-bit-set bitwise-first-bit-set)
  (environment-set! larc 'bitwise-bit-set? bitwise-bit-set?)
  (environment-set! larc 'bitwise-copy-bit bitwise-copy-bit)
  (environment-set! larc 'bitwise-bit-field bitwise-bit-field)
  (environment-set! larc 'bitwise-copy-bit-field bitwise-copy-bit-field)
  (environment-set! larc 'bitwise-arithmetic-shift bitwise-arithmetic-shift)
  (environment-set! larc 'bitwise-arithmetic-shift-left
                    bitwise-arithmetic-shift-left)
  (environment-set! larc 'bitwise-arithmetic-shift-right
                    bitwise-arithmetic-shift-right)
  (environment-set! larc 'bitwise-rotate-bit-field bitwise-rotate-bit-field)
  (environment-set! larc 'bitwise-reverse-bit-field bitwise-reverse-bit-field)

  (environment-set! larc 'real-valued? real-valued?)
  (environment-set! larc 'rational-valued? rational-valued?)
  (environment-set! larc 'integer-valued? integer-valued?)
  (environment-set! larc 'exact exact)
  (environment-set! larc 'inexact inexact)
  (environment-set! larc 'finite? finite?)
  (environment-set! larc 'infinite? infinite?)
  (environment-set! larc 'nan? nan?)
  (environment-set! larc 'div div)
  (environment-set! larc 'mod mod)
  (environment-set! larc 'div-and-mod div-and-mod)
  (environment-set! larc 'div0 div0)
  (environment-set! larc 'mod0 mod0)
  (environment-set! larc 'div0-and-mod0 div0-and-mod0)
  (environment-set! larc 'exact-integer-sqrt exact-integer-sqrt)

  (environment-set! larc 'square square)
  (environment-set! larc 'exact-integer? exact-integer?)
  (environment-set! larc 'floor/ floor/)
  (environment-set! larc 'floor-quotient floor-quotient)
  (environment-set! larc 'floor-remainder floor-remainder)
  (environment-set! larc 'truncate/ truncate/)
  (environment-set! larc 'truncate-quotient truncate-quotient)
  (environment-set! larc 'truncate-remainder truncate-remainder)

  ;; Unicode characters

  (environment-set! larc 'char-titlecase char-titlecase)
  (environment-set! larc 'char-foldcase char-foldcase)
  (environment-set! larc 'char-general-category char-general-category)
  (environment-set! larc 'char-title-case? char-title-case?)

  ;; Unicode strings in prototype form (FIXME: should disappear)

  (environment-set! larc 'ustring? ustring?)
  (environment-set! larc 'make-ustring make-ustring)
  (environment-set! larc 'ustring-length ustring-length)
  (environment-set! larc 'ustring-ref ustring-ref)
  (environment-set! larc 'ustring-set! ustring-set!)

  ;; bytevectors

  (environment-set! larc 'bytevector-ref bytevector-ref)
  (environment-set! larc 'bytevector-set! bytevector-set!)
  (environment-set! larc 'bytevector-equal? bytevector-equal?)
  (environment-set! larc 'bytevector-fill! bytevector-fill!)
  (environment-set! larc 'list->bytevector list->bytevector)
  (environment-set! larc 'bytevector->list bytevector->list)


  (environment-set! larc 'native-endianness native-endianness)
  (environment-set! larc 'bytevector? bytevector?)
  (environment-set! larc 'make-bytevector make-bytevector)
  (environment-set! larc 'bytevector bytevector)
  (environment-set! larc 'bytevector-length bytevector-length)
  (environment-set! larc 'bytevector-u8-ref bytevector-u8-ref)
  (environment-set! larc 'bytevector-s8-ref bytevector-s8-ref)
  (environment-set! larc 'bytevector-u8-set! bytevector-u8-set!)
  (environment-set! larc 'bytevector-s8-set! bytevector-s8-set!)
  (environment-set! larc 'bytevector-uint-ref bytevector-uint-ref)
  (environment-set! larc 'bytevector-sint-ref bytevector-sint-ref)
  (environment-set! larc 'bytevector-uint-set! bytevector-uint-set!)
  (environment-set! larc 'bytevector-sint-set! bytevector-sint-set!)
  (environment-set! larc 'bytevector-u16-ref bytevector-u16-ref)
  (environment-set! larc 'bytevector-s16-ref bytevector-s16-ref)
  (environment-set! larc 'bytevector-u16-set! bytevector-u16-set!)
  (environment-set! larc 'bytevector-s16-set! bytevector-s16-set!)
  (environment-set! larc 'bytevector-u16-native-ref bytevector-u16-native-ref)
  (environment-set! larc 'bytevector-s16-native-ref bytevector-s16-native-ref)
  (environment-set! larc 'bytevector-u16-native-set!
                    bytevector-u16-native-set!)
  (environment-set! larc 'bytevector-s16-native-set!
                    bytevector-s16-native-set!)
  (environment-set! larc 'bytevector-u32-ref bytevector-u32-ref)
  (environment-set! larc 'bytevector-s32-ref bytevector-s32-ref)
  (environment-set! larc 'bytevector-u32-set! bytevector-u32-set!)
  (environment-set! larc 'bytevector-s32-set! bytevector-s32-set!)
  (environment-set! larc 'bytevector-u32-native-ref bytevector-u32-native-ref)
  (environment-set! larc 'bytevector-s32-native-ref bytevector-s32-native-ref)
  (environment-set! larc 'bytevector-u32-native-set!
                    bytevector-u32-native-set!)
  (environment-set! larc 'bytevector-s32-native-set!
                    bytevector-s32-native-set!)
  (environment-set! larc 'bytevector-u64-ref bytevector-u64-ref)
  (environment-set! larc 'bytevector-s64-ref bytevector-s64-ref)
  (environment-set! larc 'bytevector-u64-set! bytevector-u64-set!)
  (environment-set! larc 'bytevector-s64-set! bytevector-s64-set!)
  (environment-set! larc 'bytevector-u64-native-ref bytevector-u64-native-ref)
  (environment-set! larc 'bytevector-s64-native-ref bytevector-s64-native-ref)
  (environment-set! larc 'bytevector-u64-native-set!
                    bytevector-u64-native-set!)
  (environment-set! larc 'bytevector-s64-native-set!
                    bytevector-s64-native-set!)
  (environment-set! larc 'bytevector=? bytevector=?)
  (environment-set! larc 'bytevector-ieee-single-native-ref
                    bytevector-ieee-single-native-ref)
  (environment-set! larc 'bytevector-ieee-single-ref
                    bytevector-ieee-single-ref)
  (environment-set! larc 'bytevector-ieee-double-native-ref
                    bytevector-ieee-double-native-ref)
  (environment-set! larc 'bytevector-ieee-double-ref
                    bytevector-ieee-double-ref)
  (environment-set! larc 'bytevector-ieee-single-native-set!
                    bytevector-ieee-single-native-set!)
  (environment-set! larc 'bytevector-ieee-single-set!
                    bytevector-ieee-single-set!)
  (environment-set! larc 'bytevector-ieee-double-native-set!
                    bytevector-ieee-double-native-set!)
  (environment-set! larc 'bytevector-ieee-double-set!
                    bytevector-ieee-double-set!)
  (environment-set! larc 'bytevector-append bytevector-append)
  (environment-set! larc 'bytevector-copy! bytevector-copy!)
  (environment-set! larc 'r6rs:bytevector-copy! r6rs:bytevector-copy!)
  (environment-set! larc 'r7rs:bytevector-copy! r7rs:bytevector-copy!)
  (environment-set! larc 'bytevector-copy bytevector-copy)
  (environment-set! larc 'bytevector->u8-list bytevector->u8-list)
  (environment-set! larc 'u8-list->bytevector u8-list->bytevector)
  (environment-set! larc 'bytevector->uint-list bytevector->uint-list)
  (environment-set! larc 'bytevector->sint-list bytevector->sint-list)
  (environment-set! larc 'uint-list->bytevector uint-list->bytevector)
  (environment-set! larc 'sint-list->bytevector sint-list->bytevector)
  (environment-set! larc 'utf8->string utf8->string)
  (environment-set! larc 'utf16->string utf16->string)
  (environment-set! larc 'utf32->string utf32->string)
  (environment-set! larc 'string->utf8 string->utf8)
  (environment-set! larc 'string->utf16 string->utf16)
  (environment-set! larc 'string->utf32 string->utf32)

  ;; bytevector-like

  (environment-set! larc 'bytevector-like? bytevector-like?)
  (environment-set! larc 'bytevector-like-length bytevector-like-length)
  (environment-set! larc 'bytevector-like-ref bytevector-like-ref)
  (environment-set! larc 'bytevector-like-set! bytevector-like-set!)
  (environment-set! larc 'bytevector-like-equal? bytevector-like-equal?)
  (environment-set! larc 'bytevector-like-copy bytevector-like-copy)

  ;; strings

  (environment-set! larc 'string-copy! string-copy!)

  (environment-set! larc 'string-map string-map)
  (environment-set! larc 'string-for-each string-for-each)

  (environment-set! larc 'string-hash string-hash)
  (environment-set! larc 'string-ci-hash string-ci-hash)
  (environment-set! larc 'substring-fill! substring-fill!)
  (environment-set! larc 'string-downcase! string-downcase!)
  (environment-set! larc 'string-upcase! string-upcase!)

  (environment-set! larc 'string-upcase string-upcase)
  (environment-set! larc 'string-downcase string-downcase)
  (environment-set! larc 'string-foldcase string-foldcase)
  (environment-set! larc 'string-titlecase string-titlecase)

  (environment-set! larc 'string-normalize-nfd string-normalize-nfd)
  (environment-set! larc 'string-normalize-nfkd string-normalize-nfkd)
  (environment-set! larc 'string-normalize-nfc string-normalize-nfc)
  (environment-set! larc 'string-normalize-nfkc string-normalize-nfkc)

  ;; vectors

  (environment-set! larc 'vector-map vector-map)
  (environment-set! larc 'vector-for-each vector-for-each)
  (environment-set! larc 'vector-copy vector-copy)
  (environment-set! larc 'vector-copy! vector-copy!)
  (environment-set! larc 'vector-append vector-append)
  (environment-set! larc 'vector->string vector->string)
  (environment-set! larc 'string->vector string->vector)

  ;; sorting

  (environment-set! larc 'list-sort list-sort)
  (environment-set! larc 'sort sort)
  (environment-set! larc 'sort! sort!)
  (environment-set! larc 'vector-sort vector-sort)
  (environment-set! larc 'vector-sort! vector-sort!)

  ;; control features

  (environment-set! larc 'values-list values-list)
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
  (environment-set! larc 'vector-like-cas! vector-like-cas!)
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
  (environment-set! larc 'procedure-source-line procedure-source-line)
  (environment-set! larc 'procedure-source-column procedure-source-column)
  (environment-set! larc 'procedure-expression procedure-expression)
  (environment-set! larc 'procedure-formals procedure-formals)
  (environment-set! larc 'procedure-environment procedure-environment)
  (environment-set! larc 'procedure-name-set! procedure-name-set!)      ; FIXME

  (environment-set! larc 'call/cc call/cc)

  ;; records

  ;; (err5rs records procedural)

  (environment-set! larc 'make-rtd make-rtd)
  (environment-set! larc 'rtd? rtd?)
  (environment-set! larc 'rtd-constructor rtd-constructor)
  (environment-set! larc 'rtd-predicate rtd-predicate)
  (environment-set! larc 'rtd-accessor rtd-accessor)
  (environment-set! larc 'rtd-mutator rtd-mutator)

  ;; (err5rs records inspection)

  (environment-set! larc 'record? record?)
  (environment-set! larc 'record-rtd record-rtd)
  (environment-set! larc 'rtd-name rtd-name)
  (environment-set! larc 'rtd-parent rtd-parent)
  (environment-set! larc 'rtd-field-names rtd-field-names)
  (environment-set! larc 'rtd-all-field-names rtd-all-field-names)
  (environment-set! larc 'rtd-field-mutable? rtd-field-mutable?)

  ;; (rnrs records procedural)

  (environment-set! larc 'make-record-type-descriptor
                    make-record-type-descriptor)
  (environment-set! larc 'record-type-descriptor? record-type-descriptor?)
  (environment-set! larc 'make-record-constructor-descriptor
                    make-record-constructor-descriptor)
  (environment-set! larc 'record-constructor record-constructor)
  (environment-set! larc 'record-predicate record-predicate)
  (environment-set! larc 'record-accessor record-accessor)
  (environment-set! larc 'record-mutator record-mutator)

  ;; (rnrs records inspection)

  (environment-set! larc 'record-type-name record-type-name)
  (environment-set! larc 'record-type-parent record-type-parent)
  (environment-set! larc 'record-type-uid record-type-uid)
  (environment-set! larc 'record-type-generative? record-type-generative?)
  (environment-set! larc 'record-type-sealed? record-type-sealed?)
  (environment-set! larc 'record-type-opaque? record-type-opaque?)
  (environment-set! larc 'record-type-field-names record-type-field-names)
  (environment-set! larc 'record-field-mutable? record-field-mutable?)

  ;; Larceny's traditional records API, now deprecated.

  (environment-set! larc 'make-record-type make-record-type)
  (environment-set! larc 'record-type-extends? record-type-extends?)
  (environment-set! larc 'record-updater record-updater)
  (environment-set! larc 'record-type-descriptor record-type-descriptor)

  (environment-set! larc 'record-indexer record-indexer) ; MzScheme

  ;; record printers

  (environment-set! larc 'rtd-printer rtd-printer)
  (environment-set! larc 'rtd-printer-set! rtd-printer-set!)

  ;; R7RS exception mechanism

  (environment-set! larc 'error-object? error-object?)
  (environment-set! larc 'error-object-message error-object-message)
  (environment-set! larc 'error-object-irritants error-object-irritants)
  (environment-set! larc 'read-error? read-error?)
  (environment-set! larc 'file-error? file-error?)

  ;; R6RS exception mechanism

  (environment-set! larc 'with-exception-handler with-exception-handler)
  (environment-set-macro! larc 'guard (usual-syntax 'guard))
  (environment-set! larc 'raise raise)
  (environment-set! larc 'raise-continuable raise-continuable)

  ;; Larceny's extensions to the R6RS exception mechanism

  (environment-set! larc 'raise-r6rs-exception raise-r6rs-exception)

  ;; conditions

  (environment-set! larc '&condition &condition)
  (environment-set! larc 'condition condition)
  (environment-set! larc 'simple-conditions simple-conditions)
  (environment-set! larc 'condition? condition?)
  (environment-set! larc 'condition-predicate condition-predicate)
  (environment-set! larc 'condition-accessor condition-accessor)
  (environment-set-macro! larc 'define-condition-type
                          (usual-syntax 'define-condition-type))
  (environment-set! larc '&message &message)
  (environment-set! larc 'make-message-condition make-message-condition)
  (environment-set! larc 'message-condition? message-condition?)
  (environment-set! larc 'condition-message condition-message)
  (environment-set! larc '&warning &warning)
  (environment-set! larc 'make-warning make-warning)
  (environment-set! larc 'warning? warning?)
  (environment-set! larc '&serious &serious)
  (environment-set! larc 'make-serious-condition make-serious-condition)
  (environment-set! larc 'serious-condition? serious-condition?)
  (environment-set! larc '&error &error)
  (environment-set! larc 'make-error make-error)
  (environment-set! larc 'error? error?)
  (environment-set! larc '&violation &violation)
  (environment-set! larc 'make-violation make-violation)
  (environment-set! larc 'violation? violation?)
  (environment-set! larc '&assertion &assertion)
  (environment-set! larc 'make-assertion-violation make-assertion-violation)
  (environment-set! larc 'assertion-violation? assertion-violation?)
  (environment-set! larc '&irritants &irritants)
  (environment-set! larc 'make-irritants-condition make-irritants-condition)
  (environment-set! larc 'irritants-condition? irritants-condition?)
  (environment-set! larc 'condition-irritants condition-irritants)
  (environment-set! larc '&who &who)
  (environment-set! larc 'make-who-condition make-who-condition)
  (environment-set! larc 'who-condition? who-condition?)
  (environment-set! larc 'condition-who condition-who)
  (environment-set! larc '&non-continuable &non-continuable)
  (environment-set! larc 'make-non-continuable-violation
                    make-non-continuable-violation)
  (environment-set! larc 'non-continuable-violation?
                    non-continuable-violation?)
  (environment-set! larc '&implementation-restriction
                    &implementation-restriction)
  (environment-set! larc 'make-implementation-restriction-violation
                    make-implementation-restriction-violation)
  (environment-set! larc 'implementation-restriction-violation?
                    implementation-restriction-violation?)
  (environment-set! larc '&lexical &lexical)
  (environment-set! larc 'make-lexical-violation make-lexical-violation)
  (environment-set! larc 'lexical-violation? lexical-violation?)
  (environment-set! larc '&syntax &syntax)
  (environment-set! larc 'make-syntax-violation make-syntax-violation)
  (environment-set! larc 'syntax-violation? syntax-violation?)
  (environment-set! larc 'syntax-violation-form syntax-violation-form)
  (environment-set! larc 'syntax-violation-subform syntax-violation-subform)
  (environment-set! larc '&undefined &undefined)
  (environment-set! larc 'make-undefined-violation make-undefined-violation)
  (environment-set! larc 'undefined-violation? undefined-violation?)

  ;; io

  (environment-set! larc 'call-with-text-input-file call-with-text-input-file)
  (environment-set! larc 'call-with-text-output-file
                    call-with-text-output-file)
  (environment-set! larc 'close-open-ports close-open-ports)
  (environment-set! larc 'close-open-files close-open-files)
  (environment-set! larc 'console-input-port console-input-port)
  (environment-set! larc 'console-input-port-factory
                    console-input-port-factory)
  (environment-set! larc 'console-output-port console-output-port)
  (environment-set! larc 'console-output-port-factory
                    console-output-port-factory)
  (environment-set! larc 'console-error-port console-error-port)
  (environment-set! larc 'console-error-port-factory
                    console-error-port-factory)
  (environment-set! larc 'delete-file delete-file)
  (environment-set! larc 'environment-printer environment-printer)
  (environment-set! larc 'eof-object eof-object)
  (environment-set! larc 'file-exists? file-exists?)
  (environment-set! larc 'relative-path-string? relative-path-string?)
  (environment-set! larc 'absolute-path-string? absolute-path-string?)
  (environment-set! larc 'file-modification-time file-modification-time)
  (environment-set! larc 'file-newer? file-newer?)
  (environment-set! larc 'flush-output-port flush-output-port)
  (environment-set! larc 'format format)
  (environment-set! larc 'get-output-string get-output-string)
  (environment-set! larc 'get-output-bytevector get-output-bytevector)
  (environment-set! larc 'string-output-port? string-output-port?)
  (environment-set! larc 'bytevector-output-port? bytevector-output-port?)
  (environment-set! larc 'hashtable-printer hashtable-printer)
  (environment-set! larc 'io/make-port io/make-port) ; XXX
  (environment-set! larc 'io/get-u8 io/get-u8)       ; FIXME (common.imp.sch)
  (environment-set! larc 'io/put-u8 io/put-u8)       ; FIXME (common.imp.sch)
  (environment-set! larc 'io/get-char io/get-char)   ; FIXME (common.imp.sch)
  (environment-set! larc 'io/put-char io/put-char)   ; FIXME (common.imp.sch)
  (environment-set! larc 'object-is-circular? object-is-circular?)
  (environment-set! larc 'larceny:object-map larceny:object-map)
  (environment-set! larc 'write-shared write-shared)
  (environment-set! larc 'write-simple write-simple)
  (environment-set! larc 'display-simple display-simple)
  (environment-set! larc 'lowlevel-write lowlevel-write)
  (environment-set! larc 'open-text-input-file open-text-input-file)
  (environment-set! larc 'open-text-output-file open-text-output-file)
  (environment-set! larc 'open-input-string open-input-string)
  (environment-set! larc 'open-output-string open-output-string)
  (environment-set! larc 'open-input-bytevector open-input-bytevector)
  (environment-set! larc 'open-output-bytevector open-output-bytevector)
  (environment-set! larc 'open-input/output-bytevector
                    open-input/output-bytevector)
  (environment-set! larc 'port-name port-name)
  (environment-set! larc 'port-folds-case? port-folds-case?)
  (environment-set! larc 'port-folds-case! port-folds-case!)

  (environment-set! larc 'input-port-open? input-port-open?)
  (environment-set! larc 'output-port-open? output-port-open?)
  (environment-set! larc 'u8-ready? u8-ready?)
  (environment-set! larc 'peek-u8 peek-u8)
  (environment-set! larc 'read-u8 read-u8)
  (environment-set! larc 'write-u8 write-u8)
  (environment-set! larc 'read-bytevector read-bytevector)
  (environment-set! larc 'read-bytevector! read-bytevector!)
  (environment-set! larc 'write-bytevector write-bytevector)
  (environment-set! larc 'read-string read-string)

  ; FIXME (lib/MzScheme/dotnet.sch)

  (environment-set! larc 'io/port-recognizes-javadot-symbols?
                    io/port-recognizes-javadot-symbols?)
  (environment-set! larc 'io/port-recognizes-javadot-symbols!
                    io/port-recognizes-javadot-symbols!)

  (environment-set! larc 'print-length print-length)
  (environment-set! larc 'print-level print-level)
  (environment-set! larc 'procedure-printer procedure-printer)
  (environment-set! larc 'readtable-ref readtable-ref)
  (environment-set! larc 'readtable-set! readtable-set!)
  (environment-set! larc 'datum->source-location datum->source-location)
  (environment-set! larc 'datum-source-locations? datum-source-locations?)
  (environment-set! larc 'datum-source-locations-clear!
                    datum-source-locations-clear!)
  (environment-set! larc 'rename-file rename-file)
  (environment-set! larc 'reset-output-string reset-output-string)
  (environment-set! larc 'reset-output-bytevector reset-output-bytevector)
  (environment-set! larc 'weird-printer weird-printer)
  (environment-set! larc 'with-input-from-port with-input-from-port)
  (environment-set! larc 'with-output-to-port with-output-to-port)
  (environment-set! larc 'call-with-input-string call-with-input-string)
  (environment-set! larc 'call-with-output-string call-with-output-string)
  (environment-set! larc 'with-input-from-string with-input-from-string)
  (environment-set! larc 'with-output-to-string with-output-to-string)
  (environment-set! larc 'write-bytevector-like write-bytevector-like)
  (environment-set! larc 'write-string write-string)

  ;; i/o procedures proposed for R6RS, etc

  (environment-set! larc 'buffer-mode? buffer-mode?)
  (environment-set! larc 'latin-1-codec latin-1-codec)
  (environment-set! larc 'utf-8-codec utf-8-codec)
  (environment-set! larc 'utf-16-codec utf-16-codec)
  (environment-set! larc 'native-eol-style native-eol-style)
  (environment-set! larc 'make-transcoder make-transcoder)
  (environment-set! larc 'native-transcoder native-transcoder)
  (environment-set! larc 'transcoder-codec transcoder-codec)
  (environment-set! larc 'transcoder-eol-style transcoder-eol-style)
  (environment-set! larc 'transcoder-error-handling-mode
                    transcoder-error-handling-mode)
  (environment-set! larc 'bytevector->string bytevector->string)
  (environment-set! larc 'string->bytevector string->bytevector)

  (environment-set! larc 'port? port?)
  (environment-set! larc 'port-transcoder port-transcoder)
  (environment-set! larc 'textual-port? textual-port?)
  (environment-set! larc 'binary-port? binary-port?)
  (environment-set! larc 'transcoded-port transcoded-port)
  (environment-set! larc 'port-has-port-position? port-has-port-position?)
  (environment-set! larc 'port-position port-position)
  (environment-set! larc 'port-position-nocache port-position-nocache) ;FIXME
  (environment-set! larc 'port-lines-read port-lines-read) ;FIXME
  (environment-set! larc 'port-line-start port-line-start) ;FIXME
  (environment-set! larc 'port-has-set-port-position!?
                    port-has-set-port-position!?)
  (environment-set! larc 'set-port-position! set-port-position!)
  (environment-set! larc 'close-port close-port)
  (environment-set! larc 'call-with-port call-with-port)

  ; input-port? and current-input-port were part of R4RS

  (environment-set! larc 'port-eof? port-eof?)
  (environment-set! larc 'open-file-input-port open-file-input-port)
  (environment-set! larc 'open-bytevector-input-port
                    open-bytevector-input-port)
  (environment-set! larc 'open-string-input-port open-string-input-port)
  (environment-set! larc 'standard-input-port standard-input-port)

  (environment-set! larc 'output-port-buffer-mode output-port-buffer-mode)
  (environment-set! larc 'open-file-output-port open-file-output-port)
  (environment-set! larc 'open-bytevector-output-port
                    open-bytevector-output-port)
  (environment-set! larc 'call-with-bytevector-output-port
                    call-with-bytevector-output-port)
  (environment-set! larc 'open-string-output-port open-string-output-port)
  (environment-set! larc 'call-with-string-output-port
                    call-with-string-output-port)
  (environment-set! larc 'open-file-input/output-port
                    open-file-input/output-port)
  (environment-set! larc 'standard-output-port standard-output-port)
  (environment-set! larc 'standard-error-port standard-error-port)
  (environment-set! larc 'current-error-port current-error-port)

  (environment-set! larc 'make-custom-binary-input-port
                    make-custom-binary-input-port)
  (environment-set! larc 'make-custom-binary-output-port
                    make-custom-binary-output-port)
  (environment-set! larc 'make-custom-binary-input/output-port
                    make-custom-binary-input/output-port)
  (environment-set! larc 'make-custom-textual-input-port
                    make-custom-textual-input-port)
  (environment-set! larc 'make-custom-textual-output-port
                    make-custom-textual-output-port)
  (environment-set! larc 'make-custom-textual-input/output-port
                    make-custom-textual-input/output-port)

  (environment-set! larc 'lookahead-u8 lookahead-u8)
  (environment-set! larc 'get-u8 get-u8)
  (environment-set! larc 'get-bytevector-n    get-bytevector-n)
  (environment-set! larc 'get-bytevector-n!   get-bytevector-n!)
  (environment-set! larc 'get-bytevector-some get-bytevector-some)
  (environment-set! larc 'get-bytevector-all  get-bytevector-all)

  (environment-set! larc 'lookahead-char lookahead-char)
  (environment-set! larc 'get-char get-char)
  (environment-set! larc 'get-string-n    get-string-n)
  (environment-set! larc 'get-string-n!   get-string-n!)
  (environment-set! larc 'get-string-all  get-string-all)
  (environment-set! larc 'get-line        get-line)
  (environment-set! larc 'get-datum get-datum)

  (environment-set! larc 'read-line read-line)

  ; FIXME
  (environment-set! larc 'get-datum-with-source-locations
                    get-datum-with-source-locations)

  (environment-set! larc 'put-u8 put-u8)
  (environment-set! larc 'put-bytevector put-bytevector)

  (environment-set! larc 'put-char put-char)
  (environment-set! larc 'put-string put-string)
  (environment-set! larc 'put-datum put-datum)

  (environment-set! larc '&i/o &i/o)
  (environment-set! larc 'make-i/o-error make-i/o-error)
  (environment-set! larc 'i/o-error? i/o-error?)
  (environment-set! larc '&i/o-read &i/o-read)
  (environment-set! larc 'make-i/o-read-error make-i/o-read-error)
  (environment-set! larc 'i/o-read-error? i/o-read-error?)
  (environment-set! larc '&i/o-write &i/o-write)
  (environment-set! larc 'make-i/o-write-error make-i/o-write-error)
  (environment-set! larc 'i/o-write-error? i/o-write-error?)
  (environment-set! larc '&i/o-invalid-position &i/o-invalid-position)
  (environment-set! larc 'make-i/o-invalid-position-error
                    make-i/o-invalid-position-error)
  (environment-set! larc 'i/o-invalid-position-error?
                    i/o-invalid-position-error?)
  (environment-set! larc 'i/o-error-position i/o-error-position)
  (environment-set! larc '&i/o-filename &i/o-filename)
  (environment-set! larc 'make-i/o-filename-error make-i/o-filename-error)
  (environment-set! larc 'i/o-filename-error? i/o-filename-error?)
  (environment-set! larc 'i/o-error-filename i/o-error-filename)
  (environment-set! larc '&i/o-file-protection &i/o-file-protection)
  (environment-set! larc 'make-i/o-file-protection-error
                    make-i/o-file-protection-error)
  (environment-set! larc 'i/o-file-protection-error?
                    i/o-file-protection-error?)
  (environment-set! larc '&i/o-file-is-read-only &i/o-file-is-read-only)
  (environment-set! larc 'make-i/o-file-is-read-only-error
                    make-i/o-file-is-read-only-error)
  (environment-set! larc 'i/o-file-is-read-only-error?
                    i/o-file-is-read-only-error?)
  (environment-set! larc '&i/o-file-already-exists &i/o-file-already-exists)
  (environment-set! larc 'make-i/o-file-already-exists-error
                    make-i/o-file-already-exists-error)
  (environment-set! larc 'i/o-file-already-exists-error?
                    i/o-file-already-exists-error?)
  (environment-set! larc '&i/o-file-does-not-exist &i/o-file-does-not-exist)
  (environment-set! larc 'make-i/o-file-does-not-exist-error
                    make-i/o-file-does-not-exist-error)
  (environment-set! larc 'i/o-file-does-not-exist-error?
                    i/o-file-does-not-exist-error?)
  (environment-set! larc '&i/o-port &i/o-port)
  (environment-set! larc 'make-i/o-port-error make-i/o-port-error)
  (environment-set! larc 'i/o-port-error? i/o-port-error?)
  (environment-set! larc 'i/o-error-port i/o-error-port)

  (environment-set! larc '&i/o-decoding &i/o-decoding)
  (environment-set! larc 'make-i/o-decoding-error make-i/o-decoding-error)
  (environment-set! larc 'i/o-decoding-error? i/o-decoding-error?)
  (environment-set! larc '&i/o-encoding &i/o-encoding)
  (environment-set! larc 'make-i/o-encoding-error make-i/o-encoding-error)
  (environment-set! larc 'i/o-encoding-error? i/o-encoding-error?)
  (environment-set! larc 'i/o-encoding-error-char i/o-encoding-error-char)

  ;; FIXME: deprecated procedures

  (environment-set! larc 'call-with-binary-input-file
                    call-with-binary-input-file)
  (environment-set! larc 'call-with-binary-output-file
                    call-with-binary-output-file)
  (environment-set! larc 'open-binary-input-file open-binary-input-file)
  (environment-set! larc 'open-binary-output-file open-binary-output-file)
  (environment-set! larc 'with-input-from-binary-file
                    with-input-from-binary-file)
  (environment-set! larc 'with-output-to-binary-file
                    with-output-to-binary-file)

  (environment-set! larc 'call-with-raw-latin-1-input-file
                    call-with-raw-latin-1-input-file)
  (environment-set! larc 'call-with-raw-latin-1-output-file
                    call-with-raw-latin-1-output-file)
  (environment-set! larc 'open-raw-latin-1-input-file
                    open-raw-latin-1-input-file)
  (environment-set! larc 'open-raw-latin-1-output-file
                    open-raw-latin-1-output-file)
  (environment-set! larc 'with-input-from-raw-latin-1-file
                    with-input-from-raw-latin-1-file)
  (environment-set! larc 'with-output-to-raw-latin-1-file
                    with-output-to-raw-latin-1-file)

  ;; enumeration sets

  (environment-set! larc 'make-enumeration make-enumeration)
  (environment-set! larc 'enum-set-universe enum-set-universe)
  (environment-set! larc 'enum-set-indexer enum-set-indexer)
  (environment-set! larc 'enum-set-constructor enum-set-constructor)
  (environment-set! larc 'enum-set->list enum-set->list)
  (environment-set! larc 'enum-set-member? enum-set-member?)
  (environment-set! larc 'enum-set-subset? enum-set-subset?)
  (environment-set! larc 'enum-set=? enum-set=?)
  (environment-set! larc 'enum-set-union enum-set-union)
  (environment-set! larc 'enum-set-intersection enum-set-intersection)
  (environment-set! larc 'enum-set-difference enum-set-difference)
  (environment-set! larc 'enum-set-complement enum-set-complement)
  (environment-set! larc 'enum-set-projection enum-set-projection)

  ;; common and less common extensions to R4RS

  (environment-set! larc
                    'issue-deprecated-warnings? issue-deprecated-warnings?)
  (environment-set! larc 'issue-warning-deprecated issue-warning-deprecated)
  (environment-set! larc 'assertion-violation assertion-violation)
  (environment-set! larc 'error error)
  (environment-set! larc 'larceny:errmsg larceny:errmsg)
  (environment-set! larc 'error-handler error-handler)
  (environment-set! larc 'decode-error decode-error)
  (environment-set! larc 'call-with-error-handler call-with-error-handler)
  (environment-set! larc 'call-without-errors call-without-errors)
  (environment-set! larc 'reset reset)
  (environment-set! larc 'reset-handler reset-handler)
  (environment-set! larc 'call-with-reset-handler call-with-reset-handler)
  (environment-set! larc 'exit exit)
  (environment-set! larc 'emergency-exit emergency-exit)
  (environment-set! larc 'add-exit-procedure! add-exit-procedure!)
  (environment-set! larc 'add-init-procedure! add-init-procedure!)
  (environment-set! larc 'quit quit)
  (environment-set! larc 'quit-handler quit-handler)
  (environment-set! larc 'make-parameter make-parameter)
  (environment-set! larc 'parameter? parameter?)

  ;; debugging.

  (environment-set! larc 'current-continuation-structure
                    current-continuation-structure)
  (environment-set! larc 'larceny-break larceny-break)

  (environment-set! larc 'display-condition display-condition) ; FIXME
  (environment-set! larc 'display-record display-record)       ; FIXME

  ;; property lists

  (environment-set! larc 'getprop getprop)
  (environment-set! larc 'putprop putprop)
  (environment-set! larc 'remprop remprop)

  ;; vector-like

  (environment-set! larc 'vector-like? vector-like?)
  (environment-set! larc 'vector-like-length vector-like-length)
  (environment-set! larc 'vector-like-ref vector-like-ref)
  (environment-set! larc 'vector-like-set! vector-like-set!)

  ;; structures

  (environment-set! larc 'make-structure make-structure)
  (environment-set! larc 'structure? structure?)
  (environment-set! larc 'structure-printer structure-printer)
  (environment-set! larc 'structure-comparator structure-comparator)

  ;; system performance and interface

  (environment-set! larc 'collect collect)
  (environment-set! larc 'gcctl gcctl)
  (environment-set! larc 'sro sro)
  (environment-set! larc 'larceny:use-r7rs-semantics!
                    larceny:use-r7rs-semantics!)
  (environment-set! larc 'larceny:execution-mode larceny:execution-mode)
  (environment-set! larc 'system-features system-features)
  (environment-set! larc 'stats-dump-on stats-dump-on)
  (environment-set! larc 'stats-dump-off stats-dump-off)
  (environment-set! larc 'stats-dump-stdout stats-dump-stdout)
  (environment-set! larc 'system-function system-function)
  (environment-set! larc 'gc-counter gc-counter)
  (environment-set! larc 'major-gc-counter major-gc-counter)
  (environment-set! larc '.internal:machine-address .internal:machine-address)
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
  (environment-set! larc 'memstats-gc-max-cheney-elapsed-time
                    memstats-gc-max-cheney-elapsed-time)
  (environment-set! larc 'memstats-gc-max-cheney-cpu-time
                    memstats-gc-max-cheney-cpu-time)
  (environment-set! larc 'memstats-gc-max-summar-elapsed-time
                    memstats-gc-max-summar-elapsed-time)
  (environment-set! larc 'memstats-gc-max-summar-cpu-time
                    memstats-gc-max-summar-cpu-time)
  (environment-set! larc 'memstats-gc-max-smircy-mark-elapsed-time
                    memstats-gc-max-smircy-mark-elapsed-time)
  (environment-set! larc 'memstats-gc-max-smircy-mark-cpu-time
                    memstats-gc-max-smircy-mark-cpu-time)
  (environment-set! larc 'memstats-gc-max-smircy-refine-elapsed-time
                    memstats-gc-max-smircy-refine-elapsed-time)
  (environment-set! larc 'memstats-gc-max-smircy-refine-cpu-time
                    memstats-gc-max-smircy-refine-cpu-time)
  (environment-set! larc 'memstats-gc-max-truegc-elapsed-time
                    memstats-gc-max-truegc-elapsed-time)
  (environment-set! larc 'memstats-gc-max-truegc-cpu-time
                    memstats-gc-max-truegc-cpu-time)
  (environment-set! larc 'memstats-gc-max-elapsed-time
                    memstats-gc-max-elapsed-time)
  (environment-set! larc 'memstats-gc-max-cpu-time
                    memstats-gc-max-cpu-time)
  (environment-set! larc 'memstats-gc-max-remset-scan-elapsed-time
                    memstats-gc-max-remset-scan-elapsed-time)
  (environment-set! larc 'memstats-gc-max-remset-scan-cpu-time
                    memstats-gc-max-remset-scan-cpu-time)
  (environment-set! larc 'memstats-gc-total-remset-scan-elapsed-time
                    memstats-gc-total-remset-scan-elapsed-time)
  (environment-set! larc 'memstats-gc-total-remset-scan-cpu-time
                    memstats-gc-total-remset-scan-cpu-time)
  (environment-set! larc 'memstats-gc-remset-scan-count
                    memstats-gc-remset-scan-count)
  (environment-set! larc 'memstats-gc-max-entries-remset-scan
                    memstats-gc-max-entries-remset-scan)
  (environment-set! larc 'memstats-gc-total-entries-remset-scan
                    memstats-gc-total-entries-remset-scan)
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
  (environment-set! larc 'memstats-summsets-allocated-now
                    memstats-summsets-allocated-now)
  (environment-set! larc 'memstats-summsets-allocated-max
                    memstats-summsets-allocated-max)
  (environment-set! larc 'memstats-marking-allocated-now
                    memstats-marking-allocated-now)
  (environment-set! larc 'memstats-marking-allocated-max
                    memstats-marking-allocated-max)
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
  (environment-set! larc 'memstats-gc-all-major-faults-during-gcs
                    memstats-gc-all-major-faults-during-gcs)
  (environment-set! larc 'memstats-gc-all-minor-faults-during-gcs
                    memstats-gc-all-minor-faults-during-gcs)
  (environment-set! larc 'memstats-gc-major-faults-during-max-truegc-pause
                    memstats-gc-major-faults-during-max-truegc-pause)
  (environment-set! larc 'memstats-gc-minor-faults-during-max-truegc-pause
                    memstats-gc-minor-faults-during-max-truegc-pause)
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
  (environment-set! larc 'memstats-fullgc-words-marked 
                    memstats-fullgc-words-marked)
  (environment-set! larc 'memstats-fullgc-traced memstats-fullgc-traced)
  (environment-set! larc 'memstats-dofgc-resets memstats-dofgc-resets)
  (environment-set! larc 'memstats-dofgc-repeats memstats-dofgc-repeats)
  (environment-set! larc 'memstats-gc-accounting memstats-gc-accounting)
  (environment-set! larc 'memstats-mark-elapsed memstats-mark-elapsed)
  (environment-set! larc 'memstats-mark-cpu memstats-mark-cpu)
  (environment-set! larc 'memstats-mark-count memstats-mark-count)
  (environment-set! larc 'memstats-summarize-elapsed 
                    memstats-summarize-elapsed)
  (environment-set! larc 'memstats-summarize-cpu memstats-summarize-cpu)
  (environment-set! larc 'memstats-summarize-count memstats-summarize-count)
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
  (environment-set! larc 'setenv setenv)
  (environment-set! larc 'get-environment-variables get-environment-variables)
  (environment-set! larc 'get-errno get-errno)
  (environment-set! larc 'set-errno! set-errno!)
  (environment-set! larc 'make-env-parameter make-env-parameter)
  (environment-set! larc 'system system)
  (environment-set! larc 'current-directory current-directory)
  (environment-set! larc 'list-directory list-directory)


  ;; Low-level API to the interpreter

  ;; Code objects used by the interpreter.
  (environment-set! larc 'code-object?     code-object?)

  (environment-set! larc 'make-assignment  make-assignment)
  (environment-set! larc 'assignment?      assignment?)
  (environment-set! larc 'assignment.lhs   assignment.lhs)
  (environment-set! larc 'assignment.rhs   assignment.rhs)

  (environment-set! larc 'make-begin       make-begin)
  (environment-set! larc 'begin?           begin?)
  (environment-set! larc 'begin.exprs      begin.exprs)

  (environment-set! larc 'make-call        make-call)
  (environment-set! larc 'call?            call?)
  (environment-set! larc 'call.proc        call.proc)
  (environment-set! larc 'call.args        call.args)

  (environment-set! larc 'make-conditional make-conditional)
  (environment-set! larc 'conditional?     conditional?)
  (environment-set! larc 'if.test          if.test)
  (environment-set! larc 'if.then          if.then)
  (environment-set! larc 'if.else          if.else)

  (environment-set! larc 'make-constant    make-constant)
  (environment-set! larc 'constant?        constant?)
  (environment-set! larc 'constant.value   constant.value)

  (environment-set! larc 'lambda?      lambda?)
  (environment-set! larc 'make-lambda  make-lambda)
  (environment-set! larc 'lambda.args  lambda.args)
  (environment-set! larc 'lambda.defs  lambda.defs)
  (environment-set! larc 'lambda.R     lambda.R)
  (environment-set! larc 'lambda.F     lambda.F)
  (environment-set! larc 'lambda.G     lambda.G)
  (environment-set! larc 'lambda.decls lambda.decls)
  (environment-set! larc 'lambda.doc   lambda.doc)
  (environment-set! larc 'lambda.body  lambda.body)

  (environment-set! larc 'variable?     variable?)
  (environment-set! larc 'make-variable make-variable)
  (environment-set! larc 'variable.name variable.name)

  (environment-set! larc 'make-doc make-doc)
  (environment-set! larc 'doc.name doc.name)
  (environment-set! larc 'doc.code doc.code)
  (environment-set! larc 'doc.arity doc.arity)
  (environment-set! larc 'doc.file doc.file)
  (environment-set! larc 'doc.filepos doc.filepos)
  (environment-set! larc 'doc.formals doc.formals)
  (environment-set! larc 'doc.name-set! doc.name-set!)
  (environment-set! larc 'doc.code-set! doc.code-set!)
  (environment-set! larc 'doc.arity-set! doc.arity-set!)
  (environment-set! larc 'doc.file-set! doc.file-set!)
  (environment-set! larc 'doc.filepos-set! doc.filepos-set!)
  (environment-set! larc 'doc.formals-set! doc.formals-set!)

  (environment-set! larc 'make-readable make-readable)

  (environment-set! larc 'interpret interpret)
  (environment-set! larc 'interpret-code-object interpret-code-object)
  (environment-set! larc 'interpreted-procedure?  interpreted-procedure?)
  (environment-set! larc 'interpreted-expression? interpreted-expression?)
  (environment-set! larc 'interpreted-primitive?  interpreted-primitive?)
  (environment-set! larc 'interpreted-expression-source
                    interpreted-expression-source)

  ;; Continuation marks

  (environment-set-macro! larc 'with-continuation-mark
			  (usual-syntax 'with-continuation-mark))
  (environment-set! larc 'call-with-continuation-mark
		    call-with-continuation-mark)
  (environment-set! larc 'continuation-marks continuation-marks)
  (environment-set! larc 'current-continuation-marks
		    current-continuation-marks)
  (environment-set! larc 'continuation-mark-set->list
		    continuation-mark-set->list)
  (environment-set! larc 'continuation-mark-set? continuation-mark-set?)
  (environment-set! larc 'continuation-mark-set->list*
                    continuation-mark-set->list*)
  (environment-set! larc 'continuation-mark-set-first
                    continuation-mark-set-first)
  (environment-set! larc 'call-if-continuation-mark-frame
                    call-if-continuation-mark-frame)
  (environment-set! larc 'call-if-continuation-mark-replace
                    call-if-continuation-mark-replace)

  ;; require (library system)

  (environment-set! larc 'current-larceny-root current-larceny-root)
  (environment-set! larc 'current-require-path current-require-path)
  (environment-set! larc 'current-require-path-suffix-optional
                    current-require-path-suffix-optional)
  (environment-set! larc 'current-require-path-suffixes
                    current-require-path-suffixes)
  (environment-set! larc 'current-require-path-suffixes-compiled
                    current-require-path-suffixes-compiled)
  (environment-set! larc 'current-library-resolver current-library-resolver)
  (environment-set! larc 'require require)
  (environment-set! larc 'clear-require-loaded-files!
                    clear-require-loaded-files!)

  ;; miscellaneous extensions and hacks

  (environment-set! larc 'repl repl)
  (environment-set! larc 'repl-printer repl-printer)
  (environment-set! larc 'repl-evaluator repl-evaluator)
  (environment-set! larc 'repl-prompt repl-prompt)
  (environment-set! larc 'repl-level repl-level)
  (environment-set! larc 'herald herald)
  (environment-set! larc 'load-from-port load-from-port)
  (environment-set! larc 'load-evaluator load-evaluator)
  (environment-set! larc 'load-print load-print)
  (environment-set! larc 'load-verbose load-verbose)
  (environment-set! larc 'macro-expand toplevel-macro-expand)
  (environment-set! larc 'macro-expander macro-expander)
  (environment-set! larc 'current-source-file current-source-file)
  (environment-set! larc 'source-location-recorder source-location-recorder)
  (environment-set! larc 'read-source-code read-source-code)
  (environment-set! larc 'typetag typetag)
  (environment-set! larc 'typetag-set! typetag-set!)
  (environment-set! larc 'unspecified unspecified)
  (environment-set! larc 'undefined undefined)

  (environment-set! larc 'current-second current-second)
  (environment-set! larc 'current-seconds current-seconds)
  (environment-set! larc 'twobit-timer-hook twobit-timer-hook)


  ;; INSTALL-ENVIRONMENTS! is used by Util/std-heap.sch, at least.

  (environment-set! larc 'install-environments! install-environments!)

  larc)

;; *larceny-environment-extensions* : (listof (environment -> void))
;;
;; A list of procedures which accept an environment. The procedures are 
;; expected to extend the environment with new bindings.

(define *larceny-environment-extensions* '())

;; *interactive-eval-list* : (listof s-expr)
;;
;; A list of forms to be evaluated when the interpreter starts.

(define *interactive-eval-list* '())

; eof
