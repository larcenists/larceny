; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; The interpreter's top-level environment.

($$trace "toplevel")

(define (init-toplevel-environment)
  (let* ((null (make-environment "null-environment" #f))
	 (r4rs (make-environment "report-environment-4" null))
	 (r5rs (make-environment "report-environment-5" r4rs))
	 (larc (make-environment "interaction-environment" r5rs)))

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
    ;; not in R4RS:
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

    ;; symbols

    (environment-set! r4rs 'symbol? symbol?)
    (environment-set! r4rs 'symbol->string symbol->string)
    (environment-set! r4rs 'string->symbol string->symbol)
    ;; not in R4RS:
    (environment-set! larc 'symbol-hash symbol-hash)
    (environment-set! larc 'gensym gensym)
    (environment-set! larc 'oblist oblist)
    (environment-set! larc 'oblist-set! oblist-set!)

    ;; R5RS environments

    (environment-set! r5rs 'interaction-environment interaction-environment)
    (environment-set! r5rs 'scheme-report-environment
		           scheme-report-environment)
    (environment-set! r5rs 'null-environment null-environment)
    ;; not in R5RS:
    (environment-set! larc 'make-environment make-environment)
    (environment-set! larc 'environment? environment?)
    (environment-set! larc 'environment-name environment-name)
    (environment-set! larc 'environment-variables environment-variables)
    (environment-set! larc 'environment-gettable? environment-gettable?)
    (environment-set! larc 'environment-settable? environment-settable?)
    (environment-set! larc 'environment-get environment-get)
    (environment-set! larc 'environment-get-cell environment-get-cell)
    (environment-set! larc 'environment-set! environment-set!)
    (environment-set! larc 'environment-reify environment-reify)
    (environment-set! larc 'environment-tag environment-tag)

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
    ;; not in R4RS:
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
    ;; not in R4RS:
    (environment-set! larc 'string-hash string-hash)
    (environment-set! larc 'substring-fill! substring-fill!)
    (environment-set! larc 'string-downcase! string-downcase!)
    (environment-set! larc 'string-upcase! string-upcase!)
    (environment-set! larc 'string-downcase string-downcase)
    (environment-set! larc 'string-upcase string-upcase)

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
    ;; not in R4RS:
    (environment-set! larc 'vector-copy vector-copy)

    ;; control features

    (environment-set! r4rs 'procedure? procedure?)
    (environment-set! r4rs 'apply apply)
    (environment-set! r4rs 'map map)
    (environment-set! r4rs 'for-each for-each)
    (environment-set! r4rs 'force force)
    (environment-set! r4rs 'call-with-current-continuation
		      call-with-current-continuation)
    ;; in R5RS:
    (environment-set! r5rs 'dynamic-wind dynamic-wind)
    (environment-set! r5rs 'eval eval)
    (environment-set! r5rs 'values values)
    (environment-set! r5rs 'call-with-values call-with-values)
    ;; not in R4RS:
    (environment-set! larc 'make-trampoline make-trampoline)
    (environment-set! larc 'procedure-copy procedure-copy)
    (environment-set! larc 'evaluator evaluator)
    (environment-set! larc 'procedure-length procedure-length)
    (environment-set! larc 'procedure-ref procedure-ref)
    (environment-set! larc 'procedure-set! procedure-set!)
    (environment-set! larc 'make-procedure make-procedure)
    (environment-set! larc 'enable-interrupts enable-interrupts)
    (environment-set! larc 'disable-interrupts disable-interrupts)
;    (environment-set! larc 'interrupt-handler interrupt-handler)
    (environment-set! larc 'timer-interrupt-handler timer-interrupt-handler)
    (environment-set! larc 'keyboard-interrupt-handler 
                      keyboard-interrupt-handler)
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
    (environment-set! r4rs 'transcript-on transcript-on)
    (environment-set! r4rs 'transcript-off transcript-off)
    ;; not in R4RS:
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

    ;; general

    (environment-set! r4rs 'load load)
    ;; transcript-on, transcript-off (optional)

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
    (environment-set! larc 'sort sort)
    (environment-set! larc 'sort! sort!)

    ;; debugging.

    (environment-set! larc 'error-continuation error-continuation)
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

    ;; Support for or hooks into the interpreter.

    (environment-set! larc 'macro-expand toplevel-macro-expand)
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

    ;; Loading compiled code

    (environment-set! null '.petit-patch-procedure .petit-patch-procedure)

    ;; system performance and interface

    (environment-set! larc 'run-with-stats run-with-stats)
    (environment-set! larc 'run-benchmark run-benchmark)
    (environment-set! larc 'display-memstats display-memstats)
    (environment-set! larc 'collect collect)
    (environment-set! larc 'gcctl gcctl)
    (environment-set! larc 'sro sro)
    (environment-set! larc 'memstats memstats)
    (environment-set! larc 'system-features system-features)
    (environment-set! larc 'stats-dump-on stats-dump-on)
    (environment-set! larc 'stats-dump-off stats-dump-off)
    (environment-set! larc 'system-function system-function)
    (environment-set! larc 'sys$C-ffi-apply sys$C-ffi-apply)
    (environment-set! larc 'sys$C-ffi-dlopen sys$C-ffi-dlopen)
    (environment-set! larc 'sys$C-ffi-dlsym sys$C-ffi-dlsym)
    (environment-set! larc 'peek-bytes peek-bytes)
    (environment-set! larc 'poke-bytes poke-bytes)
    (environment-set! larc 'gc-counter gc-counter)

    ;; environment interface

    (environment-set! larc 'command-line-arguments command-line-arguments)
    (environment-set! larc 'getenv getenv)
    (environment-set! larc 'dump-heap dump-heap)
    (environment-set! larc 'dump-interactive-heap dump-interactive-heap)
    (environment-set! larc 'system system)

    ;; miscellaneous extensions

    (environment-set! larc 'repl repl)
    (environment-set! larc 'repl-printer repl-printer)
    (environment-set! larc 'repl-evaluator repl-evaluator)
    (environment-set! larc 'repl-prompt repl-prompt)
    (environment-set! larc 'herald herald)
    (environment-set! larc 'load-evaluator load-evaluator)
    (environment-set! larc 'typetag typetag)
    (environment-set! larc 'typetag-set! typetag-set!)
    (environment-set! larc '**newline** **newline**) ; Who uses this???
    (environment-set! larc 'unspecified unspecified)
    (environment-set! larc 'undefined undefined)

    (initialize-environments null r4rs r5rs larc)
    #t))

; eof
