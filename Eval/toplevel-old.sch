; -*- scheme -*-
; $Id: toplevel-old.sch,v 1.1 1997/09/17 15:09:26 lth Exp $
;
; Larceny run-time system: The top-level environment.

($$trace "toplevel")

; Return the global variable cell for the named variable in the default
; environment. Create cell if it does not exist.

(define (toplevel-cell symbol)
  (let ((cell (toplevel-env-find symbol)))
    (if cell
	cell
	(begin (extend-toplevel-env! symbol (undefined))
	       (toplevel-cell symbol)))))

; Return the value associated with the name, if any. Error if none.

(define (toplevel-env-lookup name)
  (let ((cell (toplevel-env-find name)))
    (if (or (not cell) (eq? (global-cell-ref cell) (undefined)))
	(error "Undefined global variable \"" name "\".")
	(global-cell-ref cell))))

; Change the binding of a toplevel name.

(define (toplevel-env-set! name value)
  (let ((cell (toplevel-env-find name)))
    (if cell
	(global-cell-set! cell value)
	(extend-toplevel-env! name value))))

; We use property lists for the global environment. This is just a hack; I'm
; too lazy to implement a hash table right now (spring 1992).

(define (toplevel-env-find symbol)
  (getprop symbol 'value))

(define (extend-toplevel-env! name value)
  (putprop name 'value (make-global-cell value name)))

(define (clean-toplevel-env!!)
  (for-each (lambda (s) (remprop s 'value)) (namespace)))

(define (init-toplevel-environment)

  ;; booleans

  (extend-toplevel-env! 'not not)
  (extend-toplevel-env! 'boolean? boolean?)

  ;; equivalence predicates

  (extend-toplevel-env! 'eq? eq?)
  (extend-toplevel-env! 'eqv? eqv?)
  (extend-toplevel-env! 'equal? equal?)

  ;; pairs and lists

  (extend-toplevel-env! 'pair? pair?)
  (extend-toplevel-env! 'cons cons)
  (extend-toplevel-env! 'car car)
  (extend-toplevel-env! 'cdr cdr)
  (extend-toplevel-env! 'set-car! set-car!)
  (extend-toplevel-env! 'set-cdr! set-cdr!)
  (extend-toplevel-env! 'caar caar)
  (extend-toplevel-env! 'cadr cadr)
  (extend-toplevel-env! 'cdar cdar)
  (extend-toplevel-env! 'cddr cddr)
  (extend-toplevel-env! 'caaar caaar)
  (extend-toplevel-env! 'caadr caadr)
  (extend-toplevel-env! 'cadar cadar)
  (extend-toplevel-env! 'caddr caddr)
  (extend-toplevel-env! 'cdaar cdaar)
  (extend-toplevel-env! 'cdadr cdadr)
  (extend-toplevel-env! 'cddar cddar)
  (extend-toplevel-env! 'cdddr cdddr)
  (extend-toplevel-env! 'caaaar caaaar)
  (extend-toplevel-env! 'caaadr caaadr)
  (extend-toplevel-env! 'caadar caadar)
  (extend-toplevel-env! 'caaddr caaddr)
  (extend-toplevel-env! 'cadaar cadaar)
  (extend-toplevel-env! 'cadadr cadadr)
  (extend-toplevel-env! 'caddar caddar)
  (extend-toplevel-env! 'cadddr cadddr)
  (extend-toplevel-env! 'cdaaar cdaaar)
  (extend-toplevel-env! 'cdaadr cdaadr)
  (extend-toplevel-env! 'cdadar cdadar)
  (extend-toplevel-env! 'cdaddr cdaddr)
  (extend-toplevel-env! 'cddaar cddaar)
  (extend-toplevel-env! 'cddadr cddadr)
  (extend-toplevel-env! 'cdddar cdddar)
  (extend-toplevel-env! 'cddddr cddddr)
  (extend-toplevel-env! 'null? null?)
  (extend-toplevel-env! 'list? list?)
  (extend-toplevel-env! 'list list)
  (extend-toplevel-env! 'length length)
  (extend-toplevel-env! 'append append)
  (extend-toplevel-env! 'reverse reverse)
  (extend-toplevel-env! 'list-tail list-tail)
  (extend-toplevel-env! 'list-ref list-ref)
  (extend-toplevel-env! 'memq memq)
  (extend-toplevel-env! 'memv memv)
  (extend-toplevel-env! 'member member)
  (extend-toplevel-env! 'assq assq)
  (extend-toplevel-env! 'assv assv)
  (extend-toplevel-env! 'assoc assoc)
  ;; not in R4RS:
  (extend-toplevel-env! 'list-copy list-copy)
  (extend-toplevel-env! 'last-pair last-pair)
  (extend-toplevel-env! 'remq remq)
  (extend-toplevel-env! 'remv remv)
  (extend-toplevel-env! 'remove remove)
  (extend-toplevel-env! 'append! append!)
  (extend-toplevel-env! 'reverse! reverse!)
  (extend-toplevel-env! 'some? some?)
  (extend-toplevel-env! 'every? every?)

  ;; symbols

  (extend-toplevel-env! 'symbol? symbol?)
  (extend-toplevel-env! 'symbol->string symbol->string)
  (extend-toplevel-env! 'string->symbol string->symbol)
  ;; not in R4RS:
  (extend-toplevel-env! 'symbol-hash symbol-hash)
  (extend-toplevel-env! 'gensym gensym)
  (extend-toplevel-env! 'oblist oblist)
  (extend-toplevel-env! 'oblist-set! oblist-set!)

  ;; R5RS environments

  (extend-toplevel-env! 'interaction-environment interaction-environment)
  (extend-toplevel-env! 'scheme-report-environment scheme-report-environment)
  (extend-toplevel-env! 'null-environment null-environment)
  ;; not in R5RS:
  (extend-toplevel-env! 'environment-lookup-binding environment-lookup-binding)
  (extend-toplevel-env! 'eval-environment? environment?)

  ;; numbers

  (extend-toplevel-env! 'number? number?)
  (extend-toplevel-env! 'complex? complex?)
  (extend-toplevel-env! 'real? real?)
  (extend-toplevel-env! 'rational? rational?)
  (extend-toplevel-env! 'integer? integer?)
  (extend-toplevel-env! 'exact? exact?)
  (extend-toplevel-env! 'inexact? inexact?)
  (extend-toplevel-env! '= =)
  (extend-toplevel-env! '< <)
  (extend-toplevel-env! '> >)
  (extend-toplevel-env! '<= <=)
  (extend-toplevel-env! '>= >=)
  (extend-toplevel-env! 'zero? zero?)
  (extend-toplevel-env! 'positive? positive?)
  (extend-toplevel-env! 'negative? negative?)
  (extend-toplevel-env! 'odd? odd?)
  (extend-toplevel-env! 'even? even?)
  (extend-toplevel-env! 'max max)
  (extend-toplevel-env! 'min min)
  (extend-toplevel-env! '+ +)
  (extend-toplevel-env! '* *)
  (extend-toplevel-env! '- -)
  (extend-toplevel-env! '/ /)
  (extend-toplevel-env! 'abs abs)
  (extend-toplevel-env! 'quotient quotient)
  (extend-toplevel-env! 'remainder remainder)
  (extend-toplevel-env! 'modulo modulo)
  (extend-toplevel-env! 'gcd gcd)
  (extend-toplevel-env! 'lcm lcm)
  (extend-toplevel-env! 'numerator numerator)
  (extend-toplevel-env! 'denominator denominator)
  (extend-toplevel-env! 'floor floor)
  (extend-toplevel-env! 'ceiling ceiling)
  (extend-toplevel-env! 'truncate truncate)
  (extend-toplevel-env! 'round round)
  (extend-toplevel-env! 'rationalize rationalize)
  (extend-toplevel-env! 'exp exp)
  (extend-toplevel-env! 'log log)
  (extend-toplevel-env! 'sin sin)
  (extend-toplevel-env! 'cos cos)
  (extend-toplevel-env! 'tan tan)
  (extend-toplevel-env! 'asin asin)
  (extend-toplevel-env! 'acos acos)
  (extend-toplevel-env! 'atan atan)
  (extend-toplevel-env! 'sqrt sqrt)
  (extend-toplevel-env! 'expt expt)
  (extend-toplevel-env! 'make-rectangular make-rectangular)
  (extend-toplevel-env! 'real-part real-part)
  (extend-toplevel-env! 'imag-part imag-part)
  (extend-toplevel-env! 'make-polar make-polar)
  (extend-toplevel-env! 'magnitude magnitude)
  (extend-toplevel-env! 'angle angle)
  (extend-toplevel-env! 'exact->inexact exact->inexact)
  (extend-toplevel-env! 'inexact->exact inexact->exact)
  (extend-toplevel-env! 'number->string number->string)
  (extend-toplevel-env! 'string->number string->number)
  ;; not in R4RS:
  (extend-toplevel-env! 'random random)
  (extend-toplevel-env! 'fixnum? fixnum?)
  (extend-toplevel-env! 'bignum? bignum?)
  (extend-toplevel-env! 'ratnum? ratnum?)
  (extend-toplevel-env! 'flonum? flonum?)
  (extend-toplevel-env! 'compnum? compnum?)
  (extend-toplevel-env! 'rectnum? rectnum?)
  (extend-toplevel-env! 'logand logand)
  (extend-toplevel-env! 'logior logior)
  (extend-toplevel-env! 'logxor logxor)
  (extend-toplevel-env! 'lognot lognot)
  (extend-toplevel-env! 'rsha rsha)
  (extend-toplevel-env! 'rshl rshl)
  (extend-toplevel-env! 'lsh lsh)

  ;; Characters

  (extend-toplevel-env! 'char? char?)
  (extend-toplevel-env! 'char=? char=?)
  (extend-toplevel-env! 'char<? char<?)
  (extend-toplevel-env! 'char>? char>?)
  (extend-toplevel-env! 'char<=? char<=?)
  (extend-toplevel-env! 'char>=? char>=?)
  (extend-toplevel-env! 'char-ci=? char-ci=?)
  (extend-toplevel-env! 'char-ci<? char-ci<?)
  (extend-toplevel-env! 'char-ci>? char-ci>?)
  (extend-toplevel-env! 'char-ci<=? char-ci<=?)
  (extend-toplevel-env! 'char-ci>=? char-ci>=?)
  (extend-toplevel-env! 'char-alphabetic? char-alphabetic?)
  (extend-toplevel-env! 'char-numeric? char-numeric?)
  (extend-toplevel-env! 'char-whitespace? char-whitespace?)
  (extend-toplevel-env! 'char-upper-case? char-upper-case?)
  (extend-toplevel-env! 'char-lower-case? char-lower-case?)
  (extend-toplevel-env! 'char->integer char->integer)
  (extend-toplevel-env! 'integer->char integer->char)
  (extend-toplevel-env! 'char-upcase char-upcase)
  (extend-toplevel-env! 'char-downcase char-downcase)

  ;; strings

  (extend-toplevel-env! 'string? string?)
  (extend-toplevel-env! 'make-string make-string)
  (extend-toplevel-env! 'string string)
  (extend-toplevel-env! 'string-length string-length)
  (extend-toplevel-env! 'string-ref string-ref)
  (extend-toplevel-env! 'string-set! string-set!)
  (extend-toplevel-env! 'string=? string=?)
  (extend-toplevel-env! 'string<? string<?)
  (extend-toplevel-env! 'string>? string>?)
  (extend-toplevel-env! 'string<=? string<=?)
  (extend-toplevel-env! 'string>=? string>=?)
  (extend-toplevel-env! 'string-ci=? string-ci=?)
  (extend-toplevel-env! 'string-ci<? string-ci<?)
  (extend-toplevel-env! 'string-ci>? string-ci>?)
  (extend-toplevel-env! 'string-ci<=? string-ci<=?)
  (extend-toplevel-env! 'string-ci>=? string-ci>=?)
  (extend-toplevel-env! 'substring substring)
  (extend-toplevel-env! 'string-append string-append)
  (extend-toplevel-env! 'string->list string->list)
  (extend-toplevel-env! 'list->string list->string)
  (extend-toplevel-env! 'string-copy string-copy)
  (extend-toplevel-env! 'string-fill! string-fill!)
  ;; not in R4RS:
  (extend-toplevel-env! 'string-hash string-hash)
  (extend-toplevel-env! 'substring-fill! substring-fill!)

  ;; vectors

  (extend-toplevel-env! 'vector? vector?)
  (extend-toplevel-env! 'make-vector make-vector)
  (extend-toplevel-env! 'vector vector)
  (extend-toplevel-env! 'vector-length vector-length)
  (extend-toplevel-env! 'vector-ref vector-ref)
  (extend-toplevel-env! 'vector-set! vector-set!)
  (extend-toplevel-env! 'vector->list vector->list)
  (extend-toplevel-env! 'list->vector list->vector)
  (extend-toplevel-env! 'vector-fill! vector-fill!)

  ;; control features

  (extend-toplevel-env! 'procedure? procedure?)
  (extend-toplevel-env! 'apply apply)
  (extend-toplevel-env! 'map map)
  (extend-toplevel-env! 'for-each for-each)
  (extend-toplevel-env! 'force 'force)
  (extend-toplevel-env! 'call-with-current-continuation
                        call-with-current-continuation)
  ;; in R5RS:
  (extend-toplevel-env! 'dynamic-wind dynamic-wind)
  ;; not in R4RS:
  (extend-toplevel-env! 'procedure-length procedure-length)
  (extend-toplevel-env! 'procedure-ref procedure-ref)
  (extend-toplevel-env! 'procedure-set! procedure-set!)
  (extend-toplevel-env! 'make-procedure make-procedure)
;  (extend-toplevel-env! 'list->procedure list->procedure)
  (extend-toplevel-env! 'enable-interrupts enable-interrupts)
  (extend-toplevel-env! 'disable-interrupts disable-interrupts)
  (extend-toplevel-env! 'interrupt-handler interrupt-handler)
  (extend-toplevel-env! 'call-without-interrupts call-without-interrupts)

  ;; i/o

  (extend-toplevel-env! 'call-with-input-file call-with-input-file)
  (extend-toplevel-env! 'call-with-output-file call-with-output-file)
  (extend-toplevel-env! 'input-port? input-port?)
  (extend-toplevel-env! 'output-port? output-port?)
  (extend-toplevel-env! 'current-input-port current-input-port)
  (extend-toplevel-env! 'current-output-port current-output-port)
  (extend-toplevel-env! 'with-input-from-file with-input-from-file)
  (extend-toplevel-env! 'with-output-to-file with-output-to-file)
  (extend-toplevel-env! 'open-input-file open-input-file)
  (extend-toplevel-env! 'open-output-file open-output-file)
  (extend-toplevel-env! 'close-input-port close-input-port)
  (extend-toplevel-env! 'close-output-port close-output-port)
  (extend-toplevel-env! 'read read)
  (extend-toplevel-env! 'read-char read-char)
  (extend-toplevel-env! 'peek-char peek-char)
  (extend-toplevel-env! 'eof-object? eof-object?)
  (extend-toplevel-env! 'char-ready? char-ready?)
  (extend-toplevel-env! 'write write)
  (extend-toplevel-env! 'display display)
  (extend-toplevel-env! 'newline newline)
  (extend-toplevel-env! 'write-char write-char)
  ;; not in R4RS:
  (extend-toplevel-env! 'format format)
  (extend-toplevel-env! 'port? port?)
  (extend-toplevel-env! 'flush-output-port flush-output-port)
  (extend-toplevel-env! 'eof-object eof-object)
  (extend-toplevel-env! 'delete-file delete-file)
  (extend-toplevel-env! 'rename-file rename-file)
  (extend-toplevel-env! 'file-exists? file-exists?)
  (extend-toplevel-env! 'file-modification-time file-modification-time)
  (extend-toplevel-env! 'close-open-files close-open-files)
  (extend-toplevel-env! 'with-input-from-port with-input-from-port)
  (extend-toplevel-env! 'with-output-to-port with-output-to-port)
  (extend-toplevel-env! 'write-bytevector-like write-bytevector-like)

  ;; general

  (extend-toplevel-env! 'load load)
  ;; transcript-on, transcript-off (optional)

  ;; common extensions to R4RS

  (extend-toplevel-env! 'error error)
  (extend-toplevel-env! 'error-handler error-handler)
  (extend-toplevel-env! 'reset reset)
  (extend-toplevel-env! 'reset-handler reset-handler)
  (extend-toplevel-env! 'exit exit)
  (extend-toplevel-env! 'add-exit-procedure! add-exit-procedure!)
  (extend-toplevel-env! 'add-init-procedure! add-init-procedure!)
  (extend-toplevel-env! 'eval eval)

  ;; debugging.

  (extend-toplevel-env! 'error-continuation error-continuation)
  (extend-toplevel-env! 'current-continuation-structure 
			current-continuation-structure)
  ;; property lists

  (extend-toplevel-env! 'getprop getprop)
  (extend-toplevel-env! 'putprop putprop)
  (extend-toplevel-env! 'remprop remprop)

  ;; vector-like

  (extend-toplevel-env! 'vector-like? vector-like?)
  (extend-toplevel-env! 'vector-like-length vector-like-length)
  (extend-toplevel-env! 'vector-like-ref vector-like-ref)
  (extend-toplevel-env! 'vector-like-set! vector-like-set!)

  ;; bytevectors

  (extend-toplevel-env! 'bytevector? bytevector?)
  (extend-toplevel-env! 'bytevector-length bytevector-length)
  (extend-toplevel-env! 'make-bytevector make-bytevector)
  (extend-toplevel-env! 'bytevector-ref bytevector-ref)
  (extend-toplevel-env! 'bytevector-set! bytevector-set!)
  (extend-toplevel-env! 'bytevector-equal? bytevector-equal?)
  (extend-toplevel-env! 'bytevector-fill! bytevector-fill!)
  (extend-toplevel-env! 'bytevector-copy bytevector-copy)

  (extend-toplevel-env! 'bytevector-like? bytevector-like?)
  (extend-toplevel-env! 'bytevector-like-length bytevector-like-length)
  (extend-toplevel-env! 'bytevector-like-ref bytevector-like-ref)
  (extend-toplevel-env! 'bytevector-like-set! bytevector-like-set!)
  (extend-toplevel-env! 'bytevector-like-equal? bytevector-like-equal?)
  (extend-toplevel-env! 'bytevector-like-copy bytevector-like-copy)

  ;; Support for rewriter and for macro expansion.

  (extend-toplevel-env! 'macro-expand macro-expand)
  (extend-toplevel-env! '%list list)
  (extend-toplevel-env! '%list->vector list->vector)
  (extend-toplevel-env! '%cons cons)
  (extend-toplevel-env! '%append append)
  (extend-toplevel-env! '%make-promise %make-promise)

  ;; system performance and interface

  (extend-toplevel-env! 'run-with-stats run-with-stats)
  (extend-toplevel-env! 'run-benchmark run-benchmark)
  (extend-toplevel-env! 'display-memstats display-memstats) ; mostly obsolete
  (extend-toplevel-env! 'collect collect)
  (extend-toplevel-env! 'gcctl gcctl)
  (extend-toplevel-env! 'memstats memstats)
  (extend-toplevel-env! 'stats-dump-on stats-dump-on)
  (extend-toplevel-env! 'stats-dump-off stats-dump-off)
  (extend-toplevel-env! 'system-function system-function)

  ;; environment interface

  (extend-toplevel-env! 'command-line-arguments command-line-arguments)
  (extend-toplevel-env! 'getenv getenv)
  (extend-toplevel-env! 'dump-heap dump-heap)
  (extend-toplevel-env! 'dump-interactive-heap dump-interactive-heap)
  (extend-toplevel-env! 'system system)

  ;; miscellaneous extensions

;  (extend-toplevel-env! 'rep-loop rep-loop)
;  (extend-toplevel-env! 'rep-loop-startup rep-loop-startup)
  (extend-toplevel-env! 'repl-display-procedure repl-display-procedure)
  (extend-toplevel-env! 'load-noisily load-noisily)
  (extend-toplevel-env! 'load-quietly load-quietly)
  (extend-toplevel-env! 'typetag typetag)
  (extend-toplevel-env! 'typetag-set! typetag-set!)
  (extend-toplevel-env! '**newline** **newline**)
  (extend-toplevel-env! 'unspecified unspecified)
  (extend-toplevel-env! 'undefined undefined)

  ;; bignum debugging

  (extend-toplevel-env! 'bigdump bigdump)
  (extend-toplevel-env! 'bigdump* bigdump*)
  (extend-toplevel-env! 'big-divide-digits big-divide-digits)

  ;; flonum debugging

  (extend-toplevel-env! 'float-significand float-significand)
  (extend-toplevel-env! 'float-exponent float-exponent)

  #t)

; eof
