(define (run-env-tests)
  (newline)
  (display "****************************************") (newline)
  (display "Environments") (newline)
  (newline)
  (and (environment-variables-test)))


(define (environment-variables-test)
  (all-same? (environment-variables (scheme-report-environment 5))
	     *r5rs-names*))

(define (all-same? l1 l2)
  (let ((ok #t))
    (do ((l l1 (cdr l)))
	((null? l))
      (if (not (memq (car l) l2))
	  (begin (display "Unmatched ")
		 (display (car l))
		 (display " in arg1")
		 (newline)
		 (set! ok #f))))
    (do ((l l2 (cdr l)))
	((null? l))
      (if (not (memq (car l) l1))
	  (begin (display "Unmatched ")
		 (display (car l))
		 (display " in arg2")
		 (newline)
		 (set! ok #f))))
    ok))

(define *r5rs-names*
  '(string->number eq? magnitude current-output-port char<? round member eqv?
    quotient load number->string caaar floor atan length string<=? cdaar
    force read-char gcd number? char<=? vector-set! vector->list memv
    vector-ref * modulo + inexact? caaadr cadar - / list cdaadr cddar string=?
    display string->list < set-car! = expt > make-rectangular char-upcase
    string-copy cadadr caar caaaar sqrt real-part char-whitespace? char-ready?
    cddadr cdar cdaaar make-string substring string-ci=? open-output-file
    string-fill! asin call-with-input-file list->string odd? char=? ceiling
    log string-append cadaar cadr caadr rationalize char-ci<=? list? cddaar
    cddr cdadr %append string-ci<? lcm vector? string->symbol even?
    call-with-current-continuation input-port? assv newline tan caddr caaddr
    string-set! boolean? negative? string>=? string-ci>? null? cdddr cdaddr
    string<? string-ci<=? string-ref map char-downcase equal? complex? min
    exact? char-ci<? make-polar car char? sin vector char-lower-case? caadar
    cadddr cdr abs string-ci>=? char-ci>=? write cdadar cddddr angle
    call-with-output-file %list->vector write-char append output-port? %list
    denominator vector-length char-ci=? char>? memq procedure?
    with-output-to-file char-upper-case? caddar inexact->exact rational?
    cdddar char>=? cos symbol->string list-tail exp char-ci>? numerator
    assoc list-ref symbol? max %make-promise string? pair? char->integer
    not positive? char-alphabetic? imag-part acos zero? eof-object?
    integer->char close-input-port close-output-port exact->inexact string>?
    list->vector string make-vector char-numeric? set-cdr! apply peek-char
    %cons truncate open-input-file current-input-port reverse
    with-input-from-file cons <= assq >= integer? real? vector-fill! read
    for-each remainder string-length dynamic-wind values call-with-values
    scheme-report-environment null-environment interaction-environment eval))

