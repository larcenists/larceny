; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Testing the environments.

(define (run-env-tests)
  (allof "environments"
;   (test-make-environment)
   (test-environment-variables)))

(define (test-make-environment)
  ...)

(define (test-environment-variables)
  (test-all-same? "(environment-variables (scheme-report-environment 5))"
		  (environment-variables (scheme-report-environment 5))
		  *r5rs-names*))

(define *r5rs-names*
  '(* + - .append .cons .list .list->vector .make-promise / < <= = > >= abs
    acos angle append apply asin assoc assq assv atan boolean? caaaar caaadr
    caaar caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr
    cadr call-with-current-continuation call-with-input-file
    call-with-output-file call-with-values car cdaaar cdaadr cdaar cdadar
    cdaddr cdadr cdar cddaar cddadr cddar cdddar cddddr cdddr cddr cdr
    ceiling char->integer char-alphabetic? char-ci<=? char-ci<? char-ci=?
    char-ci>=? char-ci>? char-downcase char-lower-case? char-numeric?
    char-ready? char-upcase char-upper-case? char-whitespace? char<=? char<?
    char=? char>=? char>? char? close-input-port close-output-port complex?
    cons cos current-input-port current-output-port denominator display
    dynamic-wind eof-object? eq? equal? eqv? eval even? exact->inexact
    exact? exp expt floor for-each force gcd imag-part inexact->exact
    inexact? input-port? integer->char integer? interaction-environment lcm
    length list list->string list->vector list-ref list-tail list? load log
    magnitude make-polar make-rectangular make-string make-vector map max
    member memq memv min modulo negative? newline not null-environment null?
    number->string number? numerator odd? open-input-file open-output-file
    output-port? pair? peek-char positive? procedure? quotient rational?
    rationalize read read-char real-part real? remainder reverse round
    scheme-report-environment set-car! set-cdr! sin sqrt string string->list
    string->number string->symbol string-append string-ci<=? string-ci<?
    string-ci=? string-ci>=? string-ci>? string-copy string-fill!
    string-length string-ref string-set! string<=? string<? string=?
    string>=? string>? string? substring symbol->string symbol? tan truncate
    values vector vector->list vector-fill! vector-length vector-ref
    vector-set! vector? with-input-from-file with-output-to-file write
    write-char zero?))

; eof
