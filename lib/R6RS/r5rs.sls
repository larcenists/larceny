;; Nonstandard R5RS library:

(library (r5rs)  
  (export 
   
   ;; core primitives
   
   set!
   
   ;; rnrs base
   
   begin if lambda quote and or
   define define-syntax let-syntax letrec-syntax
   ...
   
   let let* letrec
   case cond else =>
   quasiquote unquote unquote-splicing
   syntax-rules 
   
   * + - / < <= = > >= abs acos append apply asin atan 
   boolean? call-with-current-continuation 
   call-with-values car cdr caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar
   cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   ceiling char? char->integer char=? char<? char>? char<=? char>=?
   complex? cons cos 
   denominator dynamic-wind 
   eq? equal? eqv? even? exact? exp expt floor for-each
   gcd imag-part inexact? integer->char integer?
   lcm length list list->string
   list->vector list-ref list-tail list? log magnitude make-polar
   make-rectangular make-string make-vector map max min
   negative? not null? number->string number? numerator
   odd? pair? 
   positive? procedure? rational? rationalize
   real-part real? reverse round
   sin sqrt string string->list string->number string->symbol
   string-append 
   string-copy string-length string-ref string<=? string<?
   string=? string>=? string>? string? substring symbol->string symbol? tan
   truncate values vector vector->list
   vector-fill! vector-length vector-ref vector-set! vector? zero?
   
   ;; rnrs eval
   
   eval
   
   ;; rnrs load
   
   load
   
   ;; rnrs control
   
   do
   
   ;; rnrs io simple
   
   call-with-input-file call-with-output-file 
   close-input-port close-output-port current-input-port current-output-port
   display eof-object? newline open-input-file open-output-file peek-char
   read read-char with-input-from-file with-output-to-file write write-char
   
   ;; rnrs unicode
   
   char-upcase char-downcase char-ci=? char-ci<? char-ci>?
   char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
   char-upper-case? char-lower-case? string-ci=? string-ci<? string-ci>?
   string-ci<=? string-ci>=?
   
   ;; rnrs mutable pairs
   
   set-car! set-cdr!
   
   ;; rnrs lists
   
   assoc assv assq member memv memq
   
   ;; rnrs mutable-strings
   
   string-set! string-fill!
   
   ;; rnrs r5rs
   
   null-environment scheme-report-environment delay force
   exact->inexact inexact->exact quotient remainder modulo)
  
  ;; Not necessary to use only and except here, but keep
  ;; them because they contain useful information.
  
  (import
   (only (core primitives) set!)
   (except (rnrs base)
           set! ; because should not be exported for expand
           _ letrec* let-values let*-values identifier-syntax
           real-valued? rational-valued? integer-valued?
           exact inexact finite? infinite?
           nan? div mod div-and-mod div0 mod0 div0-and-mod0
           exact-integer-sqrt boolean=?
           symbol=? string-for-each vector-map vector-for-each
           error assertion-violation
           call/cc)
   (only (rnrs eval) eval)
   (only (rnrs load) load)
   (only (rnrs control) do)
   (only (rnrs io simple)
         call-with-input-file call-with-output-file 
         close-input-port close-output-port
         current-input-port current-output-port
         display eof-object? newline open-input-file open-output-file peek-char
         read read-char with-input-from-file
         with-output-to-file write write-char)
   (only (rnrs unicode)
         char-upcase char-downcase char-ci=? char-ci<? char-ci>?
         char-ci<=? char-ci>=? char-alphabetic? char-numeric? char-whitespace?
         char-upper-case? char-lower-case? string-ci=? string-ci<? string-ci>?
         string-ci<=? string-ci>=?)
   (only (rnrs mutable-pairs) set-car! set-cdr!)
   (only (rnrs lists) assoc assv assq member memv memq)
   (only (rnrs mutable-strings) string-set! string-fill!)
   (rnrs r5rs))
  )

