; Chez-compatible structures, built on top of the record system.
;
; We use environment-set! to circumvent problems with the high-level
; macro system; see defstruct1.sch for another approach.  What we really
; want is token pasting.
;
; In particular, this definition only works on the top level, whereas
; the old definition would work in a lexical scope.

(define-syntax define-structure
  (syntax-rules ()
    ((define-structure name f1 ...)
     (let ()
       (define (%%constructor-name x)
	 (string->symbol (string-append "make-" (symbol->string x))))

       (define (%%predicate-name x)
	 (string->symbol (string-append (symbol->string x) "?")))

       (define (%%getter-name x field)
	 (string->symbol
	  (string-append (symbol->string x) "-" (symbol->string field))))

       (define (%%setter-name x field)
	 (string->symbol
	  (string-append
	   "set-" (symbol->string x) "-" (symbol->string field) "!")))

       (let ((t (make-record-type 'name '(f1 ...))))
	 (environment-set! (interaction-environment)
			   (%%predicate-name 'name)
			   (record-predicate t))
	 (environment-set! (interaction-environment)
			   (%%constructor-name 'name)
			   (record-constructor t))
	 (environment-set! (interaction-environment)
			   (%%getter-name 'name 'f1)
			   (record-accessor t 'f1))
	 ...
	 (environment-set! (interaction-environment)
			   (%%setter-name 'name 'f1)
			   (record-updater t 'f1))
	 ...)))))

