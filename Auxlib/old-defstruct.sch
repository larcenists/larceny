; Chez-compatible structures, built on top of the record system.
;
; This is totally gross.  It was Doug's idea.
; If we had token pasting, we'd be home free.

(define-syntax define-structure
  (syntax-rules ()
    ((define-structure name f1 ...)
     (eval 
      `(begin (define ,(%%type-name 'name)
		(make-record-type 'name '(f1 ...)))
	      (define ,(%%predicate-name 'name)
		(record-predicate ,(%%type-name 'name)))
	      (define ,(%%constructor-name 'name)
		(record-constructor ,(%%type-name 'name)))
	      (define ,(%%getter-name 'name 'f1)
		(record-accessor ,(%%type-name 'name) 'f1))
	      ...
	      (define ,(%%setter-name 'name 'f1)
		(record-updater ,(%%type-name 'name) 'f1))
	      ...)))))

(define (%%type-name x)
  (string->symbol (string-append "<" (symbol->string x) ">")))

(define (%%constructor-name x)
  (string->symbol (string-append "make-" (symbol->string x))))

(define (%%predicate-name x)
  (string->symbol (string-append (symbol->string x) "?")))

(define (%%getter-name x field)
  (string->symbol (string-append (symbol->string x)
				 "-"
				 (symbol->string field))))

(define (%%setter-name x field)
  (string->symbol (string-append "set-"
				 (symbol->string x)
				 "-"
				 (symbol->string field)
				 "!")))

; eof
