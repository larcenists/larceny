; Chez-compatible structures, built on top of the record system.
;
; This is totally gross.  It was Doug's idea.  
;
; What we really want is token pasting.  E.g., +++ is used to paste
; symbols and strings together to yield a symbol.  Then
;    (+++ name "?")     yields name?
;    (+++ "<" name ">") yields <name>
; and so on.  "name" is subject to substitution.
;
; In the following, the token pasting is replaced by use of eval,
; quasiquotations, and quoted names.

(define-syntax define-structure
  (syntax-rules ()
    ((define-structure name f1 ...)
     (eval 
      `(begin (define ,(+++ "<" 'name ">")
		(make-record-type 'name '(f1 ...)))
	      (define ,(+++ 'name "?")
		(record-predicate ,(+++ "<" 'name ">")))
	      (define ,(+++ "make-" 'name)
		(record-constructor ,(+++ "<" 'name ">")))
	      (define ,(+++ 'name "-" 'f1)
		(record-accessor ,(+++ "<" 'name ">") 'f1))
	      ...
	      (define ,(+++ "set-" 'name "-" 'f1 "!")
		(record-updater ,(+++ "<" 'name ">") 'f1))
	      ...
	      )))))

(define (+++ . args)
  (string->symbol
   (apply string-append (map (lambda (x)
			       (if (string? x) x (symbol->string x)))
			     args))))

; eof
