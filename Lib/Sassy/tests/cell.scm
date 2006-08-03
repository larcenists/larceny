(macro define-cell
       (lambda (name init)
	 `(begin (macro cell-tag "CELL")
		  (data (label ,name (dwords cell-tag ,init)))
		  (macro ,(string->symbol
			   (string-append (symbol->string name) "-ref"))
			 (& ,name 4)))))
(define-cell foo 100)

(entry _start)

(text (label _start (mov ebx foo-ref)
	      (mov eax 1)
	      (int #x80)))

