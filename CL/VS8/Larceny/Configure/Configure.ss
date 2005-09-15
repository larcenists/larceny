(define-syntax step
	(syntax-rules ()
		((step exp)
		 (begin (display 'exp) (newline) exp))
		((step exp exps ...)
		 (begin (step exp) (step exps ...)))))
(step
 (current-directory "..\\..\\..\\..")
 (if (not (directory-exists? "Rts\\Build")) (make-directory "Rts\\Build"))
 (load "util\\dotnet.sch")
 (larceny-setup "MzScheme" 'win32 'little)
 (build-config-files)
)
