; Autoload facility
; Usage:  (autoload filename name1 name2 ...)
;
; The autoload syntax creates top-level bindings for procedures
; that, when called, will load the file and override those bindings.

; Macros that work has a bug that prevents this from working. (?)

;(define-syntax autoload 
;  (syntax-rules ()
;    ((autoload filename name)
;     (define name
;       (lambda args
;	 (load filename)
;	 (apply name args))))
;    ((autoload filename name1 name2 name3 ...)
;     (begin (autoload filename name1)
;	    (autoload filename name2 name3 ...)))))
;
;(autoload "Experimental/debug.sch" debug)

; Here's another manifestation of the bug:
;
;(define-syntax glop (syntax-rules () ((glop) (define x (lambda w w)))))
;(macro-expand '(begin (glop) (glop) (glop)))
;
; Even simpler:
; (macro-expand '(glop))
;
; But this works:
; (macro-expand '(let () (glop) 33))

; Temporary workaround (works OK)
; Also accepts multiple files.

(define (autoload file . names)
  (for-each
   (lambda (name)
     (environment-set! (interaction-environment)
		       name
		       (lambda args
			 (if (string? file)
			     (load file)
			     (for-each load file))
			 (apply (environment-get (interaction-environment)
						 name)
				args))))
   names))

; eof
