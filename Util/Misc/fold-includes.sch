; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Takes a C header and textually includes all non-system include files
; that the file includes.  Useful for packaging twobit.h into a file
; that doesn't depend on the source tree.

; Depends on

'(begin (load "Auxlib/list.sch")
	(load "Auxlib/io.sch")
	(load "Auxlib/string.sch"))

; For example

'(fold-include-file "twobit.h" 
		    '("Rts/Standard-C/" "Rts/Sys/" "Rts/Build/") 
		    "twobit.out")

(define (fold-include-file input-file directories output-file)
  (call-with-output-file output-file
    (lambda (out)

      ; If #include "..." then slurp the file, return #t
      ; If anything else, return #f

      (define (parse-include l)
	(let ((start (substring-match l "\"" 9)))
	  (and start
	       (let ((end (substring-match l "\"" (+ start 1))))
		 (and end
		      (begin (slurp (substring l (+ start 1) end))
			     #t))))))
      
      (define (pick-name file)
	(string-append (or (find (lambda (x)
				   (file-exists? (string-append x file)))
				 (cons "" directories))
			   (error "Couldn't find " file))
		       file))

      (define (slurp input-file)
	(for-each-line-in-file (pick-name input-file)
	 (lambda (l)
	   (if (string-prefix=? l "#include") ; really /^#include/
	       (if (not (parse-include l))
		   (begin (display l out)
			  (newline out)))
	       (begin (display l out)
		      (newline out))))))

      (slurp input-file))))

; eof
