; -*- Scheme -*-
;
; $Id: make.sch,v 1.3 1992/06/10 09:05:07 lth Exp lth $
;
; DESCRIPTION
;   A "make" facility for Chez Scheme.
;
;   The input to the "make" procedure is a target and a list of dependencies
;   and associated make commands. The modification dates on the files are 
;   checked, and the necessary files are rebuilt using the supplied 
;   procedures.
;
;   A dependency is a list of file names. The first element of the list is 
;   the target, the rest are the sources. If any of the sources are newer 
;   than the target, the list of sources is passed to the make command 
;   associated with the dependency. 
;
;   A make command is a procedure of one argument: the list of sources.
;
; GRAMMAR FOR DEPENDENCY LIST
;   make  -->  ( <spec> ... )
;   spec  -->  ( <dep> <cmd> )
;   dep   -->  ( <file> <file> ... )
;   cmd   -->  <procedure-of-one-arg>
;
; EXAMPLE
;   > (make "../Lib/reader.lop"
;            `((("../Lib/reader.lap" "../Lib/reader.sch")
;               ,(lambda (x) (compile313 x)))
;              (("../Lib/reader.lop" "../Lib/reader.lap")
;               ,(lambda (x) (assemble313 x)))))
;
; AUTHOR
;   Lars Thomas Hansen
;
; FILES
;   make.sch      This file.
;
; DEPENDENCIES
;   Depends on non-standard procedures:
;     (file-exists? filename)             =>  boolean
;     (file-modification-time filename)   => integer
;
; COMMENTS
;   No generic patterns (like suffixes in UNIX make) are available.
;   They would be nice to have, but can be implemented on a higher level.

; Load path for the auxiliary object files for this program.
; Must end in a slash.

;(define make-load-path compilerdir)

; The make command proper.

(define (make target dependencies)

  (define (find-target target)
    (let loop ((d dependencies))
      (cond ((null? d)
	     #f)
	    ((string=? target (caaar d))
	     (car d))
	    (else
	     (loop (cdr d))))))

  (define (make target)
    (let ((dep (find-target target)))
      (cond ((and (not dep) (not (file-exists? target)))
	     (error 'make "Don't know how to make ~a" target))
	    ((not dep)
	     #t)
	    (else
	     (for-each make (cdar dep))
	     (let ((times (map file-modification-time (car dep))))
	       (if (ormap (lambda (x)
			    (< (car times) x))
			  (cdr times))
		   (begin (display (format "Making ~a~%" target))
			  ((cadr dep) target (cdar dep)))))))))

  (make target))

; eof
