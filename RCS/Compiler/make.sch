; -*- Scheme -*-
;
; $Id: make.sch,v 1.1 92/01/16 13:27:49 lth Exp Locker: lth $
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
;   mtime.o       Object file for the mtime procedure which returns 
;                 the modification time of a file.
;   mtime.c       Source for mtime.o
;
; BUGS
;   Depends a little on the Chez Scheme foreign-function facility, and is
;   consequently nonportable. This is not a major issue; there's no
;   way to make it portable anyway.
;
; COMMENTS
;   Only the UNIX interface is specific to Chez Scheme.
;
;   No generic patterns (like suffixes in UNIX make) are available.
;   They would be nice to have, but can be implemented on a higher level.


; Load path for the auxiliary object files for this program.
; Must end in a slash.

(define make-load-path "/home/systems/lth/scheme313/Compiler/")


; The make command proper.

(define (make target dependencies)

  (define mtime unix$mtime)

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
      (cond ((and (not dep) (= (unix$access target 0) -1))
	     (error 'make "Don't know how to make ~a" target))
	    ((not dep)
	     #t)
	    (else
	     (for-each make (cdar dep))
	     (let ((times (map mtime (car dep))))
	       (if (ormap (lambda (x)
			    (< (car times) x))
			  (cdr times))
		   (begin (display (format "Making ~a~%" target))
			  ((cadr dep) (cdar dep)))))))))

  (make target))


; UNIX interface.
; The load check is a hack which happens to work in Chez v4.

(if (not (bound? 'make-has-loaded-foreign))
    (load-foreign (string-append make-load-path "mtime.o")))
(set! make-has-loaded-foreign #t)

; returns accessibility of file.

(define unix$access
  (foreign-procedure "access" (string integer-32) integer-32))

; returns modification time of file.

(define unix$mtime
  (foreign-procedure "mtime" (string) unsigned-32))
