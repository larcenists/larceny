; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Common code for manipulating sets of compilation and linking
; commands, for Petit Larceny and derived systems that use an external
; program for compiling emitted MAL macros to machine code.
;
; For historical reasons the external program is called a "C compiler"
; but it might just as well be an assembler.

(define *available-compilers* '())  ; Assigned below -- ((tag name obj functions) ...)
(define *current-compiler* #f)      ; Assigned below -- (tag name obj functions)

(define (c-compile-file c-name o-name)
  ((cdr (assq 'compile (cadddr *current-compiler*))) c-name o-name))

(define (c-link-library output-name object-files libraries)
  ((cdr (assq 'link-library (cadddr *current-compiler*))) 
   output-name 
   object-files 
   libraries))

(define (c-link-executable output-name object-files libraries)
  ((cdr (assq 'link-executable (cadddr *current-compiler*))) 
   output-name 
   object-files 
   libraries))

(define (c-link-shared-object output-name object-files libraries)
  ((cdr (assq 'link-shared-object (cadddr *current-compiler*))) 
   output-name 
   object-files
   libraries))

(define (obj-suffix)
  (caddr *current-compiler*))

(define (*append-file-shell-command* x y)
  ((cdr (assq 'append-files (cadddr *current-compiler*))) x y))

(define (define-compiler name tag extension commands)
  (set! *available-compilers*
	(cons (list tag name extension commands)
	      *available-compilers*)))

(define (select-compiler . rest)
  (if (null? rest)
      (begin
	(display "Available compilers:")
	(newline)
	(for-each (lambda (c) 
		    (display (car c))
		    (display "   ")
		    (display (cadr c))
		    (newline))
		  *available-compilers*))
      (let ((probe (assq (car rest) *available-compilers*)))
	(if probe
	    (begin
	      (display "Selecting compiler: ")
	      (display (cadr probe))
	      (newline)
	      (set! *current-compiler* probe))
	    (error "No such compiler: " (car rest)))))
  (unspecified))

(define-compiler 
  "No compiler at all"
  'none
  ".o"
  `((compile 
     . ,(lambda (c-name o-name)
	  (display ">>> MUST COMPILE ")
	  (display c-name)
	  (newline)))
    (link-library
     . ,(lambda (output-name object-files libs)
	  (display ">>> MUST LINK LIBRARY ")
	  (display output-name)
	  (newline)))
    (link-executable
     . ,(lambda (output-name object-files libs)
	  (display ">>> MUST LINK EXECUTABLE ")
	  (display output-name)
	  (newline)))
    (link-shared-object
     . ,(lambda (output-name object-files libs)
	  (display ">>> MUST LINK SHARED OBJECT ")
	  (display output-name)
	  (newline)))
    (append-files 
     . ,append-file-shell-command-portable)))

(select-compiler 'none)

; Utility procedures used by clients

(define (insert-space l)
  (cond ((null? l) l)
        ((null? (cdr l)) l)
        (else (cons (car l) (cons " " (insert-space (cdr l)))))))

(define (execute cmd)
  (display cmd)
  (newline)
  (if (not (= (system cmd) 0))
      (error "COMMAND FAILED.")))
  

; eof
