; Larceny -- Sparc assembler load utility.
; $Id: asmload.sch,v 1.1.1.1 1998/11/19 21:51:58 lth Exp $
;
; lth@cs.uoregon.edu / August 22, 1995 
;
; March 31, 1996
;
; For Larceny: Load the Sparc assembler.
; If a path is given, it is used. Otherwise, "." is assumed.

(define (asmload . rest)
  (let ((path  (if (null? rest) "./" (car rest)))
	(files '("sparcutil.fasl"
		 "asmutil.fasl" 
		 "sparcasm.fasl"
		 "gen-msi.fasl" 
		 "gen-prim.fasl" 
		 "switches.fasl"  
		 ; "sparcdis.fasl"
		 )))

    (define (do-load fn)
      (let ((fn (string-append path fn)))
	(display "; Loading ") (display fn) (newline)
	(load fn)))

    (for-each do-load files)))

(define (asmload-all . rest)

  (define (loadit fn)
    (display "; Loading ") (display fn) (newline)
    (load fn))
  
  (loadit "Larceny/compat.fasl")
  (set! compat:write-bytevector compat:write-bytevector-as-vector)
  (loadit "Build/schdefs.h")
  (loadit "Compiler/switches.sch")
  (loadit "Compiler/twobit.imp.fasl")
  (loadit "Compiler/pass5p1.fasl")
  (loadit "Compiler/pass5p2sparc.fasl")
  (loadit "Compiler/peepopt.fasl")
  (loadit "Compiler/makefasl.fasl")
  (apply asmload rest))

; Scaffolding code!

(define (define-inline name proc)
  (display "Ignoring inline ")
  (display name)
  (newline))

(define compat:sort 
  (lambda (l p) (error "compat:sort: not defined (asmload)")))

(define listify? #t)

(define (lap2fasl name)
  (call-with-output-file (string-append name ".fasl")
    (lambda (po)
      (call-with-input-file (string-append name ".lap")
	(lambda (pi)
	  (do ((item (read pi) (read pi)))
	      ((eof-object? item) #t)
	    (dump-fasl-segment-to-port (assemble item) po)))))))

(define (lap2lop name)
  (call-with-output-file (string-append name ".lop")
    (lambda (po)
      (call-with-input-file (string-append name ".lap")
	(lambda (pi)
	  (do ((item (read pi) (read pi)))
	      ((eof-object? item) #t)
	    (write-lop (assemble item) po)))))))

; eof
