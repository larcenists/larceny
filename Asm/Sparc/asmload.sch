; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Asm/Sparc/asmload.sch -- Sparc assembler load utility.

; Load the Sparc assembler.  If a path is given, it is used. Otherwise, 
; "." is assumed.

(define (asmload . rest)
  (let ((path  (if (null? rest) "./" (car rest)))
	(files '("sparcutil.fasl"
		 "asmutil.fasl" 
		 "sparcasm.fasl"
		 "gen-msi.fasl" 
		 "sparcprim-part1.fasl" 
		 "sparcprim-part2.fasl" 
		 "sparcprim-part3.fasl" 
		 "switches.fasl"  
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
