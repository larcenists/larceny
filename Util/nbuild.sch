; Util/nbuild.sch
; Host-indepentent loader for Larceny development system.
;
; $Id: nbuild.sch,v 1.6 1997/09/23 20:07:36 lth Exp lth $
;
; Hacked 11 November 1998 by wdc for v0.35.
;
; The only parameter to this module is a procedure "nbuild-parameter"
; that accepts a key and returns a value for the key.
;
; The keys used by nbuild directly are
;   source          the directory that contains makefile.sch
;   compiler        the directory for Twobit and the help system
;   common-asm      the directory for the target-independent assembler
;   sparc-asm       the directory the new SPARC assembler
;   sparc-old       the directory for the old SPARC assembler [UNSUPPORTED]
;   standard-C-asm  the directory for the standard-C assembler
;   util            the directory for utilities and the make system
;   build           the directory that contains schdefs.h
;   compatibility   the directory for the compatibility code
;   host-system     the name of the host system
;   target-machine  a symbol that specifies the target architecture
;   new-assembler?  if target-machine = SPARC: use the new assembler (or not)
;
; There might be other keys used by the compatibility packages.

(define (writeln . x)
  (for-each display x) (newline))

; Compatibility library has been loaded and initialized.

(define (nbuild-load path-ident file)
  (let* ((path (nbuild-parameter path-ident))
	 (fn   (string-append path file)))
    (compat:load fn)))

(writeln "Loading the make utility.")
(nbuild-load 'util "make.sch")

(writeln "Loading Twobit.")
(nbuild-load 'compiler "sets.sch")
(nbuild-load 'compiler "switches.sch")
(nbuild-load 'compiler "pass1.aux.sch")
(case (nbuild-parameter 'target-machine)
  ((sparc)      (nbuild-load 'compiler "sparc.imp.sch"))
  ((standard-C) (nbuild-load 'compiler "standard-C.imp.sch"))
  (else ???))

(nbuild-load 'compiler "copy.sch")
(nbuild-load 'compiler "pass1.sch")
(nbuild-load 'compiler "pass2.aux.sch")
(nbuild-load 'compiler "pass2p1.sch")
(nbuild-load 'compiler "pass2p2.sch")
(nbuild-load 'compiler "pass4.aux.sch")
(nbuild-load 'compiler "pass4p1.sch")
(nbuild-load 'compiler "pass4p2.sch")
(nbuild-load 'compiler "pass4p3.sch")

(if (not (nbuild-parameter 'new-assembler?))
    (begin 
      (writeln "Loading the old generic assembler.")
      (with-optimization 2
        (lambda ()
	  (nbuild-load 'sparc-old "assembler.sch")
	  (nbuild-load 'sparc-old "peepopt.sch"))))
    (begin
      (writeln "Loading the common assembler.")
      (nbuild-load 'common-asm "pass5p1.sch")
      (nbuild-load 'common-asm "asmutil.sch")
      (nbuild-load 'common-asm "asmutil32be.sch")  ; For now
      (nbuild-load 'common-asm "asmutil32.sch")))

(writeln "Loading the back-end header files.")
(case (nbuild-parameter 'target-machine)
  ((sparc)      (nbuild-load 'build "schdefs.h"))
  ((standard-C) #t)
  (else ???))

(cond ((not (nbuild-parameter 'new-assembler?))
       (if (not (eq? 'SPARC (nbuild-parameter 'target-machine)))
	   (error "Old assembler can only do SPARC output."))
       (writeln "Loading the old SPARC assembler and code generator.")
       (with-optimization 2
	 (lambda ()
	   (nbuild-load 'sparc-old "sparcasm.sch")))
       (nbuild-load 'sparc-old "gen-msi.sch")
       (nbuild-load 'sparc-old "gen-prim.sch")
       (nbuild-load 'sparc-old "asmutil.sch")
       (nbuild-load 'sparc-old "switches.sch"))
      ((eq? 'SPARC (nbuild-parameter 'target-machine))
       (writeln "Loading the new SPARC assembler and code generator.")
       (nbuild-load 'sparc-asm "pass5p2.sch")
       (nbuild-load 'sparc-asm "peepopt.sch")
       (nbuild-load 'sparc-asm "sparcutil.sch")
       (nbuild-load 'sparc-asm "sparcasm.sch")
       (nbuild-load 'sparc-asm "gen-msi.sch")
       (nbuild-load 'sparc-asm "gen-prim.sch")
       (nbuild-load 'sparc-asm "switches.sch"))
      ((eq? 'standard-C (nbuild-parameter 'target-machine))
       (writeln "Loading the standard-C assembler.")
       (nbuild-load 'standard-C-asm "pass5p2.sch")
       (nbuild-load 'standard-C-asm "switches.sch"))
      (else
       ???))

(nbuild-load 'compiler "patch0.sch")

(case (nbuild-parameter 'target-machine)
  ((SPARC)
   (if (not (nbuild-parameter 'new-assembler?))
       (begin 
	 (writeln "Loading old SPARC disassembler.")
	 (nbuild-load 'sparc-old "sparcdis.sch"))
       (begin
	 (writeln "Loading new SPARC disassembler.")
	 (nbuild-load 'sparc-asm "sparcdis.sch"))))
  ((standard-C)
   (writeln "(No disassembler for standard-C)"))
  (else ???))

(writeln "Loading bootstrap heap dumper.")
(with-optimization 3
  (lambda ()
    (nbuild-load 'common-asm "dumpheap.sch")))

(writeln "Loading drivers and utilities.")
(nbuild-load 'compiler "compile313.sch")
(nbuild-load 'compiler "printlap.sch")
(nbuild-load 'common-asm "makefasl2.sch")

(writeln "Loading makefile.")
(nbuild-load 'source "makefile.sch")

(writeln "Loading help.")
(nbuild-load 'compiler "help.sch")
(initialize-help (nbuild-parameter 'compiler))

;;; Initialize Twobit

(twobit-target-architecture (nbuild-parameter 'target-machine))
(issue-warnings #f)			; Annoying
(include-source-code #f)		; Conserve space
(include-variable-names #t)		; Debugging
(include-procedure-names #t)		; Debugging
(fast-safe-code)			; Performance

;;; And they're off!

(writeln "Welcome. Type (help) for help.")

; eof
