; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Host-independent loader for the development system.
;
; The only parameter to this module is a procedure "nbuild-parameter"
; that accepts a key and returns a value for the key.
;
; The keys used by nbuild directly are
;   source          the directory that contains makefile.sch
;   compiler        the directory for Twobit and the help system
;   common-asm      the directory for the target-independent assembler
;   util            the directory for utilities and the make system
;   build           the directory that contains schdefs.h
;   compatibility   the directory for the compatibility code
;   host-system     the name of the host system
;   target-machine  a symbol that specifies the target architecture
;                      SPARC           Sparc v8  (32-bit, big endian)
;                      Standard-C      ANSI C
;   endianness      endianness of target, either 'big' or 'little'.
;
; If target-machine = SPARC:
;   sparc-asm       the directory the SPARC assembler
;
; If target-machine = Standard-C
;   standard-C-asm  the directory for the standard-C assembler
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
  ((SPARC)      (nbuild-load 'compiler "sparc.imp.sch"))
  ((Standard-C) (nbuild-load 'compiler "standard-C.imp.sch"))
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

(writeln "Loading the common assembler.")
(nbuild-load 'common-asm "pass5p1.sch")
(nbuild-load 'common-asm "asmutil.sch")
(case (nbuild-parameter 'endianness)
  ((big)    (nbuild-load 'common-asm "asmutil32be.sch"))
  ((little) (nbuild-load 'common-asm "asmutil32el.sch"))
  (else ???))
(nbuild-load 'common-asm "asmutil32.sch")

(writeln "Loading the back-end header files.")
(case (nbuild-parameter 'target-machine)
  ((SPARC)      (nbuild-load 'build "schdefs.h"))
  ((standard-C) (nbuild-load 'build "schdefs.h"))
  (else ???))

(case (nbuild-parameter 'target-machine)
  ((SPARC)
   (writeln "Loading the SPARC assembler and code generator.")
   (nbuild-load 'sparc-asm "pass5p2.sch")
   (nbuild-load 'sparc-asm "peepopt.sch")
   (nbuild-load 'sparc-asm "sparcutil.sch")
   (nbuild-load 'sparc-asm "sparcasm.sch")
   (nbuild-load 'sparc-asm "gen-msi.sch")
   (nbuild-load 'sparc-asm "sparcprim-part1.sch")
   (nbuild-load 'sparc-asm "sparcprim-part2.sch")
   (nbuild-load 'sparc-asm "sparcprim-part3.sch")
   (nbuild-load 'sparc-asm "sparcprim-part4.sch")
   (nbuild-load 'sparc-asm "switches.sch"))
  ((Standard-C)
   (writeln "Loading the standard-C assembler.")
   (nbuild-load 'standard-C-asm "pass5p2.sch")
   (nbuild-load 'standard-C-asm "switches.sch"))
  (else ???))

(nbuild-load 'compiler "patch0.sch")

(case (nbuild-parameter 'target-machine)
  ((SPARC)
   (writeln "Loading SPARC disassembler.")
   (nbuild-load 'sparc-asm "sparcdis.sch"))
  ((standard-C)
   (writeln "(No disassembler for standard-C)"))
  (else ???))

(writeln "Loading bootstrap heap dumper.")
(with-optimization 3
  (lambda ()
    (nbuild-load 'common-asm "dumpheap.sch")
    (case (nbuild-parameter 'target-machine)
      ((SPARC)      #t)
      ((standard-C) (nbuild-load 'standard-C-asm "dumpheap-extra.sch"))
      (else ???))))

(writeln "Loading drivers and utilities.")
(nbuild-load 'compiler "compile313.sch")
(nbuild-load 'compiler "printlap.sch")
(nbuild-load 'common-asm "makefasl.sch")

(writeln "Loading makefile.")
(nbuild-load 'source "makefile.sch")

(writeln "Loading help.")
(nbuild-load 'compiler "help.sch")
(initialize-help (nbuild-parameter 'compiler))

;;; Initialize Twobit

(fast-safe-code)

;;; Initialize assembler

; Nothing yet -- must eventually adjust endianness!

;;; Initialize heap dumper

(dumpheap.set-endianness! (nbuild-parameter 'endianness))

;;; And they're off!

(writeln "Welcome. Type (help) for help.")

; eof
