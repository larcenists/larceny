; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; File lists for nbuild et al.  Don't rearrange the lists -- order matters.

(define (nbuild-files path-ident files)
  (let ((path (nbuild-parameter path-ident)))
    (if path
	(map (lambda (fn)
	       (string-append path fn))
	     files)
	'())))

(define *nbuild:twobit-files-1*
  (nbuild-files 'compiler
		'("sets.sch" "switches.sch" "pass1.aux.sch" "pass2.aux.sch"
		  "prefs.sch" "syntaxenv.sch" "syntaxrules.sch" "lowlevel.sch"
		  "expand.sch" "usual.sch" "copy.sch" "pass1.sch")))

(define *nbuild:sparc/twobit-files*
  (nbuild-files 'compiler
		'("sparc.imp.sch")))

(define *nbuild:petit/twobit-files*
  (nbuild-files 'compiler
		'("standard-C.imp.sch")))

(define *nbuild:twobit-files-2*
  (nbuild-files 'compiler
		'("pass2p1.sch" "pass2p2.sch"
		  "pass4.aux.sch" "pass4p1.sch" "pass4p2.sch" "pass4p3.sch"
		  "compile313.sch" "printlap.sch")))

(define *nbuild:common-asm-be*
  (nbuild-files 'common-asm
		'("pass5p1.sch" "asmutil.sch" "asmutil32be.sch" "asmutil32.sch"
		  "makefasl.sch" "dumpheap.sch")))

(define *nbuild:common-asm-el*
  (nbuild-files 'common-asm
		'("pass5p1.sch" "asmutil.sch" "asmutil32el.sch" "asmutil32.sch"
                  "makefasl.sch" "dumpheap.sch")))

(define *nbuild:build-files*
  (nbuild-files 'build '("schdefs.h")))

(define *nbuild:sparcasm-files*
  (nbuild-files 'sparc-asm
		'("pass5p2.sch" "peepopt.sch" "sparcutil.sch" "sparcasm.sch"
		  "gen-msi.sch" "sparcprim-part1.sch" "sparcprim-part2.sch"
		  "sparcprim-part3.sch" "sparcprim-part4.sch" "switches.sch"
		  "sparcdis.sch")))

(define *nbuild:petitasm-files*
  (nbuild-files 'standard-C-asm
		'("pass5p2.sch" "switches.sch")))

(define *nbuild:make-files*
  (append (nbuild-files 'util '("make.sch"))
	  (nbuild-files 'source '("makefile.sch"))))

(define *nbuild:help-files*
  (nbuild-files 'compiler '("help.sch")))

(define *nbuild:sparc-heap-dumper-files* 
  '())

(define *nbuild:petit-heap-dumper-files*
  (nbuild-files 'standard-C-asm '("dumpheap-extra.sch")))

(define (nbuild:twobit-files)
  (append *nbuild:twobit-files-1*
	  (case (nbuild-parameter 'target-machine)
	    ((SPARC)      *nbuild:sparc/twobit-files*)
	    ((Standard-C) *nbuild:petit/twobit-files*)
	    (else (error "nbuild:twobit-files: bad architecture.")))
	  *nbuild:twobit-files-2*))

(define (nbuild:common-asm-files)
  (case (nbuild-parameter 'endianness)
    ((big)    (append *nbuild:common-asm-be* *nbuild:build-files*))
    ((little) (append *nbuild:common-asm-el* *nbuild:build-files*))
    (else (error "nbuild:common-asm-files: big endianness."))))

(define (nbuild:machine-asm-files)
  (case (nbuild-parameter 'target-machine)
    ((SPARC)      *nbuild:sparcasm-files*)
    ((Standard-C) *nbuild:petitasm-files*)
    (else (error "nbuild:machine-asm-files: bad architecture."))))

(define (nbuild:heap-dumper-files)
  (case (nbuild-parameter 'target-machine)
    ((SPARC)      *nbuild:sparc-heap-dumper-files*)
    ((standard-C) *nbuild:petit-heap-dumper-files*)
    (else (error "nbuild:heap-dumper-files: bad architecture."))))

(define (nbuild:utility-files)
  (append *nbuild:make-files* *nbuild:help-files*))

; eof
