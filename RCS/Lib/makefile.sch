; -*- Scheme -*-
;
; Makefile to build some arbitary initial heap from the library files.
;
; $Id: makefile.sch,v 1.3 92/02/10 12:13:59 lth Exp Locker: lth $
;
; USAGE:
;
;     (make-heap <heap-name> <options>)
;
;   where <heap-name> is a string and <options> is a list of switches or
;   additional file names (strings) to link into the heap at build time.
;
;   Currently accepted options are 'global-symbols, 'listify, and 'global-refs.
;
;   - Global-symbols initializes the cdr of a global cell to the symbol which
;     is the name of the cell. This option controls the heap dumper.
;
;   - Listify turns source listing on during assembly. This option controls
;     the assembler.
;
;   - Global-refs turns global variable checking on: if a global is loaded and
;     has the value #!unspecified, then an exception will be raised. This
;     option controls the assembler.
;
; USAGE:
;
;     (show-heap-deps <heap-name> <filename> ...)
;
;   Returns the dependence structure as it will be passed to "make" if this 
;   were a call to "make-heap". No options are allowed.

(define make-heap 'make-heap)
(define show-heap-deps 'heap-deps)

; useful aliases

(define (make-ctak-heap)
  (make-heap "../ctak.heap" 'global-symbols 'global-refs "../Lib/ctak.lop"))

(define (make-test-heap)
  (make-heap "../test.heap" 'global-refs "../Lib/main.lop"))

(let ()

  (define (dump-the-heap file files)
    (let ((fn (string-append file ".map")))
      (delete-file fn)
      (call-with-output-file fn
	(lambda (p)
	  (let ((q (apply dump-heap (cons file files))))
	    (collect)
	    (pretty-print q p))))))

  (define (assemble x)
    (assemble313 (car x)))

  (define (compile x)
    (compile313 (car x)))

  ; *All* simple file dependencies go here.
  ; Some shorthands would be lovely.

  (define deps
    `((("../Lib/unixio.lop" "../Lib/unixio.mal") ,assemble)
      (("../Lib/rawapply.lop" "../Lib/rawapply.mal") ,assemble)
      (("../Lib/integrable-procs.lop" "../Lib/integrable-procs.lap")
       ,assemble)
      (("../Lib/integrable-procs.lap" "../Lib/integrable-procs.scm")
       ,compile)
      (("../Lib/xlib.lop" "../Lib/xlib.lap") ,assemble)
      (("../Lib/xlib.lap" "../Lib/xlib.sch") ,compile)
      (("../Lib/oblist.lop" "../Lib/oblist.lap") ,assemble)
      (("../Lib/oblist.lap" "../Lib/oblist.sch") ,compile)
      (("../Lib/library.lop" "../Lib/library.lap") ,assemble)	  
      (("../Lib/library.lap" "../Lib/library.sch") ,compile)
      (("../Lib/print.lop" "../Lib/print.lap") ,assemble)
      (("../Lib/print.lap" "../Lib/print.sch") ,compile)
      (("../Lib/schemeio.lop" "../Lib/schemeio.lap") ,assemble)
      (("../Lib/schemeio.lap" "../Lib/schemeio.scm") ,compile)
      (("../Lib/numberparser.lop" "../Lib/numberparser.lap") ,assemble)
      (("../Lib/numberparser.lap" "../Lib/numberparser.sch") ,compile)
      (("../Lib/reader.lop" "../Lib/reader.lap") ,assemble)
      (("../Lib/reader.lap" "../Lib/reader.sch") ,compile)
      (("../Lib/strings.lop" "../Lib/strings.lap") ,assemble)
      (("../Lib/strings.lap" "../Lib/strings.sch") ,compile)
      (("../Lib/number2string.lop" "../Lib/number2string.lap") ,assemble)
      (("../Lib/number2string.lap" "../Lib/number2string.sch") ,compile)
      (("../Lib/bellerophon.lop" "../Lib/bellerophon.lap") ,assemble)
      (("../Lib/bellerophon.lap" "../Lib/bellerophon.sch") ,compile)
      (("../Lib/flonum-stuff.lop" "../Lib/flonum-stuff.lap") ,assemble)
      (("../Lib/flonum-stuff.lap" "../Lib/flonum-stuff.sch") ,compile)
      (("../Lib/go.lop" "../Lib/go.lap") ,assemble)
      (("../Lib/go.lap" "../Lib/go.sch") ,compile)
      (("../Lib/Sparc/glue.lop" "../Lib/Sparc/glue.mal") ,assemble)
      (("../Lib/ctak.lop" "../Lib/ctak.lap") ,assemble)
      (("../Lib/ctak.lap" "../Lib/ctak.sch") ,compile)
      (("../Lib/ctest.lop" "../Lib/ctest.lap") ,assemble)
      (("../Lib/ctest.lap" "../Lib/ctest.sch") ,compile)
      (("../feb7/fib.lop" "../feb7/fib.mal") ,assemble)
;      (("../Lib/millicode-support.lop" "../Lib/millicode-support.mal") 
;       ,assemble)
      (("../Lib/millicode-support.lop" "../Lib/millicode-support.lap") 
       ,assemble)
      (("../Lib/millicode-support.lap" "../Lib/millicode-support.sch") 
       ,compile)
      (("../Lib/millicode-support-dummies.lop" 
	"../Lib/millicode-support-dummies.lap") ,assemble)
      (("../Lib/millicode-support-dummies.lap" 
	"../Lib/millicode-support-dummies.sch") ,compile)
      (("../Lib/bignums.lop" "../Lib/bignums.lap") ,assemble)
      (("../Lib/bignums.lap" "../Lib/bignums.scm") ,compile)
      (("../Lib/ratnums.lop" "../Lib/ratnums.lap") ,assemble)
      (("../Lib/ratnums.lap" "../Lib/ratnums.scm") ,compile)
      (("../Lib/rectnums.lop" "../Lib/rectnums.lap") ,assemble)
      (("../Lib/rectnums.lap" "../Lib/rectnums.scm") ,compile)
      (("../Lib/contagion.lop" "../Lib/contagion.lap") ,assemble)
      (("../Lib/contagion.lap" "../Lib/contagion.scm") ,compile)
      (("../Lib/number.lop" "../Lib/number.lap") ,assemble)
      (("../Lib/number.lap" "../Lib/number.sch") ,compile)
      (("../Lib/main.lop" "../Lib/main.lap") ,assemble)
      (("../Lib/main.lap" "../Lib/main.sch") ,compile)
      (("../Lib/debug.lop" "../Lib/debug.lap") ,assemble)
      (("../Lib/debug.lap" "../Lib/debug.sch") ,compile)
      ))

  ; Basic make command for a heap.
  ; "Other-sources" are additional source files to be loaded.
  ;
  ; Be warned that load order *is* important. This can be partly circumvented
  ; by good use of 'install-X' procedures, like in the reader and the oblist.

  (define (heap-deps heap-name other-sources)
    `(((,heap-name

	; fundamental 

	"../Lib/unixio.lop"
	"../Lib/integrable-procs.lop"
	"../Lib/rawapply.lop" 
        "../Lib/Sparc/glue.lop"

	; random stuff which supposedly works

	"../Lib/xlib.lop"
	"../Lib/strings.lop"
	"../Lib/library.lop"
	"../Lib/oblist.lop"
	"../Lib/millicode-support.lop"
	"../Lib/millicode-support-dummies.lop"

	; basic i/o

	"../Lib/schemeio.lop"     ; needs testing
	"../Lib/print.lop"

	; basic arithmetic

	"../Lib/number.lop"

	; stuff which is not at all trusted

	"../Lib/bignums.lop"
	"../Lib/ratnums.lop"
	"../Lib/rectnums.lop"
	"../Lib/flonum-stuff.lop"
	"../Lib/number2string.lop"
;;	"../Lib/bellerophon.lop"
	"../Lib/numberparser.lop"
	"../Lib/reader.lop"
	"../Lib/contagion.lop"
	"../Lib/debug.lop"

	; other application files

        ,@other-sources

	; driver

	"../Lib/go.lop")
       ,(lambda (x) (dump-the-heap heap-name x)))
      ,@deps))

  ; doit

  (set! make-heap
	(lambda (heap-name . switches)
	  (set! listify? #f)
	  (set! generate-global-symbols? #f)
	  (set! emit-undef-check? #f)

	  (let loop ((switches switches) (others '()))
	    (cond ((null? switches) 
		   (make heap-name (heap-deps heap-name (reverse others))))
		  ((eq? (car switches) 'listify)
		   (set! listify? #t)
		   (loop (cdr switches) others))
		  ((eq? (car switches) 'global-symbols)
		   (set! generate-global-symbols? #t)
		   (loop (cdr switches) others))
		  ((eq? (car switches) 'global-refs)
		   (set! emit-undef-check? #t)
		   (loop (cdr switches) others))
		  ((string? (car switches))
		   (loop (cdr switches) (cons (car switches) others)))
		  (else
		   (display "Invalid option ")
		   (display (car switches))
		   (newline)
		   (loop (cdr switches) others))))))

  (set! show-heap-deps
	(lambda (heap-name . others)
	  (heap-deps heap-name others)))

  #t)
