;; -*- Scheme -*-
;;
;; Makefile to build an initial heap from the library files.
;; WARNING: This file is a mess.
;;
;; $Id: makefile.sch,v 1.7 1992/05/18 05:07:23 lth Exp lth $
;;
;; USAGE:
;;
;;     (make-heap <heap-name> <options>)
;;
;;   where <heap-name> is a string and <options> is a list of switches or
;;   additional file names (strings) to link into the heap at build time.
;;
;;   Currently accepted options are 'global-symbols, 'listify, and 
;;   'global-refs.
;;
;;   - Global-symbols initializes the cdr of a global cell to the symbol which
;;     is the name of the cell. This option controls the heap dumper.
;;
;;   - Listify turns source listing on during assembly. This option controls
;;     the assembler.
;;
;;   - Global-refs turns global variable checking on: if a global is loaded and
;;     has the value #!unspecified, then an exception will be raised. This
;;     option controls the assembler.
;;
;; USAGE:
;;
;;     (show-heap-deps <heap-name> <filename> ...)
;;
;;   Returns the dependence structure as it will be passed to "make" if this 
;;   were a call to "make-heap". No options are allowed.

(define make-heap 'make-heap)
(define show-heap-deps 'heap-deps)

; useful aliases

(define internaltargetdir targetdir)

(define (make-test-heap)
  (set! targetdir internaltargetdir)
  (make-heap (string-append topleveldir "test.heap")
	     'global-refs
	     'global-symbols 
	     (string-append targetdir "testmain.lop")
	     (string-append targetdir "sort.lop")))

(define (make-larceny-heap)
  (set! targetdir internaltargetdir)
  (make-heap (string-append topleveldir "larceny.heap")
	     'global-refs
	     'global-symbols
	     (string-append targetdir "Eval/reploop.lop")
	     (string-append targetdir "Eval/eval.lop")
	     (string-append targetdir "Eval/rewrite.lop")))

(define (make-larceny-eheap)
  (set! targetdir (string-append topleveldir "Thesis/Lib-mg+sc/"))
  (make-heap (string-append topleveldir "larceny.eheap")
	     'global-refs
	     'global-symbols
	     'no-transactions
	     (string-append targetdir "Eval/reploop.lop")
	     (string-append targetdir "Eval/eval.lop")
	     (string-append targetdir "Eval/rewrite.lop")))

(let ()

  ;; Heap dumper

  (define (dump-the-heap file files)
    (let ((fn (string-append file ".map")))
      (delete-file fn)
      (call-with-output-file fn
	(lambda (p)
	  (let ((q (apply dump-heap (cons file files))))
	    (collect)
	    (pretty-print q p))))))


  ;; Magic stuff to expand quasiqotations in a file so as to make it
  ;; possible for the current twobit to compile the resulting file.
  ;; The directory names must be defined in the global environment
  ;; (the default build script sets this up).

  (define (sdir name)
    (string-append sourcedir name))

  (define (tdir name)
    (string-append targetdir name))

  (define (cdir name)
    (string-append configdir name))

  ;;

  (define (preprocess target x)
    (expand313 (car x) target))

  (define (assemble target x)
    (assemble313 (car x) target))

  (define (compile target x)
    (compile313 (car x) target))

  ;; Works only when topleveldir is defined, i.e. when started with build
  ;; script.

  (define (config target x)
    (system (string-append topleveldir "config " (car x))))

  ;; *All* simple file dependencies go here.
  ;; Some shorthands would be lovely.

  (define (makedeps sourcedir targetdir configdir)
    `(((,(targetdir "unixio.lop")
	,(sourcedir "unixio.mal"))
       ,assemble)
      ((,(targetdir "rawapply.lop") 
	,(sourcedir "rawapply.mal"))
       ,assemble)
      ((,(targetdir "integrable-procs.lop") 
	,(targetdir "integrable-procs.lap"))
       ,assemble)
      ((,(targetdir "integrable-procs.lap")
	,(sourcedir "integrable-procs.scm"))
       ,compile)
      ((,(targetdir "xlib.lop")
	,(targetdir "xlib.lap"))
       ,assemble)
      ((,(targetdir "xlib.lap")
	,(sourcedir "xlib.sch"))
       ,compile)
      ((,(targetdir "oblist.lop")
	,(targetdir "oblist.lap"))
       ,assemble)
      ((,(targetdir "oblist.lap")
	,(sourcedir "oblist.sch"))
       ,compile)
      ((,(targetdir "library.lop")
	,(targetdir "library.lap"))
       ,assemble)	  
      ((,(targetdir "library.lap")
	,(sourcedir "library.sch"))
       ,compile)
      ((,(targetdir "print.lop")
	,(targetdir "print.lap"))
       ,assemble)
      ((,(targetdir "print.lap")
	,(sourcedir "print.sch"))
       ,compile)
      ((,(targetdir "schemeio.lop")
	,(targetdir "schemeio.lap"))
       ,assemble)
      ((,(targetdir "schemeio.lap")
	,(sourcedir "schemeio.scm"))
       ,compile)
      ((,(targetdir "numberparser.lop")
	,(targetdir "numberparser.lap"))
       ,assemble)
      ((,(targetdir "numberparser.lap")
	,(sourcedir "numberparser.sch"))
       ,compile)
      ((,(targetdir "reader.lop")
	,(targetdir "reader.lap"))
       ,assemble)
      ((,(targetdir "reader.lap")
	,(sourcedir "reader.sch"))
       ,compile)
      ((,(targetdir "strings.lop")
	,(targetdir "strings.lap"))
       ,assemble)
      ((,(targetdir "strings.lap")
	,(sourcedir "strings.sch"))
       ,compile)
      ((,(targetdir "number2string.lop")
	,(targetdir "number2string.lap"))
       ,assemble)
      ((,(targetdir "number2string.lap")
	,(sourcedir "number2string.sch"))
       ,compile)
      ((,(targetdir "bellerophon.lop")
	,(targetdir "bellerophon.lap"))
       ,assemble)
      ((,(targetdir "bellerophon.lap")
	,(sourcedir "bellerophon.sch"))
       ,compile)
      ((,(targetdir "flonum-stuff.lop")
	,(targetdir "flonum-stuff.lap"))
       ,assemble)
      ((,(targetdir "flonum-stuff.lap")
	,(sourcedir "flonum-stuff.sch"))
       ,compile)
      ((,(targetdir "go.lop")
	,(targetdir "go.lap"))
       ,assemble)
      ((,(targetdir "go.lap")
	,(sourcedir "go.sch"))
       ,compile)
      ((,(targetdir "Sparc/glue.lop")
	,(sourcedir "Sparc/glue.mal"))
       ,assemble)
      ((,(targetdir "ctak.lop")
	,(targetdir "ctak.lap"))
       ,assemble)
      ((,(targetdir "ctak.lap")
	,(sourcedir "ctak.sch"))
       ,compile)
      ((,(targetdir "millicode-support.lop")
	,(targetdir "millicode-support.lap"))
       ,assemble)
      ((,(targetdir "millicode-support.lap")
	,(sourcedir "millicode-support.sch"))
       ,compile)
      ((,(targetdir "millicode-support-dummies.lop")
	,(targetdir "millicode-support-dummies.lap"))
       ,assemble)
      ((,(targetdir "millicode-support-dummies.lap")
	,(sourcedir "millicode-support-dummies.sch"))
       ,compile)
      ((,(targetdir "memstats.lop")
	,(targetdir "memstats.lap"))
       ,assemble)
      ((,(targetdir "memstats.lap")
	,(sourcedir "memstats.sch"))
       ,compile)
      ((,(targetdir "bignums.lop")
	,(targetdir "bignums.lap"))
       ,assemble)
      ((,(targetdir "bignums.lap")
	,(sourcedir "bignums.scm"))
       ,compile)
      ((,(targetdir "ratnums.lop")
	,(targetdir "ratnums.lap"))
       ,assemble)
      ((,(targetdir "ratnums.lap")
	,(sourcedir "ratnums.scm"))
       ,compile)
      ((,(targetdir "rectnums.lop")
	,(targetdir "rectnums.lap"))
       ,assemble)
      ((,(targetdir "rectnums.lap")
	,(sourcedir "rectnums.scm"))
       ,compile)
      ((,(targetdir "contagion.lop")
	,(targetdir "contagion.lap"))
       ,assemble)
      ((,(targetdir "contagion.lap")
	,(sourcedir "contagion.scm"))
       ,compile)
      ((,(targetdir "number.lop")
	,(targetdir "number.lap"))
       ,assemble)
      ((,(targetdir "number.lap")
	,(sourcedir "number.sch"))
       ,compile)
      ((,(targetdir "main.lop")
	,(targetdir "main.lap"))
       ,assemble)
      ((,(targetdir "main.lap")
	,(sourcedir "main.sch"))
       ,compile)
      ((,(targetdir "testmain.lop")
	,(targetdir "testmain.lap"))
       ,assemble)
      ((,(targetdir "testmain.lap")
	,(sourcedir "testmain.sch"))
       ,compile)
      ((,(targetdir "sort.lop")
	,(targetdir "sort.lap"))
       ,assemble)
      ((,(targetdir "sort.lap")
	,(sourcedir "sort.sch"))
       ,compile)
      ((,(targetdir "debug.lop")
	,(targetdir "debug.lap"))
       ,assemble)
      ((,(targetdir "debug.lap")
	,(sourcedir "debug.sch"))
       ,compile)
      ((,(targetdir "preds.lop")
	,(targetdir "preds.lap"))
       ,assemble)
      ((,(targetdir "preds.lap")
	,(sourcedir "preds.sch"))
       ,compile)
      ((,(targetdir "Eval/eval.lop")
	,(targetdir "Eval/eval.lap"))
       ,assemble)
      ((,(targetdir "Eval/eval.lap")
	,(sourcedir "Eval/eval.sch"))
       ,compile)
      ((,(targetdir "Eval/reploop.lop")
	,(targetdir "Eval/reploop.lap"))
       ,assemble)
      ((,(targetdir "Eval/reploop.lap")
	,(sourcedir "Eval/reploop.sch"))
       ,compile)
      ((,(targetdir "Eval/rewrite.lop")
	,(targetdir "Eval/rewrite.lap"))
       ,assemble)
      ((,(targetdir "Eval/rewrite.lap")
	,(sourcedir "Eval/rewrite.sch"))
       ,compile)
      ((,(sourcedir "Eval/rewrite.sch")
	,(sourcedir "Eval/rewrite.raw"))
       ,preprocess)
      ((,(targetdir "exception-handler.lop")
	,(targetdir "exception-handler.lap"))
       ,assemble)
      ((,(targetdir "exception-handler.lap")
	,(sourcedir "exception-handler.sch"))
       ,compile)
      ((,(targetdir "exceptions.lop")
	,(targetdir "exceptions.lap"))
       ,assemble)
      ((,(targetdir "exceptions.lap")
	,(sourcedir "exceptions.sch"))
       ,compile)
      ((,(sourcedir "exceptions.sch")
	,(configdir "exceptions.cfg"))
       ,config)
      ))

  ;; Basic make command for a heap.
  ;; "Other-sources" are additional source files to be loaded.
  ;;
  ;; Be warned that load order *is* important. This can be partly 
  ;; circumvented by good use of 'install-X' procedures, like in 
  ;; the reader and the oblist.

  (define (heap-deps heap-name other-sources sourcedir targetdir configdir)
    `(((,heap-name

	; fundamental 

	,(targetdir "unixio.lop")
	,(targetdir "integrable-procs.lop")
	,(targetdir "rawapply.lop" )
        ,(targetdir "Sparc/glue.lop")

	; random stuff which supposedly works

	,(targetdir "xlib.lop")
	,(targetdir "strings.lop")
	,(targetdir "library.lop")
	,(targetdir "preds.lop")
	,(targetdir "oblist.lop")
	,(targetdir "millicode-support.lop")
	,(targetdir "millicode-support-dummies.lop")
	,(targetdir "memstats.lop")
	,(targetdir "exceptions.lop")
	,(targetdir "exception-handler.lop")

	; basic i/o

	,(targetdir "schemeio.lop")
	,(targetdir "print.lop")

	; basic arithmetic

	,(targetdir "number.lop")

	; stuff which is not at all trusted

	,(targetdir "bignums.lop")
	,(targetdir "ratnums.lop")
	,(targetdir "rectnums.lop")
	,(targetdir "flonum-stuff.lop")
	,(targetdir "number2string.lop")
;;	,(targetdir "bellerophon.lop")
	,(targetdir "numberparser.lop")
	,(targetdir "reader.lop")
	,(targetdir "contagion.lop")
	,(targetdir "debug.lop")

	; other application files

        ,@other-sources

	; driver

	,(targetdir "go.lop"))
       ,(lambda (target x) (dump-the-heap target x)))
      ,@(makedeps sourcedir targetdir configdir)))

  ; doit

  (set! make-heap
	(lambda (heap-name . switches)
	  (set! listify? #f)
	  (set! generate-global-symbols? #f)
	  (set! emit-undef-check? #f)
	  (set! register-transactions-for-side-effects #t)  ; sanity...

	  (let loop ((switches switches) (others '()))
	    (cond ((null? switches) 
		   (make heap-name (heap-deps heap-name
					      (reverse others) 
					      sdir 
					      tdir
					      cdir)))
		  ((eq? (car switches) 'listify)
		   (set! listify? #t)
		   (loop (cdr switches) others))
		  ((eq? (car switches) 'global-symbols)
		   (set! generate-global-symbols? #t)
		   (loop (cdr switches) others))
		  ((eq? (car switches) 'global-refs)
		   (set! emit-undef-check? #t)
		   (loop (cdr switches) others))
		  ((eq? (car switches) 'no-transactions)
		   (set! register-transactions-for-side-effect #f)
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
	  (heap-deps heap-name others sdir tdir cdir)))

  #t)

; eof
