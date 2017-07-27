; Copyright 1998-2004 Lars T Hansen.    -*- mode: scheme; mode: font-lock -*-
;
; $Id$
;
; Petit Larceny:
; Script for building the full heap image "petit-larceny.heap"
;
; 1  Evaluate (BUILD-LARCENY-FILES) in the development environment
; 2  From the command line run
;        "./larceny.bin" -stopcopy -heap petit.heap
; 3  Load this script.  It will create petit-larceny.heap.

(define ($$trace x) #f)                 ; Some code uses this

(define toplevel-macro-expand #f)       ; A hack for the benefit of 
                                        ; init-toplevel-environment

(define (displn x) (display x) (newline))

;;; First load the compiler and seal its namespace.

(let ()
    
  (load "setup.sch")
  (setup)
  (load-compiler 'release)
; (compat:load (param-filename 'common-asm "link-lop.fasl"))

  ;; Need compile-files to find Petit includes

  (require 'petit-compile-file))

(let ()
    
  (displn "Install twobit's macro expander as the interpreter's ditto")

  (macro-expander (lambda (form environment)
		    (let ((switches (compiler-switches 'get)))
		      (dynamic-wind
			  (lambda ()
			    (compiler-switches 'standard))
			  (lambda ()
			    (twobit-expand
                             form
                             (environment-syntax-environment environment)))
			  (lambda ()
			    (compiler-switches 'set! switches))))))

  ; Kids, don't try this at home

  (vector-like-set! (interaction-environment) 
		    4
		    (the-usual-syntactic-environment))

  (displn "Replace and populate the top-level environment")

  (let ((load&disp (lambda (where . names)
                     (let ((filename (apply param-filename where names)))
                       (displn filename)
                       (compat:load filename)))))
    (load&disp 'common-source "toplevel.sch")
    (load&disp 'source "Arch" "Standard-C" "toplevel-target.sch"))

  (displn "done with load")

  (let ((e (interaction-environment)))
    (letrec ((install-procedures 
              (lambda (x procs)
                (if (not (null? procs))
                    (begin
                      (environment-set! x
                                        (car procs)
                                        (environment-get e (car procs)))
                      (install-procedures x (cdr procs)))))))
        
  
      ; Replace the existing environments with fresh environments
      ; containing the bindings for the standard names; those bindings are 
      ; initialized with values taken from the current interaction
      ; environment.

      (init-toplevel-environment)
      
      (install-procedures (interaction-environment)
                          '(; Compilation

			    *scheme-file-types*
                            *fasl-file-type*
			    rewrite-file-type
			    compile-files
                            macro-expand-expression

                            ; On-line help

                            help

                            ; Compiler and assembler switches

                            compiler-switches
                            compiler-flags
                            global-optimization-flags
                            runtime-safety-flags
                            compile-despite-errors
                            issue-warnings
                            include-procedure-names
                            include-source-code
                            include-variable-names
                            single-stepping
                            hygienic-literals
                            avoid-space-leaks
                            runtime-safety-checking
                            catch-undefined-globals
                            integrate-procedures
                            faster-arithmetic
                            control-optimization
                            parallel-assignment-optimization
                            lambda-optimization
                            benchmark-mode
                            benchmark-block-mode
                            global-optimization
                            interprocedural-inlining
                            interprocedural-constant-propagation
                            common-subexpression-elimination
                            representation-inference
                            local-optimization
                            peephole-optimization
                            inline-allocation
			    optimize-c-code

                            ; Temporary to assist this file

                            param-filename
                            compat:load

                            ; Make utility

                            make:project
                            make:new-project
                            make:project?
                            make:rule
                            make:deps
                            make:targets
                            make:pretend
                            make:debug
                            make:make

			    ; Other

			    pretty-print
			    ))))


  (unspecified))

"Install a herald that identifies the generated heap."

(let ()
  (define (file->char-list port)
    (do ((c (read-char port) (read-char port))
         (l '() (cons c l)))
        ((eof-object? c) (reverse l))))
  (define (trim-leading-spaces char-list)
    (let loop ((l char-list))
      (if (char-whitespace? (car l))
          (loop (cdr l))
          l)))
  (define (trim-trailing-spaces char-list)
    (reverse (trim-leading-spaces (reverse char-list))))
  (define (trim-spaces char-list)
    (trim-trailing-spaces
     (trim-leading-spaces char-list)))
  (define date-cmd
    (if (equal? (cdr (assq 'os-name (system-features)))
                "Win32")
        "date /t "
        "date    "))
  (let ((herald-string
         (begin

           ;; A "temporary" file (we know it is about to get overwritten).

           (system (string-append date-cmd "> petit-larceny.heap"))
           (call-with-input-file "petit-larceny.heap"
             (lambda (port)
               (string-append 
                "petit-larceny.heap, built on " 
                (list->string (trim-spaces (file->char-list port)))))))))
    (herald herald-string)))

"Redefine MACRO-EXPAND in the interaction environment."

(define macro-expand
  (lambda (expr . rest)
    (let ((env (if (null? rest)
                   (interaction-environment)
                   (car rest))))
      (macro-expand-expression expr env))))

;;; Redefine COMPILE-FILES to pick up the syntactic environment
;;; from the interaction environment

(define compile-files
  (let ((compile-files compile-files))
    (lambda (inputs output)
      (compile-files inputs output (environment-syntax-environment
				    (interaction-environment))))))

;;; Redefine COMPILE-FILE to use the above redefinition of COMPILE-FILES
;;; (the one that we required from petit-compile-file was sealed off with
;;;  the old compile-files definition)

(define (compile-file infile . rest)
  (let ((outfile
          (if (null? rest)
            (rewrite-file-type infile
                               *scheme-file-types*
                               *fasl-file-type*)
            (car rest))))
    (compile-files (list infile) outfile)))

"Helpful procedure for hiding irrelevant names"

;;; FIXME: this is unsafe because definitions within the loaded files
;;; will override any previous definitions made at Larceny's top level.
;;; That leads to mysterious bugs when adding new standard procedures
;;; or enhanced versions of previously standard procedures.

(define (load-in-private-namespace files procs)
  (let ((e (environment-copy (interaction-environment))))
    (for-each (lambda (f) (load f e)) files)
    (let ((x (interaction-environment)))
      (do ((procs procs (cdr procs)))
          ((null? procs))
        (environment-set! x
                          (car procs)
                          (environment-get e (car procs)))))))

"Load a bunch of useful procedures"

;;; FIXME: the following is commented out, which is just fine.

'
(load-in-private-namespace
 (map (lambda (fasl) (param-filename 'auxiliary fasl))
      `("pp.fasl"              ; defines pretty-print, pretty-line-length
        ;; "misc.fasl"         ; defines nothing that matters here
        "list.fasl"            ; defines aremq!, aremv!, aremove!
                               ;     and redefines list-head (FIXME)
                               ;     and defines some stuff needed by load.fasl
        ;; "string.fasl"       ; defines nothing that matters here
        ;; "vector.fasl"       ; defines nothing at all now
        ;; "pp.fasl"           ; was a duplicate
        ;; "io.fasl"           ; was redefining file-newer?

        ;; These two define some stuff needed by load.fasl.

        ,(if (string=? (cdr (assq 'os-name (system-features)))
                       "Win32")
           "osdep-win32.fasl"
           "osdep-unix.fasl")

        "load.fasl"            ; redefines load (deliberately), defines load-*
        ))
 '(;; remq! remv! remove!      ; defined at toplevel (FIXME: but shouldn't be)
   aremq! aremv! aremove!      ; FIXME: what are these? and why?
   ;; filter find              ; defined at toplevel
   reduce reduce-right         ; FIXME: who uses these?
   ;; fold-left fold-right     ; FIXME: are these R6RS-compatible?
   ;; make-list                ; defined at toplevel for R7RS
   ;; vector-copy              ; defined at toplevel for R7RS
   ;; read-line                ; defined at toplevel for R7RS
   pretty-print pretty-line-length
   load load-noisily load-quietly))

"Load and install the debugger"

;;; FIXME: this stuff is commented out as well

'
(load-in-private-namespace
 (map (lambda (fasl) (param-filename 'debugger fasl))
      '("debug.fasl" "inspect-cont.fasl" "trace.fasl"))
 '(debug trace trace-entry trace-exit untrace break-entry unbreak 
   trace-entry-printer trace-exit-printer
   install-debugger))

'(install-debugger)
'(define install-debugger)

;;; Install pretty printer as default printer.

(repl-printer
 (lambda (x port)
   (if (not (eq? x (unspecified)))
       (pretty-print x port))))

"Load a bunch of useful things."

;;; FIXME: Some of these files could usefully be loaded in private namespaces.

(compat:load (param-filename 'auxiliary "macros.sch"))

; The record system is now defined in Lib/Common/record.sch.

;(load (param-filename 'auxiliary "record.sch"))        ; Record package
;(load "Experimental/define-record.sch") ; DEFINE-RECORD syntax
;(load "Experimental/exception.sch")
;(load "Auxlib/std-ffi.sch")
;(load "Auxlib/unix-functions.sch")
;(load "Experimental/system-stuff.fasl")
;(load "Experimental/applyhook0.fasl")
;(load "Experimental/applyhook.fasl")

"Improve some definitions"

(define (procedure-documentation-string p)
  (let ((e (procedure-expression p)))
    (if (and (list? e)
             (> (length e) 2)
             (string? (caddr e))
             (not (null? (cdddr e))))
        (caddr e)
        #f)))

;;; Give names to all procedures in the interaction environment.

(let* ((env (interaction-environment))
       (vars (environment-variables env))
       (cells (map (lambda (var) (environment-get-cell env var))
                   vars)))
  (for-each (lambda (var cell)
              (let ((val (.cell-ref cell)))
                (if (and (procedure? val)
                         (not (procedure-name val)))
                    (procedure-name-set! val var))))
            vars
            cells))

;;; Set parameters to their defaults.

(compat:load (param-filename 'auxiliary "defaults.sch"))
(set-parameter-defaults-for-a-standard-heap!)
(set! set-parameter-defaults-for-a-standard-heap! (undefined))

"Remove the functions that we only exported to assist on this script"

(set! param-filename (undefined))
(set! compat:load (undefined))

"Dump the heap"

;(gctwa)

(dump-interactive-heap "petit-larceny.heap")

(cond
 ((equal? (cdr (assq 'os-name (system-features)))
	  "Win32")
  (system
   ".\\petit-larceny.bin.exe -reorganize-and-dump -heap petit-larceny.heap")
  (system "move petit-larceny.heap.split petit-larceny.heap"))
 (else
  (system
   "./petit-larceny.bin -reorganize-and-dump -heap petit-larceny.heap")
  (system "mv petit-larceny.heap.split petit-larceny.heap")))

; eof
