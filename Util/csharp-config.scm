;; Copy-and-Hack from LARCENY/Util/config.sch

; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Multi-lingual autoconfiguration program, version 2.
; Viciously hacked to accomodate v0.20, needs cleaning up. It's a mess.
; ASM parts are SPARC dependent (sigh).
;
; USAGE
;   (config <configfile> ...)
;
; DESCRIPTION
;   The situation is this: we have a number of defined constants which are
;   used in various guises and with slightly differing names in several
;   programs in different languages. For example, there is the constant
;   "M_BREAK", which is the index in the millicode table of the entry point
;   for the breakpoint handler. In C, this identifier denotes the value
;   "x", while in assembly language it denotes the value "x*4" and in
;   scheme it denotes "x*4" also but here it has the name "$m.break".
;
;   The problem is to maintain "header" files in all languages and ensure that
;   they are in sync. The only reasonable way of doing this is to generate
;   the language specific headers automatically from one language independent
;   base file.
;
;   While we could have used M4, Scheme is more fun. Hence this program.
;
;   There is one config file for each group of related header files.
;   The first expression in the file should define the output files:
;
;     (define-files <c header> <assy header> <scheme header>)
;
;   A file name can be #f, meaning that language will not be generated from
;   this config file at all.
;
;   Then the constants follow. The basic syntax of each constant is this:
; 
;     (define-const <name> <base value> <c name> <assy name> <scheme name>)
;
;   The <x name> in the define-const can be #f, avoiding generating it 
;   altogether for the appropriate language. If the entry is not #f but the
;   file entry was #f, the entry is ignored.
;
;   This defines the base value and the names for the header files for all
;   languages. The base value can be any constant or it can be a previously
;   defined <name>, or a general Scheme expression involving previously 
;   defined <name>s and Scheme procedures. If the base value is not simply
;   a constant, known variables will be substituted for and the resulting
;   expression will be passed to "eval".
;
;   After evaluation, a the base value is passed through an action procedure
;   for each language; this procedure may modify the value and return the
;   modified value. The default action is the identity procedure.
;
;   One can override the default action on a language basis:
;
;     (define-action <language> <expr>)
;
;   where <language> is one of the symbols "c", "assembly", or "scheme", and
;   where <expr> must evaluate to a procedure of one argument. It will be
;   invoked with the base value of the define-const if the entry for the
;   language is non-false. <Expr> is evaluated like the base: known variables
;   are substituted for before evaluation.
;
; AUTHOR
;   Lars Thomas Hansen
;
; BUGS
;   Some more shorthands would be nice.
;
;   Little, if any, error checking. Watch where you step.
;
;   The evaluator knows nothing of quasiquotations; don't use them. Also, don't
;   use a defined name as a formal in a lambda expression, and don't define
;   a name using a Scheme reserved word.

; "Config" takes configuration files as arguments and runs through each one
; in turn.

;; csharp-config : string (listof (list string 'int|'uint)) -> void
(define (csharp-config outfile inputs)
  (define infiles (map car inputs))
  (define types (map cadr inputs))
  (define make-info vector)
  (define (info.symtab x) (vector-ref x 0))
  (define (info.symtab! x v) (vector-set! x 0 v))
  (define (info.csharp x) (vector-ref x 1))
  (define (info.csharp! x v) (vector-set! x 1 v))
  
  (define (info.lookup x n)
    (assq n (info.symtab x)))
  
  (define make-lang vector)
  (define (lang.fn x)  (vector-ref x 0))
  (define (lang.port x) (vector-ref x 1))
  (define (lang.fmt x) (vector-ref x 2))
  (define (lang.action x) (vector-ref x 3))
  
  (define (caddddr x) (car (cddddr x)))
  (define (cadddddr x) (car (cdr (cddddr x))))
  
  ; predicates for commands
  
  (define (define-files? x)
    (and (pair? x) (eq? (car x) 'define-files)))

  (define (define-const? x)
    (and (pair? x) (eq? (car x) 'define-const)))

  (define (define-action? x)
    (and (pair? x) (eq? (car x) 'define-action)))

  (define (define-table? x)
    (and (pair? x) (eq? (car x) 'define-table)))

  (define (start-roots? x)
    (and (pair? x) (eq? (car x) 'start-roots)))

  (define (end-roots? x)
    (and (pair? x) (eq? (car x) 'end-roots)))

  (define (define-global? x)
    (and (pair? x) (eq? (car x) 'define-global)))

  (define (define-mproc? x)
    (and (pair? x) (eq? (car x) 'define-mproc)))

  (define (align? x)
    (and (pair? x) (eq? (car x) 'align)))

  (define (config-loop inp info)
    (fold/sexprs/file 
     handle-conf-item
     info
     inp))

  (define (fold/sexprs/file op info port)
    (let loop [(info info)]
      (let [(next (read port))]
        (if (eof-object? next)
            info
            (loop (op next info))))))
  
  (define (handle-conf-item item info)
    (cond ((eof-object? item)
           info)
          ((define-const? item)
           (define-const item info))
          ((define-action? item)
           info)
          ((define-table? item)
           '(define-table item)
           info)
          ((start-roots? item)
           '(define-const 
             `(define-const first-root ,table-counter "FIRST_ROOT" #f #f) 
             accum)
           info)
          ((end-roots? item)
           '(define-const 
             `(define-const last-root ,(- table-counter 1) "LAST_ROOT" #f #f)
             accum)
           info)
          ((define-global? item)
           info)
          ((define-mproc? item)
           info)
          ((align? item)
           info)
          (else
           (twobit-format #t "Unknown command ~a~%" item)
           info)))

  ; Evaluate expression, watching out for constants.

  (define (eval-expr expr symtab)

    (define (subs expr)
      (cond ((number? expr) expr)
	    ((symbol? expr)
	     (let ((probe (assq expr symtab)))
	       (if probe
		   (cdr probe)
		   expr)))
	    ((string? expr) expr)
	    ((boolean? expr) expr)
	    ((null? expr) expr)
	    ((pair? expr)
	     (if (eq? 'quote (car expr))
		 expr
		 (map subs expr)))
	    (else
	     (error "Invalid expression ~a" expr))))

    (if (number? expr) 
	expr
	(eval (subs expr))))

  ; Define a constant; return a new info structure.

  (define (define-const x info)
    (define (dump-const! entry lang base)
      (if lang
	  (cond ((string? entry)
                 (twobit-format (lang.port lang)
                                (lang.fmt lang)
                                entry
                                ((lang.action lang) base)))
                ((not entry))
                (else
                 (error "Invalid entry for DEFINE-CONST: " x)))))

    (let ((name (cadr x))
	  (base (eval-expr (caddr x) (info.symtab info)))
          (csharp (cadddr x))) ;; same as c was
      (let ((probe (info.lookup info name)))
	(if probe
	    (begin 
	      (error "Redefinition of ~a ignored." name)
	      info)
	    (begin
	      (dump-const! csharp (info.csharp info) base)
	      (make-info (cons (cons name base) (info.symtab info))
			 (info.csharp info)))))))


  (define (close-output-files info)
    (if (info.csharp info) (close-output-port (lang.port (info.csharp info)))))
  
  (define (prologue out)
    (display "namespace Scheme.RT {" out) 
    (newline out)
    (display "  public class Constants {" out)
    (newline out))
  (define (subprologue file outport)
    (twobit-format outport "~%  /**** ~a ****/~%~%" file))
  (define entry-format "    public static readonly uint ~a = ~a;~%")
  (define entry-format/int "    public static readonly int ~a = ~a;~%")
  (define (epilogue out)
    (display "  }" out)
    (newline out)
    (display "}" out)
    (newline out))

  (call-with-output-file outfile
    (lambda (outport)
        (prologue outport)
        (for-each (lambda (fn type)
                    (let [(entry-format
                           (case type
                             ((int) entry-format/int)
                             ((uint) entry-format)))]
                      (let [(config-info
                             (make-info '() (make-lang #f outport entry-format values)))]
                        (let* ((inp   (open-input-file fn))
                               (files (read inp)))
                          (if (not (define-files? files))
                              (error "Expected 'define-files' in ~a" fn))
                          (subprologue fn outport)
                          (config-loop inp config-info)
                          (close-input-port inp)))))
                  infiles
                  types)
      (epilogue outport))))
