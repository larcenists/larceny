; Util/bootcomp.sch
; $Id: bootcomp.sch,v 1.1 1997/02/11 21:53:13 lth Exp lth $
;
; Program that bootstraps the heap containing the compiler.
; Load from Larceny root directory.

(define compilation-system-loaded #f)
(define compat-file-loaded #f)
(define basic-stuff-loaded #f)

(define assembler-dir "Newasm")
(define compiler-dir "Compiler")

(define (loadit fn)
  (display "; Loading ")
  (display fn)
  (newline)
  (load fn))

(define (fixpath s)
  (cond ((string=? s "") s)
	((char=? (string-ref s (- (string-length s) 1)) #\/) s)
	(else (string-append s "/"))))

(define (assert-exists fn)
  (if (not (file-exists? fn))
      (error "Can't find file \"" fn "\"")))

; Load some useful procedures.

(define (load-basic-stuff root)
  (let ((root (fixpath root)))
    (define auxload-fn (string-append root "Auxlib/auxload.sch"))

    (if (not basic-stuff-loaded)
	(begin (assert-exists auxload-fn)
	       (loadit auxload-fn)
	       (auxload (string-append root "Auxlib/"))
	       (set! basic-stuff-loaded #t)))))

(define (load-compat-file root)
  (let ((root (fixpath root)))
    (define compat-fn (string-append root "Larceny/compat.fasl"))

    (if (not compat-file-loaded)
	(begin (assert-exists compat-fn)
	       (loadit compat-fn)
	       (set! compat-file-loaded #t)))))

(define (load-compiler root)
  (let ((root (fixpath root)))
    (define compload-fn (string-append root compiler-dir "/compload.sch"))

    (load-compat-file root)
    (assert-exists compload-fn)
    (loadit compload-fn)
    (compload 'compiler (string-append root compiler-dir "/"))
    #t))

(define (load-assembler root)
  (let ((root (fixpath root)))
    (define compload-fn (string-append root compiler-dir "/compload.sch"))
    (define asmload-fn (string-append root assembler-dir "/asmload.sch"))
    (define schdefs-fn (string-append root "Build/schdefs.h"))

    (assert-exists compload-fn)
    (assert-exists asmload-fn)
    (assert-exists schdefs-fn)

    (loadit compload-fn)
    (compload 'assembler (string-append root compiler-dir "/"))
    (loadit asmload-fn)
    (loadit schdefs-fn)
    (asmload (string-append root assembler-dir "/"))
    #t))

; Load everything needed to run the compiler and assembler, but not the
; stuff needed to build complete bootstrap heap images (make file, dumper,
; makefasl, etc).

(define (load-compilation-system root)
  (let ((root (fixpath root)))
    (if (not compilation-system-loaded)
	(begin (load-basic-stuff root)
	       (load-compat-file root)
	       (load-compiler root)
	       (load-assembler root)
	       (loadit (string-append root compiler-dir "/makefasl.fasl"))
	       (set! compilation-system-loaded #t)))
    (default-compilation-options)))

(define (dump-compiler-heap root)
  (let ((root (fixpath root)))
    (load-compilation-system root)
    (dump-interactive-heap "compiler.heap")))

; Note that this procedure will be present in the dumped heap and can
; be called, and since compilation-system-loaded is preserved as #t it
; will do the right thing... Pretty neat!
;
; FIXME: should load help system, compilation switch printer.

(define (load-development-system . rest)
  (let ((root (if (null? rest) "" (fixpath (car rest)))))
    (load-basic-stuff root)
    (load-compilation-system root)
    (loadit (string-append root compiler-dir "/make.fasl"))
    (loadit (string-append root "Lib/makefile.fasl"))
    (loadit (string-append root compiler-dir "/dumpheap.fasl"))
    (loadit (string-append root compiler-dir "/pass5p1.fasl"))
    (loadit (string-append root compiler-dir "/pass5p2sparc.fasl"))
    (set! compat:write-bytevector compat:write-bytevector-as-bytevector)
    (default-compilation-options)))

(define (default-compilation-options)
  (issue-warnings #f)
  (include-source-code #f)
  (include-variable-names #f)
  #t)

; eof
