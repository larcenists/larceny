; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; GC testsuite autorun script.
;
; Load this in a Larceny system, then call run-gc-tests with the 
; following parameters:
;   - the name of the larceny executable
;   - the name of the heap image
;   - the name of a logfile
;   - the name of a tracefile
; The tracefile is used to record output from the runs.
;
; Optionally, the following keyword arguments may be used at the end:
;
;   'conservative
;      Selects the conservative system.  Must precede the other keywords.
;
;   'bm name           
;   'bm (name ...)
;      Benchmark name to run (default: all).  The benchmark names are
;      symbols that should match the fourth element in the entry in
;      the 'benchmarks' list below.
;
;   'param name
;   'param (name ...)
;      System name to run (default: all).  The system names are symbols
;      that should match the first element in the entry in in the
;      'precise-parameters' or 'conservative-parameters' lists, below.

; TO DO: benchmarks should produce parseable output, and this program could
; be extended to grovel over the trace file and collect performance info,
; which can then be compared with previous runs to make sure we're not
; losing unexpectedly in new versions.
;
; TO DO: Implement the use of statistics checks.


; Controlling globals.

(define *progpath* ".")			; Load path
(define *debug* #f)			; For debugging runs


; Table of benchmarks.
;
; Each benchmark entry is a name, a list of file names to load, a list
; of expressions to run, a keyword, and a possibly empty list of
; statistics check parameters.  The statistics check parameters are
; used to determine whether the results of the benchmark run are in
; their expected range. [Currently unused.]
;
; DDD   OO     N  N  OO  TTTTT
; D  D O  O    NN N O  O   T     put single or double quotation marks in ANY
; D  D O  O    N NN O  O   T     header strings in this list: the shell or
; DDD   OO     N  N  OO    T     postprocessor will see them, and be confused.

(define bm.text car)
(define bm.files cadr)
(define bm.exprs caddr)
(define bm.ident cadddr)
(define (bm.check x) (car (cddddr x)))

(define benchmarks
  `(("Henglein-Wright Dynamic type inference benchmark"
     ("dynamic")
     ("(dynamic-benchmark 50)"
      "(dynamic-benchmark 50)"
      "(dynamic-benchmark 50)")
     dynamic
     ())
    ("Ellis-Boehm GC benchmark, large object"
     ("gcbench0")
     ("(gc-benchmark 18)"
      "(gc-benchmark 18)"
      "(gc-benchmark 18)")
     gcbench0
     ((allocated 6292000 0.001)		; 6.292MW +/- 0.1%
      (maxheap   7700000 0.05)          ; 7.7MW +/- 5%
      )
     )
    ("Ellis-Boehm GC benchmark, smaller object"
     ("gcbench1")
     ("(gc-benchmark 18)"
      "(gc-benchmark 18)"
      "(gc-benchmark 18)")
     gcbench1
     ())
    ("Array growth benchmark (integers)"
     ("grow")
     ("(int-sequence-benchmark)"
      "(int-sequence-benchmark)")
     grow-int
     ())
    ("Array growth benchmark (lists)"
     ("grow")
     ("(list-sequence-benchmark)"
      "(list-sequence-benchmark)")
     grow-list
     ())
    ("Wright`s lattice benchmark"
     ("lattice")
     ("(lattice-benchmark)"
      "(lattice-benchmark)"
      "(lattice-benchmark)"
      )
     lattice
     ())
    ("Wright`s nbody benchmark"
     ("nbody")
     ("(nbody-benchmark)"
      "(nbody-benchmark)"
      "(nbody-benchmark)"
      )
     nbody
     ())
    ("Boyer-Shaw-Clinger-Baker (nboyer) benchmark"
     ("nboyer")
     ("(nboyer-benchmark 1)"
      "(nboyer-benchmark 2)"
      "(nboyer-benchmark 3)")
     nboyer
     ())
    ("Feeley-Clinger Nucleic2 benchmark"
     ("nucleic2")
     ("(nucleic2-benchmark)"
      "(nucleic2-benchmark)"
      "(nucleic2-benchmark)")
     nucleic2
     ())
    ("Boyer-Shaw-Clinger-Baker (sboyer) benchmark (sharing cons)"
     ("sboyer")
     ("(sboyer-benchmark 0)"
      "(sboyer-benchmark 2)"
      "(sboyer-benchmark 3)")
     sboyer
     ())
    ("Hansen-Clinger 10perm8/mergesort benchmarks"
     ("permsort")
     ("(10perm8-benchmark)"
      "(mergesort-benchmark)"
      "(10perm8-benchmark)"
      "(mergesort-benchmark)"
      "(10perm8-benchmark)"
      "(mergesort-benchmark)")
     permsort
     ())
    ("Dummy benchmark (for testing)"
     ("dummy")
     ("(dummy-benchmark 1)"
      "(dummy-benchmark 2)"
      "(dummy-benchmark 3)"
      )
     dummy
     ())))


; Tables of system/parameter strings.
;
; Each table entry is a list containing a name, a descriptive string, and
; the parameter string.

(define param.ident car)
(define param.text cadr)
(define param.params caddr)

(define conservative-parameters
  '((std
     "Default: only one setting :-)"
      "")))

(define precise-parameters
  '((stopcopy-std
     "Stop-and-copy system with static area"
     "-stopcopy")
    (stopcopy-nostatic
     "Stop-and-copy system without static area"
     "-stopcopy -nostatic")
    (gen-std
     "Default: generational = nursery+ephemeral+dynamic+static"
     "")
    (gen-4gen
     "4 generations, nursery, static"
     "-areas 4 -size1 512K -size2 1M -size3 1536K -size4 3M")
    (np-std
     "Non-predictive standard"
     "-np")
    (np-4gen
     "4 generations, nursery, static, non-predictive"
     "-areas 4 -size1 1M -size2 1M -size3 1M -size4 2M -np")
    ))


(define *bb-files* '())

(define (run-gc-tests program heap logfile tracefile . rest)

  (define conservative? #f)

  (define bm-names (remv 'dummy (map bm.ident benchmarks)))
  (define param-names (map param.ident precise-parameters))

  (define (setup-param-names)
    (set! param-names (if conservative?
                          (map param.ident conservative-parameters)
                          (map param.ident precise-parameters))))

  (define core-number 0)
  (define next-bb-file 0)
  (define failures 0)

  (define (support-load-command)
    (string-append " (load \"" *progpath* "/support.sch\")"))

  (define (file-load-command benchmark)
    (apply string-append
	   (map (lambda (file)
		  (string-append " (load \"" *progpath* "/" file ".fasl\")"))
		(bm.files benchmark))))

  (define (benchmark-run-command benchmark)
    (apply string-append
	   (map (lambda (command)
		  (string-append " " command))
		(bm.exprs benchmark))))

  (define (valid-bm-name? x)
    (memq x (map bm.ident benchmarks)))

  (define (valid-param-name? x)
    (memq x (if conservative?
		(map param.ident conservative-parameters)
		(map param.ident precise-parameters))))

  (define (parse-keywords l)
    (cond ((null? l))
	  ((eq? (car l) 'conservative)
	   (set! conservative? #t)
           (setup-param-names)
	   (parse-keywords (cdr l)))
	  ((null? (cdr l)) 
           (error "Bad rest parameters."))
	  ((eq? (car l) 'bm)
	   (cond ((symbol? (cadr l))
		  (set! bm-names (list (cadr l))))
		 ((list? (cadr l))
		  (set! bm-names (cadr l)))
		 (else 
                  (error "Bad arg to 'bm.")))
	   (if (not (every? valid-bm-name? bm-names))
	       (error "Invalid bm name in list " bm-names))
	   (parse-keywords (cddr l)))
	  ((eq? (car l) 'param)
	   (cond ((symbol? (cadr l))
		  (set! param-names (list (cadr l))))
		 ((list? (cadr l))
		  (set! param-names (cadr l)))
		 (else 
                  (error "Bad arg to 'param.")))
	   (if (not (every? valid-param-name? param-names))
	       (error "Invalid parameter name in list " param-names))
	   (parse-keywords (cddr l)))
	  (else 
           (error "Bad keyword " (car l)))))

  (define (find-benchmarks)
    (mappend (lambda (x)
	       (select (lambda (b) (eq? x (bm.ident b)))
		       benchmarks))
	     bm-names))

  (define (find-parameters)
    (mappend (lambda (x)
	       (select (lambda (p) (eq? x (param.ident p)))
		       (if conservative?
			   conservative-parameters
			   precise-parameters)))
	     param-names))

  (define (process-benchmark benchmark parameter log trace)

    (define (process-failure r)
      (display "FAILED WITH CODE " log)
      (display r log)
      (newline log)
      (display "FAILED!\n") ; Progress meter
      (set! failures (+ failures 1))
      (if (file-exists? "core")
	  (let* ((k  (number->string core-number))
		 (cf (string-append "core-" k)))
	    (set! core-number (+ core-number 1))
	    (rename-file "core" cf)
	    (display "RENAMED CORE FILE TO " log)
	    (display cf log)
	    (newline log)
	    (system (string-append "gzip " cf)))
	  (begin
	    (display "NO CORE FILE???\n" log))))

    (define (construct-cmdline)
      (string-append "echo '(begin "
		     (support-load-command)
		     (file-load-command benchmark)
		     " "
		     (benchmark-run-command benchmark)
		     ")' | "
		     program
		     " "
		     (param.params parameter)
		     " "
		     heap
		     " -args '"
		     (bm.text benchmark)
		     " '"
		     (param.params parameter)
		     " >> " tracefile
		     " 2>&1"))

    (define (process-tcov-file)
      (if (file-exists? "bb.out")
	  (let ((newfn (string-append "bb.out." (number->string next-bb-file))))
	    (rename-file "bb.out" newfn)
	    (set! next-bb-file (+ next-bb-file 1))
	    (set! *bb-files* (append *bb-files* (list newfn))))))

    (let ((cmd (construct-cmdline)))
      (newline log)
      (display "***************************************\n" log)
      (display (bm.text benchmark) log)
      (newline log)
      (display "***************************************\n" log)
      (display cmd log)
      (newline log)
      (flush-output-port log)
      (display (bm.text benchmark)) (newline) ; Progress meter.
      (if (not *debug*)
	  (let ((r (system cmd)))
	    (if (zero? r)
		(display "SUCCEEDED.\n" log)
		(process-failure r))))
      (flush-output-port log)
      (process-tcov-file)))

  (set! *bb-files* '())
  (parse-keywords rest)
  (delete-file logfile)
  (delete-file tracefile)
  (let ((log (open-output-file logfile))
	(trace (open-output-file tracefile)))
    (do ((bm (find-benchmarks) (cdr bm)))
	((null? bm))
      (do ((sys (find-parameters) (cdr sys)))
	  ((null? sys))
	(process-benchmark (car bm) (car sys) log trace)))
    (close-output-port log)
    (close-output-port trace)
    (if (> failures 0)
	(begin (display "\nTOTAL FAILURES: ")
	       (display failures)
	       (newline)))
    (if (not (null? *bb-files*))
	(begin
	  (display 
"\nYou may now process the test coverage traces.  To do this,
load Auxlib/sort.sch and Util/tcov.sch, and evaluate (gc-test-coverage)
to see the summary statistics, or (gc-test-coverage #t) to also annotate
the source files.")))))

; This could do more; e.g., it might 

(define (gc-test-coverage . rest)
  (if (null? *bb-files*)
      (error "No tcov files were created."))
  (tcov0 *bb-files*
	 '("larceny.c" "unix.c" "stats.c" "syscall.c" "callback.c" "argv.c"
           "heapio.c")
	 #t
	 (not (null? rest))))

(define (select p l)
  (cond ((null? l) '())
	((p (car l)) (cons (car l) (select p (cdr l))))
	(else (select p (cdr l)))))

(define (mappend p l)
  (apply append (map p l)))


; The following are currently unused.

(define (process-trace trace)

  (define (read-trace)
    (call-with-input-file trace
      (lambda (in)
	(let loop ((items '()))
	  (let ((item (read in)))
	    (cond ((eof-object? item)
		   (reverse items))
		  ((not (pair? item))
		   (loop items))
		  (else
		   (loop (cons item items)))))))))

  (define (un-nest x)
    (let loop ((a '()) (b '()) (x x))
      (cond ((null? x)
	     (cons a b))
	    ((and (pair? (car x)) (eq? (caar x) 'RUNNING))
	     (loop a (append b (un-nest (car x))) (cdr x)))
	    (else
	     (loop (append a (list (car x))) b (cdr x))))))

  (let ((t (apply append (map un-nest (read-trace)))))
    t))

(define (find-maxheap trace)
  (let ((t (process-trace trace)))
    (sort (map (slot-getter 'maxheap) t) <)))

(define (slot-getter name)
  (lambda (r)
    (let loop ((r r))
      (cond ((null? r) (error "NO SLOT " name))
	    ((pair? (car r))
	     (let ((probe (assq name (car r))))
	       (if probe
		   (cadr probe)
		   (loop (cdr r)))))
	    (else (loop (cdr r)))))))

; eof

