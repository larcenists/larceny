; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; tcov-like test coverage tool for gcc-compiled programs.
;
; If a C program is compiled with gcc with the -g -a options, and then run,
; then a file called "bb.out" is created in the working directory that was 
; current on startup.  That file contains information about execution counts 
; for all the basic blocks in the program.  The file cannot be used by the 
; standard SunOS 'tcov' program (and Solaris isn't shipped with tcov), but the
; present program can extract all the necessary information from the file.
;
; Equipped with this information, various summary statistics can be produced,
; and source files annotated with execution counts can be produced.  It works
; quite well in practice.
;
;
; Evaluate the following expression:
;
;   (tcov bbfile ...)
;
; where bbfile is the name of the basic-block output file from the program
; (normally "bb.out") to produce, for each source file filename.c mentioned
; in the bb.out file, a file in that file's directory called filename.tcov that
; is a copy of the source file annotated with execution counts for each basic 
; block.  Several bbfiles can be named; their contents are merged and an 
; overall analysis is presented.
;
; It is also possible to execute parts of the analysis, or to read the bbfile
; once and then use it for further analysis.  Examine the functions tcov and
; summary-report to get some ideas.
;
;
; Note on C compiler usage.
;
; Compiling the C program with optimization will cause tcov to produce less
; meaningful results, since optimization rearranges the program, merges, 
; deletes, and inserts basic blocks, and the coverage is recorded in terms
; of basic blocks.
;
; Also, it may be useful to compile with -DNDEBUG (this gets rid of uses of
; the assert() macro) since the failure branch in an assertion is typically
; never taken.  Leaving assertions in thus pollutes data about functions 
; called but with nonexecuted blocks.
;
;
; Note on portability.
;
; When running with Chez Scheme, you must uncomment the two compatibility 
; procedures at the end of the file.
;
; When running with Larceny, you must load a sorting package first.  The sort
; used by this program takes a list and a less-than predicate.

(define (tcov . bbfiles)
  (tcov0 bbfiles '() #t #t))

(define (tcov0 bb-files ignore-files summary? annotate?)
  (let* ((info (merge-info-structures
		(map collect-information bb-files)))
	 (summary-info
	  (summary-statistics info)))
    (set! *tcov-computed-info* info)
    (set! *tcov-computed-summary* summary-info)
    (if summary? (summary-report summary-info ignore-files))
    (if annotate? (annotate-source-files info))))

(define *tcov-computed-info* #f)
(define *tcov-computed-summary* #f)

;;; Summary report

; The optional second argument is a list of filenames to ignore.
; This can be expanded to both filenames and function-names.

(define (summary-report summary-info . rest)

  (define uninteresting-files (if (null? rest) '() (car rest)))

  (define (process-summary summary-info)
    (for-each 
     (lambda (file-info)
       (let* ((fn      (car file-info))
	      (summary (cdr file-info))
	      (func-info (summary.func-info summary)))
	 (if (not (null? func-info))
	     (begin (print "  " fn)
		    (for-each (lambda (func)
				(print "    " (summary-func.name func)))
			      func-info)))))
     (filter-files summary-info uninteresting-files)))

  (newline)
  (print "Functions never called")
  (process-summary
   (select-functions-never-called summary-info))

  (newline)
  (print "Functions called that have non-executed blocks")
  (process-summary
   (select-functions-with-nonexecuted-blocks summary-info)))


; Useful

(define (filter-files summary-info uninteresting-files)

  (define (interesting? fn)
    (let loop ((u uninteresting-files))
      (cond ((null? u) #t)
	    ((suffix? fn (car u)) #f)
	    (else (loop (cdr u))))))

  (select (lambda (file-info)
	    (let ((fn (car file-info)))
	      (interesting? fn)))
	  summary-info))

(define (select-functions-never-called summary-info)
  (select-functions 
   (lambda (f)
     (= (summary-func.blocks f) (summary-func.zero f)))
   summary-info))

(define (select-functions-with-nonexecuted-blocks summary-info)
  (select-functions
   (lambda (f)
     (< 0 (summary-func.zero f) (summary-func.blocks f)))
   summary-info))


;;; Annotate the source files.

; For each source file filename.c, produce filename.tcov which has the same
; contents as filename.c except that each line is indented by one tab and
; at the beginning of a line that starts a basic block, the highest execution 
; count for that line (there may be several, because there may be several
; blocks per line) is shown.  If the execution count is 0, then "#####" is
; printed in place of a count.
;
; If there are multiple blocks on a line, and some but not all have been
; executed, then the line is annotated with nnn/0 where n is the maximal
; count.

(define (annotate-source-files all-files-info)
  (for-each (lambda (fn)
	      (annotate-file fn all-files-info))
	    (map car all-files-info)))

(define (annotate-file file-name all-files-info)

  (define (show x z l out)
    (cond ((equal? x 0)
	   (display "#####" out))
	  ((number? x)
	   (display x out)
	   (if z (display "/0" out)))
	  (else
	   (display x out)))
    (display #\tab out)
    (display l out)
    (newline out))

  (define (annotate-it info)
    (let ((infn  (car info))
	  (outfn (hack-extension (car info) ".tcov"))
	  (data  (sort (cdr info)
		       (lambda (a b)
			 (< (info.line a) (info.line b))))))
      (call-with-input-file infn
	(lambda (in)
	  (print "Annotating " outfn)
	  (call-with-output-file outfn
	    (lambda (out)
	      (let loop ((lineno 1) (data data))
		(let ((l (read-line in)))
		  (cond ((eof-object? l))
			((and (not (null? data))
			      (= lineno (info.line (car data))))
			 (let loop2 ((data (cdr data))
				     (z    #f)
				     (m    (info.count (car data))))
			   (if (and (not (null? data))
				    (= lineno (info.line (car data))))
			       (loop2 (cdr data)
				      (or z (zero? (info.count (car data))))
				      (max m (info.count (car data))))
			       (begin (show m z l out)
				      (loop (+ lineno 1) data)))))
			(else
			 (show "" #f l out)
			 (loop (+ lineno 1) data)))))))))))

  (annotate-it
   (find (lambda (x) (suffix? (car x) file-name)) all-files-info)))


;;; Compute summary statistics.

; Returns an assoc list that maps source file names to statistics.
; The statistics is a list with the following elements:
;    a list of lists of the form (function count blocks zero)
;      where function is a function name
;          count is the highest execution count for any basic block in the fn
;          blocks is the number of basic blocks in the function
;          zero is the number of blocks in the function with zero execution 
;            count
;    ... nothing else yet ...

(define (summary-statistics file-info)

  ; Input is the info list from a single file.  This is sorted by block 
  ; number, but since blocks are lexically ordered, all blocks in a file
  ; are contiguous.

  (define (collect-function-info info)
    (let ((fns   '())
	  (fname "")
	  (mcount 0)
	  (blocks 0)
	  (zero   0))

      (define (complete-function new-fname)
	(if (not (string=? fname ""))
	    (begin
	      (set! fns (cons (list fname mcount blocks zero) fns))
	      (set! mcount 0)
	      (set! blocks 0)
	      (set! zero 0)))
	(set! fname new-fname))

      (do ((info info (cdr info)))
	  ((null? info)
	   (complete-function "")
	   fns)
	(if (not (string=? fname (info.function (car info))))
	    (complete-function (info.function (car info))))
	(set! mcount (max mcount (info.count (car info))))
	(set! blocks (+ blocks 1))
	(if (zero? (info.count (car info)))
	    (set! zero (+ zero 1))))))

  (print "Computing summary statistics.")
  (map
   (lambda (fn-info)
     (let ((fn (car fn-info))
	   (info (cdr fn-info)))
       (cons fn (list (collect-function-info info)))))
   file-info))

; Overall-summary is a list of pairs: (filename . summary)

(define (select-functions pred? overall-summary)
  (map (lambda (file-summary)
	 (let ((filename (car file-summary))
	       (summary  (cdr file-summary)))
	   (cons filename
		 (list (select pred? (summary.func-info summary))))))
       overall-summary))


(define summary.func-info car)
(define summary-func.name car)
(define summary-func.max cadr)
(define summary-func.blocks caddr)
(define summary-func.zero cadddr)


;;; Information extraction

; Process the bb.out file -- extract information about the basic blocks.
; The file has the following form:
;
; Header: the string beginning with "Basic block profiling finished"
; A file leader: the string beginning with "File "
; Block info per file: the string beginning with spaces and then "Block #".
; This line has the following format:
;      Block #<blockno>: executed <count> time(s) address= <address> \
;      function= <name> line= <lineno> file= <filename>
; There may be additional blanks between the text items.  <address> is a hex 
; address; <name> contains no spaces; <filename> starts on the second 
; character after file= and goes through the end of the line.
;
; Blocks are listed in increasing block number order.
; Blank lines should be ignored.
;
; This procedure returns an assoc list that maps filename to a list of block 
; info nodes.  Blocks are in block # order.  Files are in alphabetical order.
;
; This would be a lot less painful if the bb.out file was already in sexpr
; format! :-)

(define (collect-information bbfile)

  (define info '())
  (define fn   #f)
  (define blockinfo '())

  (define (complete-block)
    (if fn
	(set! info (cons (cons fn (reverse blockinfo)) info)))
    (set! fn #f)
    (set! blockinfo '()))

  (print "Processing " bbfile)
  (call-with-input-file bbfile
    (lambda (bbin)
      (do ((l (read-nonblank-line bbin) (read-nonblank-line bbin)))
	  ((eof-object? l)
	   (complete-block)
	   (sort info (lambda (a b)
			(string<? (car a) (car b)))))
	(cond ((prefix? l "Basic block profiling finished"))
	      ((prefix? l "File")
	       (complete-block)
	       (set! fn
		     (hack-extension (substring l 5 (find-char l #\, 5)) ".c")))
	      ((prefix? l "Block #")
	       (set! blockinfo (cons (extract-block-info l) blockinfo)))
	      (else
	       (fail "Don't understand line in bbinfo: " l)))))))

(define (extract-block-info l)

  (define (field left right)
    (let ((start (+ (string-length left) (find-substring l left)))
	  (end   (find-substring l right)))
      (substring l start end)))

  (let* ((block
	  (string->number (prestrip (substring l 7 (find-char l #\: 0)))))
	 (count
	  (string->number (endstrip (field "executed" "time(s)"))))
	 (addr 
	  (let ((a (endstrip (field "address=" "function="))))
	    (string-set! a 0 #\#)
	    (string->number a)))
	 (function
	  (endstrip (field "function=" "line=")))
	 (line
	  (string->number (endstrip (field "line=" "file="))))
	 (file
	  (let* ((start (+ 6 (find-substring l "file=")))
		 (end   (string-length l)))
	    (substring l start end))))
    (make-info block count addr function line file)))


;;; Info nodes

(define (make-info blockno count addr function lineno file)
  (list blockno count addr function lineno file))

(define info.block car)
(define info.count cadr)
(define info.addr caddr)
(define info.function cadddr)
(define (info.line x) (car (cddddr x)))
(define (info.file x) (cadr (cddddr x)))


;;; Merge info structures

(define (merge-info-structures structures)

  (define (merge-blocks a b)
    (map (lambda (a b)
	   (if (not (= (info.block a) (info.block b)))
	       (fail "Consistency check#2 in merge: " a " " b))
	   (make-info (info.block a)
		      (+ (info.count a) (info.count b))
		      (info.addr a)
		      (info.function a)
		      (info.line a)
		      (info.file a)))
	 a b))

  (define (merge a b)
    (cond ((null? a) b)
	  ((null? b) a)
	  ((string=? (caar a) (caar b))
	   (cons (cons (caar a) (merge-blocks (cdar a) (cdar b)))
		 (merge (cdr a) (cdr b))))
	  ((string<? (caar a) (caar b))
	   (cons (car a) (merge (cdr a) b)))
	  (else
	   (cons (car b) (merge a (cdr b))))))

  (cond ((null? structures)
	 (fail "No structures to merge."))
	((null? (cdr structures))
	 (car structures))
	(else
	 (merge-info-structures
	  (cons (merge (car structures) (cadr structures))
		(cddr structures))))))

;;; String stuff

(define (hack-extension fn ext)		;Replace exension by .ext (not robust)
  (let ((x (find-char-from-end fn #\.)))
    (if (not x)
	(string-append fn ext)
	(string-append (substring fn 0 x) ext))))

(define (find-char x c i)
  (cond ((= i (string-length x)) #f)
	((char=? c (string-ref x i)) i)
	(else (find-char x c (+ i 1)))))

(define (find-char-from-end fn c)
  (let loop ((i (- (string-length fn) 1)))
    (cond ((< i 0) #f)
	  ((char=? (string-ref fn i) c) i)
	  (else (loop (- i 1))))))

(define (find-substring x pat)
  (let ((l  (string-length x))
	(c  (string-ref pat 0)))
    (let loop ((i 0))
      (cond ((= i l) #f)
	    ((and (char=? c (string-ref x i)) (match-at? x pat i)) i)
	    (else (loop (+ i 1)))))))

(define (prefix? x pat)
  (match-at? x pat 0))

(define (suffix? x pat)
  (let ((i (- (string-length x) (string-length pat))))
    (and (>= i 0) (match-at? x pat i))))

(define (match-at? x pat i)		;Is pat a substring of x at i?
  (let ((pl (string-length pat))
	(l  (string-length x)))
    (and (<= (+ i pl) l)
	 (let loop ((i i) (j 0))
	   (cond ((= j pl) #t)
		 ((not (char=? (string-ref pat j) (string-ref x i))) #f)
		 (else (loop (+ i 1) (+ j 1))))))))

(define (prestrip s)			;Strip spaces at beginning
  (let ((l (string-length s)))
    (let loop ((i 0))
      (cond ((= i l) "")
	    ((char-whitespace? (string-ref s i))
	     (loop (+ i 1)))
	    (else
	     (substring s i l))))))

(define (poststrip s)			;Strip spaces at end
  (let ((l (string-length s)))
    (let loop ((i (- l 1)))
      (cond ((< i 0) "")
	    ((char-whitespace? (string-ref s i))
	     (loop (- i 1)))
	    (else
	     (substring s 0 (+ i 1)))))))

(define (endstrip s)			;Strip spaces at both ends
  (prestrip (poststrip s)))


;;; I/O stuff

(define (print . exprs)
  (for-each display exprs)
  (newline))

(define (read-line inp)
  (let loop ((l '()) (c (read-char inp)))
    (cond ((eof-object? c)
	   (if (null? l) c (list->string (reverse l))))
	  ((char=? c #\newline)
	   (list->string (reverse l)))
	  (else
	   (loop (cons c l) (read-char inp))))))

(define (read-nonblank-line inp)
  (let loop ((l (read-line inp)))
    (if (eof-object? l)
	l
	(let ((l (prestrip l)))
	  (if (string=? l "")
	      (loop (read-line inp))
	      l)))))

;;; Misc

(define (select pred? l)
  (cond ((null? l) '())
	((pred? (car l)) (cons (car l) (select pred? (cdr l))))
	(else (select pred? (cdr l)))))

(define (find pred? l)
  (cond ((null? l) #f)
	((pred? (car l)) (car l))
	(else (find pred? (cdr l)))))

; Works in both Chez and Larceny

(define (fail . x)
  (display "ERROR: ")
  (for-each display x)
  (newline)
  (reset))


;;; Chez Scheme compatibility

;(define sort
;  (let ((sort sort))
;    (lambda (list less?)
;      (sort less? list))))

;(define call-with-output-file
;  (let ((cwof call-with-output-file))
;    (lambda (fn proc)
;      (delete-file fn)
;      (cwof fn proc))))

; eof
