; -*- Scheme -*-
; $Id: make.sch,v 1.2 1997/02/11 20:21:58 lth Exp $
;
; Larceny compilation system -- `make' facility.
;
; Lars Thomas Hansen / August 25, 1995
;
; A makefile is set up by creating a project and then adding 
; dependencies, rules, and targets to that project, and then running
; 'make:make' to create a target.
;
; PORTABILITY
;   Depends on non-standard procedures:
;     (file-exists? filename)             =>  boolean
;     (file-modification-time filename)   =>  integer or <time vector>
;     (reset)                             =>  unspecified
;     (delete-file filename)              =>  unspecified
;
;   If A and B are files and file-modification-time returns an
;   integer, then (file-modification-time A) < (file-modification-time B)
;   iff B was modified after A. If a time vector is returned, then each
;   element must be a number and if the numbers are compared pairwise 
;   from low to high indices, then the first non-equal pair determines
;   the relationship as before.
;
;   - Larceny has all four procedures; f-m-t returns a time vector.
;   - Chez 4.1 has f-e?, reset, and d-f; f-m-t is a simple foreign-function. 
;   - MIT Scheme 7.3.0 has f-e?, f-m-t (returns an integer) and d-f; reset is 
;     called abort.
;
; BUGS
;   - Needs to disambiguate direct rules based on modification time; see
;     FIXME in code.
;   - Could use hash tables many places for faster lookup in large projects.
;   - Make:pretend is global (not project-specific); should this change?
;   - Must remove primary dependency from occurring subsequently in the
;     dependency list.
;   - When doing a pretend run, for each file ostensibly created by
;     the make run we should cache its name and time to use later in the
;     simulation.
;   - In general, existence status and modification times could be cached.
;   - There could be a notion of rule precedence to allow A->C to be chosen
;     over A->B and B->C.

; Select your host system

(define (make:new-project name) 
  (vector 'project name '() '() '()))

(define (make:project? obj)
  (and (vector? obj)
       (> (vector-length obj) 0)
       (eq? (vector-ref obj 0) 'project)))

(define (make-project.name proj) (vector-ref proj 1))
(define (make-project.rules proj) (vector-ref proj 2))
(define (make-project.rules! proj rules) (vector-set! proj 2 rules))
(define (make-project.deps proj) (vector-ref proj 3))
(define (make-project.deps! proj deps) (vector-set! proj 3 deps))
(define (make-project.targets proj) (vector-ref proj 4))
(define (make-project.targets! proj targets) (vector-set! proj 4 targets))

; Files with extension 'ext1' can be made from a similarly-named file with
; extension 'ext2' by running 'command', which is a procedure which takes
; the target file and a list of dependencies as arguments, where the
; source file is always the first element of the list of dependencies
; (and is not duplicated).
;
; The extensions include a leading period.

(define (make:rule proj ext1 ext2 command)
  (let* ((rules (make-project.rules proj))
	 (probe (assoc (cons ext1 ext2) rules)))
    (if probe
	(begin (display "Overriding rule for ")
	       (display ext1)
	       (display " -> ")
	       (display ext2)
	       (newline)
	       (set-cdr! probe command))
	(make-project.rules! proj (cons (cons (cons ext1 ext2) command)
					rules)))))

; Files named in 'files1' depend on files in 'files2'.
;
; A dependency specifies a relationship between files: some files depend on
; other files. No commands are specified for creating the dependent file;
; such commands are specified in a rule or in a target command.

(define (make:deps proj files1 files2)

  (define (merge orig new)
    (let loop ((new new) (l '()))
      (cond ((null? new)
	     (append orig (reverse l)))
	    ((member (car new) orig)
	     (loop (cdr new) l))
	    (else
	     (loop (cdr new) (cons (car new) l))))))

  (for-each (lambda (target)
	      (let* ((deps  (make-project.deps proj))
		     (probe (assoc target deps))
		     (item  (if probe
				probe
				(let ((item (cons target '())))
				  (make-project.deps! proj (cons item deps))
				  item))))
		(set-cdr! item (merge (cdr item) files2))))
	    files1))

; Files named in 'targets' can be created by running 'command', which is
; a procedure which takes a target name and its dependencies as arguments.

(define (make:targets proj targets command)
  (for-each (lambda (target)
	      (let* ((t     (make-project.targets proj))
		     (probe (assoc target t)))
		(if probe
		    (begin (display "Overriding command for ")
			   (display target)
			   (newline)
			   (set-cdr! probe command))
		    (make-project.targets! proj
					   (cons (cons target command) t)))))
	    targets))

; Manage a flag which makes the MAKE execute the commands or just pretend
; to. For debugging makefiles.

(define make:pretend 
  (let ((state #f))
    (lambda args
      (cond ((null? args) state)
	    ((and (null? (cdr args))
		  (boolean? (car args)))
	     (set! state (car args))
	     state)
	    (else
	     (display "Make:pretend: strange: " args)
	     state)))))

; Create 'target', unless all its dependencies exist and are older,
; recursively.

(define (make:make proj target)

  (define have-made '())
  (define soft-rules '())

  (define (errhandler target)
    (lambda ()
      (newline)
      (display "*** Error encountered!") (newline)
      (display "Deleting target file: ")
      (display target)
      (newline)
      (delete-file target)))

  (define (extension fn)
    (let ((len (string-length fn)))
      (let loop ((i (- len 1)))
	(cond ((< i 0) "")
	      ((char=? (string-ref fn i) #\.)
	       (substring fn i len))
	      (else (loop (- i 1)))))))

  (define (replace-extension fn old new)
    (string-append (substring fn 0 (- (string-length fn) (string-length old)))
		   new))

  (define (newer-than? a b)
    (let ((ta (file-modification-time a))
	  (tb (file-modification-time b)))
      (timestamp-newer? ta tb)))

  (define (timestamp-newer? ta tb)
    (cond ((and (number? ta) (number? tb))
	   (> ta tb))
	  ((and (vector? ta) (vector? tb))
	   (let ((limit (vector-length ta)))
	     (let loop ((i 0))
	       (cond ((= i limit) #f)
		     ((= (vector-ref ta i) (vector-ref tb i)) (loop (+ i 1)))
		     (else (> (vector-ref ta i) (vector-ref tb i)))))))
	  (else
	   (make:error 'newer-than? "Internal error: " ta tb))))

  ; l is either a list of file names or a list of pairs whose car is
  ; a file name and cdr is arbitrary. Return the item whose file is 
  ; newer than the others.

  (define (newest l)
    (cdr (select-least (map (lambda (x)
			      (cons (if (string? x)
					(file-modification-time x)
					(file-modification-time (car x)))
				    x))
			    l)
		       (lambda (a b)
			 (timestamp-less? (car a) (car b))))))

  ; This is generally useful; shuffle to a library and provide
  ; compatibility libraries for systems that don't have it.

  (define (select-least l less?)
    (cond ((null? l) #f)
	  ((null? (cdr l)) (car l))
	  (else
	   (let loop ((least (car l)) (l (cdr l)))
	     (cond ((null? l) least)
		   ((less? (car l) least)
		    (loop (car l) (cdr l)))
		   (else
		    (loop least (cdr l))))))))

  ; Return a rule which will create the target from some source files
  ; that currently exist. 
  ;
  ; If there is no rule which matches based on
  ; a source which exists, we must look at all rules that match purely
  ; on extension, and then attempt to find rules which can be applied
  ; to create a source. Process repeats. We should end up with a set
  ; of rule chains and should pick the one which has the newest source
  ; file. We then cache the rule chains for later use. A rule which
  ; represents the rule chain can be created by simple composition.
  ;
  ; FIXME: if several direct rules apply, should pick the one where
  ; the source is the newest.

  (define (find-rule target)
    (let ((r   (make-project.rules proj))
	  (ext (extension target)))
      (let loop ((r r) (matches '()))
	(cond ((null? r)
	       (find-soft-rule target ext matches))
	      ((and (string=? ext (caaar r))
		    (file-exists? (replace-extension
				   target
				   ext
				   (cdaar r))))
	       (car r))
	      (else
	       (loop (cdr r) (if (string=? ext (caaar r))
				 (cons (car r) matches)
				 matches)))))))

  ; Look for or create a soft rule for the given target. Ext is the
  ; extension of the target.

  (define (find-soft-rule target ext matches)
    (or (soft-rule-match target ext)
	(let ((rules (let loop ((matches matches) (c '()))
		       (if (null? matches)
			   c
			   (let* ((m (car matches))
				  (try (find-rule (replace-extension
						   target
						   (caar m)
						   (cdar m)))))
			     (loop (cdr matches)
				   (if try
				       (cons (make-soft-rule m try) c)
				       c)))))))
	  (set! soft-rules (append rules soft-rules))
	  (if (null? rules)
	      #f
	      (cdr (newest (map (lambda (r)
				  (cons (replace-extension
					 target
					 (caar r)
					 (cdar r))
					r))
				rules)))))))

  ; Create a soft rule: it must make the dependent, then execute the
  ; command for the final link.
  ;
  ; FIXME: should this look at make:pretend?

  (define (make-soft-rule r1 r2)
    (cons (cons (caar r1) (cdar r2))
	  (lambda (t d)
	    (do-make (replace-extension t (extension t) (caar r2)))
	    (call-with-error-control
	     (lambda ()
	       ((cdr r1) t (cons (replace-extension (car d)
						    (extension (car d))
						    (cdar r1))
				 (cdr d))))
	     (errhandler t)))))

  ; Look for a soft rule and return the rule that matches best.

  (define (soft-rule-match target ext)
    (let loop ((r soft-rules) (c '()))
      (cond ((null? r)
	     (if (null? c)
		 #f
		 (cdr (newest c))))
	    ((string=? ext (caaar r))
	     (let ((name (replace-extension target ext (cdaar r))))
	       (if (file-exists? name)
		   (loop (cdr r) (cons (cons name (car r)) c))
		   (loop (cdr r) c))))
	    (else
	     (loop (cdr r) c)))))

  (define (make target cmd deps)
    (for-each do-make deps)
    (if (or (not (file-exists? target))
	    (some? (lambda (d) (newer-than? d target)) deps))
	(call-with-error-control
	 (lambda () (if (make:pretend)
			(begin (display "MAKE: ")
			       (display target)
			       (display " : ")
			       (display deps)
			       (newline))
			(cmd target deps)))
	 (errhandler target))))

  ; Look for target in the target list, then in a rule, and finally
  ; see if the file simply exists.

  (define (do-make target)
    (if (member target have-made)
	#t
	(let* ((targets (make-project.targets proj))
	       (deps    (make-project.deps    proj))
	       (probe   (assoc target targets))
	       (dprobe  (assoc target deps)))
	  (if probe
	      (make target (cdr probe) (if dprobe (cdr dprobe) '()))
	      (let ((r (find-rule target)))
		(cond (r (make target
			       (cdr r)
			       (cons (replace-extension 
				      target
				      (caar r)
				      (cdar r))
				     (if dprobe
					 (cdr dprobe) '()))))
		      ((file-exists? target))
		      (else
		       (make:error 'make:make "Don't know how to make "
				   target)))))
	  (set! have-made (cons target have-made)))))

  (set! have-made '())
  (set! soft-rules '())
  (do-make target)
  #t)

; Our own error handler for portability's sake.

(define (make:error kwd msg . args)
  (display "Error: ")
  (display kwd)
  (display ": ")
  (display msg)
  (for-each display args)
  (newline)
  (reset))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Configuration.

; Calls thunk1, and if thunk1 causes an error to be signalled, calls thunk2.
; If your Scheme system cannot support such a procedure, you may use the
; following, but be sure to clean up any half-built targets manually.

;(define (call-with-error-control thunk1 thunk2) (thunk1))

; Reset resets the Scheme system.
; If your scheme system does not have reset, you may use the following
; or a variation thereof.

;(define (reset) (error "RESET"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Test code. WARNING! Creates foo.sch and bar.sch if they don't exist.

(define (make-test pretend?)
  (if (not (file-exists? "foo.sch"))
      (with-output-to-file "foo.sch" (lambda () #t)))
  (if (not (file-exists? "bar.sch"))
      (with-output-to-file "bar.sch" (lambda () #t)))
  (let ((m (make:new-project "test")))
    (make:rule m ".lap" ".sch" (lambda (t d)
				 (display "Compiling ")
				 (display t)
				 (display " ")
				 (display d)
				 (newline)))
    (make:rule m ".lop" ".lap" (lambda (t d)
				 (display "Assembling ")
				 (display t)
				 (display " ")
				 (display d)
				 (newline)))
    (make:deps m '("foo.lap") '("foo.sh"))
    (make:deps m '("bar.lap") '("foo.sh"))
    (make:targets m '("foo.sh") (lambda (t d)
				  (display "Making foo.sh")
				  (newline)))
    (make:deps m '("foo.heap") '("foo.lop" "bar.lop"))
    (make:targets m '("foo.heap") (lambda (t d)
				    (display "Creating heap: ")
				    (display t)
				    (display " ")
				    (display d)
				    (newline)))
    (make:pretend pretend?)
    (make:make m "foo.heap")))

; eof
