; Compiler/make.sch
; Larceny compilation system -- `make' facility.
;
; $Id$
;
; DESCRIPTION
; A project is created with make:new-project.  It consists of rules, 
; dependencies, and targets. 
;
; A _rule_ is an association of two file extensions (target and source) and
; a command.  Given a target file name with an extension that matches the
; target extension, a rule is used to search for a source file name, and
; serves to specify the command that generates the target file from the
; source file.  A command is a procedure of two arguments: the target
; file name and the list of source file names on which the target depends.
; When called, the command must create the target file.
;
; A _dependency_ is an association of two file name lists: target files and
; source files.  It states that each target file depends on all of the
; source files.  Therefore, if any of the source files have a modification
; time newer than the target file, or if the target file does not exist,
; then the target file must be remade from the source files.
; 
; A _target_ associates a list of file names with a command.  The command
; is a procedure of two arguments: the target name and the list of the
; files on which the target depends (computed from the dependencies).
; When called, the command must create the target file.
;
; When make:make is called with a target name, a check is made that the
; target is newer than all the files on which it transitively depends, and
; if this is not the case, then the target is rebuilt.  The transitive
; check may cause files on which the target depends to be rebuilt, and
; so on.
; 
; A command specified as part of a _target_ takes precedence of a command
; specified as part of a _rule_.
;
;
; Make works by computing, for each target, the files on which a target
; depends, and the command that is to be used to rebuild the target once
; those files have been rebuilt.  There four cases, which are applied
; in the following order:
;
; (1) If a target is in the list of targets, then the command is part of
;     the target specification, and the dependees are necessarily part
;     of the dependencies list.
;
; If a target is not in the list of targets, then we must use a rule
; to compute the command.  There are two cases:
;
; (2a) Assume that there is a dependency of the form 'abc.xyz: def.uvw',
;      i.e., abc.xyz depends only on def.uvw.  Then, if there is a rule of
;      the form (.xyz .uvw cmd) and it is possible to make def.uvw, then
;      we can use cmd to create abc.xyz.  The only dependency for abc.xyz
;      is def.uvw.
;
; (2b) Assume that the name of the target is 'abc.xyz'.  Then we must
;      look for rules of the form (.xyz .uvw cmd) where '.uvw' is another
;      extension.  If it is possible to make abc.uvw, then we can use cmd
;      to create abc.xyz.  The dependencies for abc.xyz are any explicit
;      dependencies plus abc.uvw.
;
; Otherwise,
;
; (3)  If the target is not one of the listed targets, and there are no
;      rules that apply to the target, then the target is considered
;      successfully built if and only if it exists.
;
; INTERFACE
;   (make:new-project name)                 => project
;   (make:project? obj)                     => bool
;   (make:rule project ext1 ext2 cmd)       => unspecified
;   (make:deps project files1 files2)       => unspecified
;   (make:targets project targets command)  => unspecified
;   (make:pretend)                          => bool
;   (make:pretend bool)                     => bool
;   (make:debug)                            => bool
;   (make:debug bool)                       => bool
;   (make:make project target)              => unspecified
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
;   - Make:pretend is global (not project-specific); should this change?
;   - Must remove primary dependency from occurring subsequently in the
;     dependency list.
;   - When doing a pretend run, for each file ostensibly created by
;     the make run we should cache its name and time to use later in the
;     simulation.
;   - In general, existence status and modification times could be cached.
;   - There could be a notion of rule precedence to allow A->C to be chosen
;     over A->B and B->C.

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
	(make-project.rules! proj (cons (make-rule ext1 ext2 command)
					rules)))))

(define (make-rule ext1 ext2 cmd) (cons (cons ext1 ext2) cmd))
(define (make-rule.ext1 r) (caar r))
(define (make-rule.ext2 r) (cdar r))
(define (make-rule.cmd r)  (cdr r))

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
; to.  For debugging makefiles.

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


; Turn debugging output on/off.

(define make:debug
  (let ((state #f))
    (lambda args
      (cond ((null? args) state)
	    ((and (null? (cdr args))
		  (boolean? (car args)))
	     (set! state (car args))
	     state)
	    (else
	     (display "Make:debug: strange: " args)
	     state)))))

; Create 'target', unless all its dependencies exist and are older,
; recursively.

(define (make:make proj target)

  (define have-made '())

  (define (errhandler target)
    (lambda ()
      (newline)
      (display "*** Error encountered!") (newline)
      (display "Deleting target file: ")
      (display target)
      (newline)
      (if (not (make:pretend))
	  (delete-file target))))

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

  ; Given a target name, find a rule such that if the source extension of 
  ; the rule is substituted into the target name to get a source name,
  ; then the source can be made.

  (define (find-rule target)
    (let ((r   (make-project.rules proj))
	  (ext (extension target)))
      (let loop ((r r) (matches '()))
	(cond ((null? r)
	       #f)
	      ((and (string=? ext (make-rule.ext1 (car r)))
		    (can-make? (replace-extension
				target
				ext
				(make-rule.ext2 (car r)))))
	       (car r))
	      (else
	       (loop (cdr r)
		     (if (string=? ext (make-rule.ext1 (car r)))
			 (cons (car r) matches)
			 matches)))))))


  ; If 'dep' is of the type (a b) [meaning: a depends on b] and there
  ; is an extension rule that matches the extension of a and the extension
  ; of b, then return the command for that rule.
  ;
  ; If 'dep' is not of the proper form or there is no such rule, return #f.

  (define (rule-controlled-dependency dep)
    (cond ((not dep) #f)
	  ((not (= (length dep) 2))
	   #f)
	  (else
	   (let ((e1 (extension (car dep)))
		 (e2 (extension (cadr dep))))
	     (define (loop r)
	       (cond ((null? r) #f)
		     ((and (string=? e1 (make-rule.ext1 (car r)))
			   (string=? e2 (make-rule.ext2 (car r))))
		      (make-rule.cmd (car r)))
		     (else
		      (loop (cdr r)))))
	     (loop (make-project.rules proj))))))


  ; Given a target, a command to build it, and its list of dependencies,
  ; build all the dependencies and then the target.

  (define (make-target target cmd deps)  
    (for-each make deps)
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


  ; Return #t iff the target can be made (but don't make it).

  (define (can-make? target)
    (or (member target have-made)
	(assoc target (make-project.targets proj))
	(rule-controlled-dependency (assoc target (make-project.deps proj)))
	(find-rule target)
	(file-exists? target)))


  ; Given a target, compute the command and dependencies and then build it.

  (define (make target)
    (if (not (member target have-made))
	(let* ((targets (make-project.targets proj))
	       (deps    (make-project.deps    proj))
	       (probe   (assoc target targets))
	       (dprobe  (assoc target deps)))
	  (cond (probe
		 (make:debugmsg target ": #1")
		 (make-target target (cdr probe) (if dprobe (cdr dprobe) '())))
		((rule-controlled-dependency dprobe)
		 =>
		 (lambda (cmd)
		   (make:debugmsg target ": #2")
		   (make-target (car dprobe) cmd (cdr dprobe))))
		((find-rule target)
		 =>
		 (lambda (r)
		   (make:debugmsg target ": #3")
		   (make-target target
				(make-rule.cmd r)
				(cons (replace-extension 
				       target
				       (make-rule.ext1 r)
				       (make-rule.ext2 r))
				      (if dprobe (cdr dprobe) '())))))
		((file-exists? target)
		 (make:debugmsg target ": #4")
		 #t)
		(else
		 (make:error 'make:make "Don't know how to make "
			     target)))
	  (set! have-made (cons target have-made)))))

  (set! have-made '())
  (make target)
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

(define (make:debugmsg . msgs)
  (if (make:debug)
      (begin (for-each display msgs)
	     (newline))))

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
