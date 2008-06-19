(define (make-beginning-editor-agent wnd textmodel)
  (let ((agent (extend-with-file-handling
		(extend-with-paren-matching
		 (extend-with-auto-indentation textmodel)))))
    ;; Hopefully this will go away in the future...
    (extend-object agent self
     ((on-close) (set! *request-kill-busy-loop* #t)))))

;; The busy-loop forces the timer interrupts to occur, which is the
;; primary means for driving the event handling loop.  One could also
;; use the show-dialog method on a window, but it is not clear how
;; that technique for driving the GUI would interact with the tasking
;; library, which Felix hopes to use to get green threads soon...
(define *request-kill-busy-loop* #f)
(define (busy-loop)
  (set! *request-kill-busy-loop* #f)
  (do () (*request-kill-busy-loop* 'done)))

(define (extend-with-beginner-repl initial-program textmodel)
  ;; interpret-beginning-definition mutates its globals arg.
  (define globals (make-hashtable symbol-hash eq?))

  ;; This *should* be imported from Beginning/interpreter, but its not
  ;; in a suitable scope in that file...
  (define (beginning-definition? def/exp)
    (and (pair? def/exp)
         (or (eq? 'define (car def/exp))
             (eq? 'define-struct (car def/exp)))))

  ;; Start by evaluating all of the definitions provided by the user
  ;; in their source text.
  (let loop ((pgm initial-program))
    (cond ((null? pgm) 'done-with-initial-program-evaluation)
	  ((beginning-definition? (car pgm))
	   (interpret-beginning-definition (car pgm) globals)
	   (loop (cdr pgm)))
	  (else
	   (let ((exp-cont (make-defn-cont 'fake-globals #f)))
	     (interpret-beginning-expression (car pgm) globals exp-cont)))))
  
  (let ((repl-agent
	 (extend-with-repl
	  (extend-with-paren-matching
	   (extend-with-auto-indentation textmodel)))))
    (extend-object repl-agent beginner-repl-agent
     ((evaluate sexp) 
      (cond ((beginning-definition? sexp)
	     (error 'beginner-repl ": no definitions allowed in repl"))
	    (else
	     (let ((exp-cont (make-defn-cont 'fake-globals #f)))
	       (let ((saved-stepping? stepping?))
		 (dynamic-wind 
		     (lambda () 
		       (set! saved-stepping? stepping?)
		       (set! stepping? #f))
		     (lambda () 
		       (interpret-beginning-expression sexp globals exp-cont))
		     (lambda () 
		       (set! stepping? saved-stepping?))
		     )))))))))

(define (make-beginning-repl sexps)
  (make-wnd 'double-buffered 'title "Beginning Evaluator" 
	    'make-agent (editor-agent-maker 
			 (lambda (wnd textmodel) 
			   (extend-with-beginner-repl
			    sexps
			    textmodel)))))

(define read-all-sexps-in-editor 
  (let ((read-all 
	 (lambda (input-port)
	   (let loop ()
	     (let ((x (read input-port)))
	       (if (eof-object? x) '() (cons x (loop))))))))
    (lambda (editor)
      (call-with-current-continuation
       (lambda (escape)
	 (let* ((agt ((editor 'agent)))
		(txt ((agt 'textstring)))
		(sexps (call-with-error-handler 
			(lambda args (escape #f))
			(lambda ()
			  (call-with-input-string txt read-all)))))
	   sexps))))))

(define (extend-with-beginner-stepper initial-program textmodel)
  (let* ((c/vals call-with-values)
	 (self #f) ;; necessitated by incremental-step! construction below
	 (handle-configurations!
	  (lambda (c1 c2)
	    ((self 'handle-configurations!) c1 c2)))
	 (incremental-step! 
	  (make-incremental-stepper 
	   initial-program handle-configurations!)))
    (extend-object textmodel stepper-agent 
     ((step!) ;; main external interface
      ;; do this here so that we hook in extensions of this object before
      ;; invoking handle-configurations!
      (set! self stepper-agent)
      (incremental-step!))
     ((handle-configurations! c1 c2)
      (c/vals (lambda () (configuration->pseudocode c1 #t))
       (lambda (str1 hilite-range1)
	 (c/vals (lambda () (configuration->pseudocode c2 #f))
	  (lambda (str2 hilite-range2)
	    (begin (display `(,stepper-agent handle-configurations! 
					     ,str1 ==> ,str2))
		   (newline))

	    (let* ((separator "\n==>\n")
		   (new-text (string-append str1 separator str2))
		   (lo1 (car hilite-range1))
		   (hi1 (cadr hilite-range1))
		   (lo2 (car hilite-range2))
		   (hi2 (cadr hilite-range2))
		   (lo2 (+ (string-length str1) (string-length separator) lo2))
		   (hi2 (+ (string-length str1) (string-length separator) hi2))
		   (purple (name->col "Purple"))
		   (green  (name->col "Green"))
		   )
	      
	      ((stepper-agent 'set-textstring!) new-text)

	      ((stepper-agent 'color-foreground-stably!) lo1 hi1 purple)
	      ((stepper-agent 'color-foreground-stably!) lo2 hi2 green)

	      )

	    ((((stepper-agent 'wnd)) 'update))
	    ))))))))

(define make-simple-button
  (lambda (title click-routine)
    (make-btn 'title title
	      'make-agent 
	      (lambda args
		(make-root-object button-agent
				  ((on-mouseclick x y)
				   (click-routine x y)))))))

(define (make-beginning-stepper sexps)
  (let* ((wnd 
	  (make-wnd 'double-buffered 'title "Beginning Stepper"
		    'make-agent
		    (editor-agent-maker
		     (lambda (wnd textmodel)
		       (extend-with-beginner-stepper sexps textmodel)))))
	 (step-action (lambda args ((((wnd 'agent)) 'step!))))
	 (step-button (make-simple-button "=>" step-action)))
    ((wnd 'push-buttons) step-button)
    wnd))

'(define-syntax let*-tracing
  (syntax-rules () 
    ((_ () BODY ...) (let () BODY ...))
    ((_ ((X EXP) REST ...) BODY ...)
     (begin (display 'X) (newline)
	    (let ((X EXP)) (let*-tracing (REST ...) BODY ...))))))

(define (demo-1)
  (let* ((editor-wnd
	  (make-wnd 'double-buffered 
		    'title "Beginning Editor"
		    'make-agent 
		    (editor-agent-maker
		     (lambda (w tm)
		       (let ((agent (make-beginning-editor-agent w tm)))
			 (extend-object agent self
					((on-close) 
					 (set! *request-kill-busy-loop* #t)
					 (unspecified)))))))))
    
    (let* ((file-menu (make-file-menu editor-wnd))
	   (edit-menu (make-edit-menu editor-wnd)))
      ((editor-wnd 'push-menus) file-menu edit-menu))

    (let* ((run-action (lambda args 
			 (display (string-append "Run clicked"))
			 (newline)
			 (let* ((sexps (read-all-sexps-in-editor editor-wnd))
				(repl (make-beginning-repl sexps)))
			   ((((repl 'agent)) 'prompt!))
			   ((repl 'show)))))
	   (run-button (make-simple-button "Run" run-action))
	   (step-action (lambda args 
			  (display (string-append "Step clicked"))
			  (newline)
			  (let* ((sexps (read-all-sexps-in-editor editor-wnd))
				 (stepper (make-beginning-stepper sexps)))
			    ((stepper 'show)))))
	   (step-button (make-simple-button "Step" step-action)))
      ((editor-wnd 'push-buttons) run-button step-button))
    ((editor-wnd 'show))
    (busy-loop)
    editor-wnd))
