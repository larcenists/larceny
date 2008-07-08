;;; === START largely cut-and-pasted from code in example.sch ===

(define (make-file-menu editor-wnd)
  (define file (make-mnu "File"))
  (let ((w editor-wnd))
    ((file 'append) "Open"       
     (lambda () ((((w 'agent)) 'load-file-cmd))))
    ((file 'append) "Save"       
     (lambda () ((((w 'agent)) 'save-file-cmd))))
    ((file 'append) "Save As..." 
     (lambda () ((((w 'agent)) 'save-file-as-cmd))))

    ((file 'append) "Kill Busy Loop" 
     (lambda () (set! *request-kill-busy-loop* #t))))
  file)

(define (make-edit-menu editor-wnd)
  (define edit (make-mnu "Edit"))
  
  (define (wnd->cut-cmd wnd)
    (lambda () 
      (let ((displayln (lambda (x) (write x) (newline)))
	    (ra ((wnd 'agent))))
	(displayln 'edit..cut)
	(cond (((ra 'selectionstring))
	       => (lambda (text)
		    (clipboard-set-text! text)
		    ((ra 'delete-char-at-point!)))))
	;; XXX support Image cutting
	(unspecified))))
  
  (define (wnd->copy-cmd wnd)
    (lambda ()
      (let ((displayln (lambda (x) (write x) (newline)))
	    (ra ((wnd 'agent))))
	(displayln 'edit..copy)
	(clipboard-set-text! ((ra 'selectionstring)))
	;; XXX support Image copying
	(unspecified))))
  
  (define (wnd->paste-cmd wnd)
    (lambda () 
      (let ((displayln (lambda (x) (write x) (newline)))
	    (ra ((wnd 'agent)))
	    (clip-text (clipboard-get-text)))
	(displayln 'edit..paste)
	(cond ((and clip-text
		    (string? clip-text))
	       (for-each (lambda (c)
			   ((ra 'insert-char-at-point!) c))
			 (string->list clip-text))))
	;; XXX support Image pasting
	(unspecified))))
  
  ((edit 'append) "Cut"        (wnd->cut-cmd editor-wnd))
  ((edit 'append) "Copy"       (wnd->copy-cmd editor-wnd))
  ((edit 'append) "Paste"      (wnd->paste-cmd editor-wnd))
  
  edit)

;;; === FINIS largely cut-and-pasted from code in example.sch ===
