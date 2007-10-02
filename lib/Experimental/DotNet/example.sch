;; Demo of how to start up wnd with behavior dictated by an agent.

(load "lib/Experimental/DotNet/collections.sch")
(load "lib/Experimental/DotNet/simple-inspection.sch")
(load "lib/Experimental/DotNet/simple-reflection.sch")
(load "lib/Experimental/DotNet/protoobj.sch")
(load "lib/Experimental/DotNet/toolsmith-shared.sch")
(load "lib/Experimental/DotNet/toolsmith-form.sch")
(load "lib/Experimental/DotNet/gui-inspection.sch")

(define editor-wnd
  (make-wnd 'double-buffered 'title "DrStrange"
            'make-agent (editor-agent-maker make-scheme-editor-agent)))

(define repl-wnd
  (make-wnd 'double-buffered 'title "StrangeRepl" 
            'make-agent (editor-agent-maker make-read-eval-print-loop-agent)))

;; The busy-loop forces the timer interrupts to occur, which is the
;; primary means for driving the event handling loop.  One could also
;; use the show-dialog method on a window, but it is not clear how
;; that technique for driving the GUI would interact with the tasking
;; library, which Felix hopes to use to get green threads soon...
(define *request-kill-busy-loop* #f)
(define (busy-loop)
  (set! *request-kill-busy-loop* #f)
  (do () (*request-kill-busy-loop* 'done)))

(define file (make-mnu "File"))
(let ((w editor-wnd))
  ((file 'append) "Open"       (lambda () ((((w 'agent)) 'load-file-cmd))))
  ((file 'append) "Save"       (lambda () ((((w 'agent)) 'save-file-cmd))))
  ((file 'append) "Save As..." (lambda () ((((w 'agent)) 'save-file-as-cmd))))

  ((file 'append) "Kill Busy Loop" 
   (lambda () (set! *request-kill-busy-loop* #t))))
  
(define edit (make-mnu "Edit"))
((edit 'append) "Cut"        (lambda () (displayln `(selected edit..cut))))
((edit 'append) "Copy"       (lambda () (displayln `(selected edit..copy))))
((edit 'append) "Paste"      (lambda () (displayln `(selected edit..paste))))

((editor-wnd 'push-menus) file edit)

(define (start-example)
  ((editor-wnd 'show))
  ((repl-wnd 'show))
  ((((((repl-wnd 'agent)) 'backing-agent)) 'prompt!))
  (busy-loop))
