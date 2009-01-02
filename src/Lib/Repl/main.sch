; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Bootstrap code.

($$trace "main")

; Public procedures.

(define herald
  (make-parameter "herald" #f))

(define (dump-interactive-heap filename)
  (dump-heap filename interactive-entry-point))


; Entry point in a bootstrap heap.  Called by go.

(define (main argv)
  ($$trace "In main")
  (init-toplevel-environment)
  (setup-interrupt-and-error-handlers)
  (evaluator interpret)
  (issue-warnings #t)              ; Warnings are off during bootstrapping
  (issue-deprecated-warnings? #t)   ; Warnings are off during bootstrapping
  (interactive-entry-point argv))


; Entry point in a saved interactive heap.

(define (interactive-entry-point argv)
  (for-each eval *interactive-eval-list*) ; FIXME: used only for JavaDot?
  ($$trace "In interactive-entry-point")
  (command-line-arguments argv)
  (standard-timeslice (most-positive-fixnum))
  (enable-interrupts (standard-timeslice))

  (let* ((features (system-features))

         (get-feature
          (lambda (name)
            (let ((probe (assq name features)))
              (and probe (cdr probe)))))

         (adjust-case-sensitivity!
          (lambda ()
            (case-sensitive? (get-feature 'case-sensitivity))))

         (adjust-transcoder!
          (lambda ()
            (let ((t (get-feature 'transcoder))
                  (rep (get-feature 'char-representation)))
              (cond ((and t (> t 0))
                     (default-transcoder t)
                     (console-io/initialize))
                    ((eq? rep 'unicode)
                     (default-transcoder
                      (make-transcoder (utf-8-codec)))
                     (console-io/initialize))))))

         ; FIXME: do all varieties support all these switches?

         (adjust-safety!
          (lambda (safety)
            (let* ((emode (get-feature 'execution-mode))
                   (dargo? (eq? 'dargo emode))
                   (settings
                    (case safety
                     ((0)  `(begin (runtime-safety-checking #f)
                                   (faster-arithmetic #t)))
                     ((1)  `(begin (runtime-safety-checking #t)
                                   (catch-undefined-globals (not ,dargo?))
                                   (faster-arithmetic #f)))
                     (else `(begin (runtime-safety-checking #t)
                                   (catch-undefined-globals #t)
                                   (faster-arithmetic #f))))))
              (eval settings (interaction-environment)))))

         ; FIXME

         (adjust-optimization!
          (lambda (opt)
            (case opt
             ((0)
              (eval '(begin (control-optimization #f)
                            (global-optimization #f))
                    (interaction-environment))))))

         (add-require-path!
          (lambda ()

            ; Given a string containing a possibly empty sequence
            ; of library paths separated by some special character
            ; (a semicolon under Windows, otherwise a colon),
            ; returns a list of the library paths (as strings).

            (define (list-of-paths path separator)
              (let* ((pchars (string->list path))
                     (probe (memv separator pchars)))
                (cond (probe
                       (let ((path1 (substring path
                                               0
                                               (- (string-length path)
                                                  (length probe))))
                             (path2 (list->string (cdr probe))))
                         (cons path1 (list-of-paths path2 separator))))
                      ((string=? path "")
                       '())
                      (else
                       (list path)))))

            (define (add-absolute-path! path)
              (current-require-path (cons path (current-require-path))))

            (define (add-path! path)
              (cond ((string=? path "") #t)
                    ((absolute-path-string? path)
                     (add-absolute-path! path))
                    (else
                     (add-absolute-path!
                      (string-append (current-directory) "/" path)))))

            (let* ((path (get-feature 'library-path))
                   (path (if (string=? path "")
                             (getenv "LARCENYLIBPATH")  ; FIXME
                             path))
                   (path (if (string? path) path #f))
                   (os (get-feature 'os-name))
                   (separator (if (string=? os "Win32") #\; #\:)))
              (if path
                  (for-each add-path!
                            (reverse (list-of-paths path separator)))))))

         (aeryn-mode!
          (lambda ()
            (let ((env (interaction-environment)))
              (eval '(begin
                      (require 'r6rsmode)
                      (larceny:load-r6rs-package))
                    env)
              (let* ((aeryn-fasl-evaluator (eval 'aeryn-fasl-evaluator env))
                     (aeryn-evaluator (eval 'aeryn-evaluator env)))
                (fasl-evaluator aeryn-fasl-evaluator)
                (load-evaluator aeryn-evaluator)
                (repl-evaluator aeryn-evaluator)))))

         (emode (get-feature 'execution-mode)))

    (case emode
     ((r5rs err5rs)
      (failsafe-load-init-files)
      (failsafe-process-arguments)
      (if (herald)
          (writeln (herald)))
      (adjust-case-sensitivity!)
      (adjust-transcoder!)
      (adjust-safety! (get-feature 'safety))
      (add-require-path!)
      (if (eq? emode 'err5rs)
          (begin (aeryn-mode!)
                 (writeln "ERR5RS mode (no libraries have been imported)")))
      (r5rs-entry-point argv))

     ; R6RS modes are batch modes, so we want to exit rather
     ; than enter the debugger.

     ((dargo)
      (adjust-case-sensitivity!)
      (adjust-transcoder!)
      (adjust-safety! (get-feature 'safety))
      (adjust-optimization! 2)                            ; FIXME
      (add-require-path!)
      (aeryn-mode!)
      (parameterize ((error-handler
                      (lambda the-error
                        (parameterize ((print-length 7)
                                       (print-level 7))
                          (decode-and-raise-r6rs-exception the-error))))
                     (issue-deprecated-warnings? #f))
        ; Twobit has its own issue-warnings switch.
        (eval '(issue-warnings #f) (interaction-environment))
        (let* ((pgm (get-feature 'top-level-program))
               (input (if (string=? pgm "")
                          (do ((x (read) (read))
                               (forms '() (cons x forms)))
                              ((eof-object? x)
                               (reverse forms)))
                          pgm)))
          (if (string? input)
              (eval (list 'run-r6rs-program input)
                    (interaction-environment))
              (eval (list 'run-r6rs-forms (list 'quote input))
                    (interaction-environment))))
        (exit 0)))

     ((spanky)
      (display "Larceny's R6RS-conforming mode isn't implemented yet.")
      (newline)
      (display "Please use Larceny's R6RS-compatible mode instead.")
      (newline)
      (exit 1))
     (else
      (display "Unrecognized execution mode: ")
      (write (get-feature 'execution-mode))
      (newline)
      (exit 1)))))


; Entry point for R5RS and ERR5RS execution modes.
; Called only by interactive-entry-point, above.

(define (r5rs-entry-point argv)
  (start-repl)
  (exit 0))


; Error/reset/interrupt handling.

(define (setup-interrupt-and-error-handlers)
  (error-handler
   (let ((old-handler (error-handler)))
     (lambda error
       (parameterize ((error-handler
                       (lambda error
                         ($$debugmsg "Error handler signalled an error")
                         (cond ((string? (car error))
                                ($$debugmsg (car error)))
                               ((and (null? (car error))
                                     (not (null? (cdr error)))
                                     (string? (cadr error)))
                                ($$debugmsg (cadr error)))
                               ((number? (car error))
                                ($$debugmsg (number->string (car error)))))
                         (exit))))
         (with-output-to-port (console-output-port)
           (lambda ()
             (apply old-handler error)))))))

  (timer-interrupt-handler
   (lambda ()
     (enable-interrupts (standard-timeslice))))

  (keyboard-interrupt-handler
   (lambda ()
     (writeln "Keyboard interrupt.")
     (reset))))


; Init file loading.

(define (failsafe-load-init-files)
  (map failsafe-load-file (osdep/find-init-files)))

(define (failsafe-process-arguments)
  (let ((argv (command-line-arguments)))
    (let loop ((i 0))
      (cond 
       ((>= i (vector-length argv)) #t)
       (else
        (let ((arg (vector-ref argv i)))
          (cond 
           ((or (string=? arg "-e")
                (string=? arg "--eval"))
            (failsafe-eval-thunk 
             (lambda () 
               (retract-eof 
                (read (open-input-string (vector-ref argv (+ i 1))))))
             (list "Error parsing argument " (+ i 1)))
            (loop (+ i 2)))
           ((and (> (string-length arg) 0)
                 (char=? (string-ref arg 0) #\-))
            (writeln "Error unrecognized option " arg)
            (loop (+ i 1)))
           (else
            (if (file-exists? arg)
                (failsafe-load-file arg))
            (loop (+ i 1))))))))))

;; retract-eof : Any -> Any

(define (retract-eof x)
  (cond ((eof-object? x) (error "Encountered EOF"))
        (else x)))
  
;; failsafe-eval-thunk : (-> S-exp) [Listof Any] -> Any

(define (failsafe-eval-thunk arg-thunk error-mesgs)
  (call-with-current-continuation
   (lambda (k)
     (call-with-reset-handler 
      (lambda ()
        (apply writeln error-mesgs)
        (k #f))
      (lambda ()
        (failsafe-eval (arg-thunk)))))))

;; failsafe-eval-string : S-exp -> Any

(define (failsafe-eval arg-exp)
  (call-with-current-continuation
   (lambda (k)
     (call-with-reset-handler
      (lambda ()
        (writeln "Error while evaluating " arg-exp)
        (k #f))
      (lambda ()
        (eval arg-exp))))))

;; failsafe-load-file : String -> Any

(define (failsafe-load-file filename)
  (call-with-current-continuation
   (lambda (k)
     (call-with-reset-handler
      (lambda ()
        (writeln "Error while loading " filename)
	(k #f))
      (lambda ()
	(load filename))))))

(define (writeln . xs)
  (let ((p (console-output-port)))
    (for-each (lambda (x) (display x p)) xs)
    (newline p)))

; eof
