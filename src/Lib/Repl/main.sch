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
  (let* ((exit-status
          (call-with-current-continuation
           (lambda (k)
             (exit-continuation k)
             (for-each eval *interactive-eval-list*) ; FIXME: only for JavaDot?
             ($$trace "In interactive-entry-point")
             (interactive-entry-point0 argv)
             0))))
    (run-exit-procedures)
    (emergency-exit exit-status)))

(define (interactive-entry-point0 argv)
  (command-line-arguments argv)
  (standard-timeslice (most-positive-fixnum))
  (enable-interrupts (standard-timeslice))

  (let* ((clr? (equal? "CLR" (cdr (assq 'arch-name (system-features)))))

         (ignored (if clr? (clr-process-arguments))) ; FIXME

         (features (system-features))

         (get-feature
          (lambda (name)
            (let ((probe (assq name features)))
              (and probe (cdr probe)))))

         (adjust-case-sensitivity!
          (lambda ()
            (let ((flag (get-feature 'case-sensitivity)))
              (case-sensitive? flag)
              (port-folds-case! (current-input-port) (not flag)))))

         (adjust-transcoder!
          (lambda ()
            (let ((t (get-feature 'transcoder))
                  (rep (get-feature 'char-representation))
                  (os (get-feature 'os-name)))
              (cond ((and t (> t 0))
                     (default-transcoder t)
                     (console-io/initialize))
                    ((string=? os "Win32")
                     ; FIXME: Windows would prefer UTF-16
                     (default-transcoder
                      (make-transcoder (latin-1-codec))))
                    ((eq? rep 'unicode)
                     (default-transcoder
                      (make-transcoder (utf-8-codec)))
                     (console-io/initialize))))))

         ; Compiler switches are defined only in released heaps,
         ; so we use them only if safety is other than 1 or
         ; the execution mode is other than r5rs.

         (adjust-safety!
          (lambda (safety)
            (let* ((emode (larceny:execution-mode))
                   (r6rs? (eq? 'r6rs emode)) 
                   (settings
                    (if (and (eq? emode 'r5rs) (= safety 1))
                        #f
                        (case safety
                         ((0)  `(begin (runtime-safety-checking #f)
                                       (faster-arithmetic #t)))
                         ((1)  `(begin (runtime-safety-checking #t)
                                       (catch-undefined-globals (not ,r6rs?))
                                       (faster-arithmetic #f)))
                         (else `(begin (runtime-safety-checking #t)
                                       (catch-undefined-globals #t)
                                       (faster-arithmetic #f)))))))
              (if (not clr?)                                            ; FIXME
                  (eval settings (interaction-environment))))))

         ; FIXME

         (adjust-optimization!
          (lambda (opt)
            (case opt
             ((0)
              (if (not clr?)                                            ; FIXME
                  (eval '(begin (control-optimization #f)
                                (global-optimization #f))
                        (interaction-environment)))))))

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

            (define (make-absolute path)
              (cond ((absolute-path-string? path)
                     path)
                    (else
                     (string-append (current-directory) "/" path))))

            (let* ((path  (get-feature 'library-path))
                   (path2 (get-feature 'library-path2))
                   (path (if (string=? path "")
                             (getenv "LARCENY_LIBPATH")  ; FIXME
                             path))
                   (path (if (string? path) path ""))
                   (os (get-feature 'os-name))
                   (separator (if (string=? os "Win32") #\; #\:)))
              (current-require-path
               (append (map make-absolute (list-of-paths path separator))
                       (current-require-path)
                       (map make-absolute (list-of-paths path2 separator)))))))

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

         (disallow-non-r6rs-syntax!
          (lambda (p)
            (io/port-allows-r6rs-weirdness! p #t)
            (io/port-allows-r7rs-weirdness! p #f)
            (io/port-allows-larceny-weirdness! p #f)
            (io/port-allows-traditional-weirdness! p #f)
            (io/port-allows-mzscheme-weirdness! p #f)))

         (emode (get-feature 'execution-mode)))

    (case emode
     ((r5rs err5rs r7rs r7r6)
      (if clr?                                      ; FIXME (ticket #547)
          (begin (failsafe-load-init-files)
                 (failsafe-process-arguments)))
      (let ((pgm (get-feature 'top-level-program)))
        (if (and (herald)
                 (or (not (string? pgm))
                     (string=? pgm "")))
            (writeln (herald))))
      (adjust-transcoder!)
      (adjust-case-sensitivity!)
      (adjust-safety! (get-feature 'safety))
      (if (not clr?)                                ; FIXME (ticket #547)
          (begin (failsafe-load-init-files)
                 (failsafe-process-arguments)))
      (case emode
       ((err5rs r7rs r7r6)
        (aeryn-mode!)))
      (case emode
       ((err5rs)
        (writeln "ERR5RS mode (no libraries have been imported)"))
       ((r7rs)
        ((repl-evaluator) '(import (scheme base))))
       ((r7r6)
        ((repl-evaluator) '(import (larceny r7r6)))))
      (add-require-path!)
      (let ((pgm (get-feature 'top-level-program))
            (original-handler (error-handler)))
        (parameterize ((error-handler
                        (case emode
                         ((r5rs) original-handler)
                         (else
                          (lambda the-error
                            (parameterize ((error-handler original-handler))
                             (decode-and-raise-r6rs-exception the-error)))))))
         (cond ((and (memq emode '(r7rs r7r6))
                     (not (string=? pgm "")))
                (eval (list 'run-r6rs-program pgm)
                      (interaction-environment)))
               ((and (not (string=? pgm ""))
                     (file-exists? pgm))
                (failsafe-load-file pgm))
               (else
                (r5rs-entry-point argv))))))

     ; R6RS mode is a batch mode, so we want to exit rather
     ; than enter the debugger.

     ((r6rs)
      (if clr?                                            ; FIXME
          (begin (failsafe-load-init-files)
                 (failsafe-process-arguments)))
      (adjust-transcoder!)
      (adjust-case-sensitivity!)
      (adjust-safety! (get-feature 'safety))
      (adjust-optimization! 2)                            ; FIXME
      (add-require-path!)
      (aeryn-mode!)
      (parameterize ((error-handler
                      (lambda the-error
                        (parameterize ((print-length 7)
                                       (print-level 7))
                          (decode-and-raise-r6rs-exception the-error))))
                     (issue-deprecated-warnings? #f)
                     (read-r7rs-weirdness? #f)
                     (read-larceny-weirdness? #f)
                     (read-traditional-weirdness? #f)
                     (read-mzscheme-weirdness? #f))

        (io/port-recognizes-javadot-symbols! (current-input-port) #f)
        (disallow-non-r6rs-syntax! (current-input-port))
        (disallow-non-r6rs-syntax! (current-output-port))
        (disallow-non-r6rs-syntax! (current-error-port))

        ; Twobit has its own issue-warnings switch.
        ; FIXME: not working in Common Larceny
        (if (not clr?)
            (eval '(issue-warnings #f) (interaction-environment)))
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

     (else
      (display "Unrecognized execution mode: ")
      (write emode)
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

;;; FIXME: Larceny shouldn't parse anything past -- on the command line.
;;; It now parses past -- only in R5RS mode.  Even then it parses past
;;; only if no top-level program has been specified.
;;; FIXME: Common Larceny is an exception, which makes this complicated.

(define (failsafe-process-arguments)
  (let ((argv (command-line-arguments))
        (emode (larceny:execution-mode)))
    (let loop ((i 0))
      (cond 
       ((>= i (vector-length argv)) #t)
       (else
        (let ((arg (vector-ref argv i)))
          (cond
           ((and (string=? arg "--")
                 (string=? "CLR" (cdr (assq 'arch-name (system-features)))))
            ; FIXME: Common Larceny is the oddball here
            (command-line-arguments
             (list->vector
              (cdr (member "--" (vector->list argv))))))
           ((not (eq? emode 'r5rs))
            #t)
           ((< 0 (string-length
                  (cdr (assq 'top-level-program (system-features)))))
            #t)
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

; FIXME: Native and Petit Larceny use C code to parse the command line,
; but Common Larceny duplicates some of that parsing here.
; FIXME: no checking of library-path and top-level-program parameters

(define (clr-process-arguments)
  (let* ((features (system-features))
         (argv (command-line-arguments))

         (clr:case-sensitivity #f)
         (clr:execution-mode #f)
         (clr:library-path #f)
         (clr:top-level-program #f))

    (define (return! args)
      (command-line-arguments args)
      (if clr:case-sensitivity
          (set! features
                (cons (cons 'case-sensitivity clr:case-sensitivity)
                      features)))
      (if clr:execution-mode
          (set! features
                (cons (cons 'execution-mode clr:execution-mode)
                      features)))
      (if clr:library-path
          (set! features
                (cons (cons 'library-path clr:library-path)
                      features)))
      (if clr:top-level-program
          (set! features
                (cons (cons 'top-level-program clr:top-level-program)
                      features)))
      (set! system-features
            (lambda () features))
      (eval `(set! system-features
                   (lambda () ',features))
            (interaction-environment))
      (unspecified))

    (let loop ((i 0)
               (args '()))
      (cond 
       ((>= i (vector-length argv))
        (return! (list->vector (reverse args))))
       (else
        (let ((arg (vector-ref argv i)))
          (cond
           ((member arg '("-fold-case" "--fold-case" "/fold-case"))
            (set! clr:case-sensitivity? #f)
            (loop (+ i 1) args))

           ;; FIXME: ("-r7rs" "--r7rs" "/r7rs" "-r7r6" "--r7r6" "/r7r6")
           ;; goes here eventually

           ((member arg '("-err5rs" "--err5rs" "/err5rs"))
            (set! clr:execution-mode 'err5rs)
            (loop (+ i 1) args))
           ((member arg '("-r6rs" "--r6rs" "/r6rs"))
            (set! clr:execution-mode 'r6rs)
            (loop (+ i 1) args))
           ((and (member arg '("-path" "--path" "/path"))
                 (< (+ i 1) (vector-length argv)))
            (set! clr:library-path (vector-ref argv (+ i 1)))
            (loop (+ i 2) args))
           ((and (member arg '("-program" "--program" "/program"))
                 (< (+ i 1) (vector-length argv)))
            (set! clr:top-level-program (vector-ref argv (+ i 1)))
            (loop (+ i 2) args))
           ((string=? arg "--")
            (let ((args (append (reverse args)
                                (member "--" (vector->list argv)))))
              (return! (list->vector args))))

           ;; All other command-line arguments are saved for later.

           ((and (or (string=? arg "-e")
                     (string=? arg "--eval"))
                 (< (+ i 1) (vector-length argv)))
            (loop (+ i 2)
                  (cons (vector-ref argv (+ i 1))
                        (cons arg args))))
           ((and (> (string-length arg) 0)
                 (char=? (string-ref arg 0) #\-))
            (writeln "Error unrecognized option " arg)
            (loop (+ i 1) args))
           (else
            (loop (+ i 1) (cons arg args))))))))))

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
