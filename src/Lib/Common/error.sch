; Copyright 1998 Lars T Hansen.               -*- indent-tabs-mode: nil -*-
;
; $Id$
;
; Larceny library -- higher-level error system.

($$trace "error")

; R6RS-style programs should never enter Larceny's debugger,
; because Larceny's R6RS mode is designed for batch-mode
; execution by people who don't know anything about Scheme.
; Programmers use R7RS/ERR5RS modes instead of R6RS mode.

(define (unhandled-exception-error x)
  (let ((out (current-error-port))
        (emode (larceny:execution-mode)))
    (case emode
     ((r6rs)
      (newline out)
      (display "Error: no handler for exception " out)
      (write x out)
      (newline out)
      (if (condition? x)
          (display-condition x out))
      (newline out)
      (display "Terminating program execution." out)
      (newline out)
      (exit 1))
     (else
      ((error-handler) x)))))

; Heuristically recognizes both R6RS-style and Larceny's old-style
; arguments.
;
; The R6RS exception mechanism is used if and only if
;     the program is executing in an R6RS mode, or
;     a custom exception handler is currently installed,
;         and the arguments are not acceptable to the R7RS.

(define (use-r6rs-mechanism? who msg)
  (let ((emode (larceny:execution-mode)))
    (or (eq? emode 'r6rs)
        (and (custom-exception-handlers?)
             (or (symbol? who) (eq? who #f))
             (string? msg)))))

(define (use-r7rs-mechanism? msg)
  (let ((emode (larceny:execution-mode)))
    (and (or (eq? emode 'r7rs)
             (custom-exception-handlers?))
         (string? msg))))

(define (error . args)

  (define (nonstandard-arguments)
    (apply (error-handler) '() args))

  (define (separated irritants)
    (if (null? irritants)
        '()
        (cons " "
              (cons (car irritants) (separated (cdr irritants))))))

  (cond ((null? args)
         (nonstandard-arguments))
        ((null? (cdr args))
         (if (use-r7rs-mechanism? (car args))
             (raise-r6rs-exception (make-error) #f (car args) '())
             (nonstandard-arguments)))
        (else
         (let ((arg1 (car args))
               (arg2 (cadr args))
               (irritants (cddr args))
               (handler (error-handler)))
           (cond ((use-r6rs-mechanism? arg1 arg2)
                  (raise-r6rs-exception (make-error) arg1 arg2 irritants))
                 ((use-r7rs-mechanism? arg1)
                  (raise-r6rs-exception (make-error)
                                        #f arg1 (cons arg2 irritants)))
                 ((or (symbol? arg1) (string? arg1))
                  (apply handler arg1 arg2 (separated irritants)))
                 ((eq? arg1 #f)
                  (apply handler arg2 (separated irritants)))
                 (else
                  ; old-style
                  (nonstandard-arguments)))))))

(define (assertion-violation who msg . irritants)
  (if (or #t (use-r6rs-mechanism? who msg)) ; FIXME
      (raise-r6rs-exception (make-assertion-violation) who msg irritants)
      (apply error who msg irritants)))

(define (reset)
  ((reset-handler)))

; Now uses R6RS exception system.

(define (call-without-errors thunk . rest)
  (let ((fail (if (null? rest) #f (car rest))))
    (call-with-current-continuation
     (lambda (k)
       (with-exception-handler (lambda (exn) (k fail)) thunk)))))

; Old code: clients should use PARAMETERIZE instead.

(define (call-with-error-handler handler thunk)
  (let ((old-handler (error-handler)))
    (dynamic-wind 
     (lambda () (error-handler handler))
     thunk
     (lambda () (error-handler old-handler)))))

; Old code: clients should use PARAMETERIZE instead.

(define (call-with-reset-handler handler thunk)
  (let ((old-handler (reset-handler)))
    (dynamic-wind 
     (lambda () (reset-handler handler))
     thunk
     (lambda () (reset-handler old-handler)))))

; DECODE-ERROR takes a list (describing an error) and optionally
; a port to print on (defaults to the current error port) and
; prints a human-readable error message to the port based on the
; information in the error.
;
; The error is a list.  The first element is a key, the rest depend on the
; key.  There are three cases, depending on the key:
;  - a number:  The error is a primitive error.  There will be three
;               additional values, the contents of RESULT, SECOND, and
;               THIRD.
;  - null:      The key is to be ignored, and the following elements are
;               to be interpreted as though they were arguments passed
;               to the error procedure.
;  - otherwise: The elements are to be interpreted as though they were
;               arguments passed to the error procedure.
;
; There is also a special subcase of the third case above:
; If the key is a condition, and there are no other elements
; of the list, then the condition is assumed to describe an
; unhandled exception that has been raised.

(define (decode-error the-error . rest)
  (let ((who (car the-error))
        (port (if (null? rest) (current-error-port) (car rest))))
    (cond ((and (number? who)
                (list? the-error)
                (= 4 (length the-error)))
           (decode-system-error who 
                                (cadr the-error) 
                                (caddr the-error)
                                (cadddr the-error)
                                port))
          (else
           (newline port)
           (display "Error: " port)
           (cond ((and (condition? who) (null? (cdr the-error)))
                  (display "unhandled condition:" port)
                  (newline port)
                  (display-condition who port))
                 ((not (null? who))
                  (display who port)
                  (display ": " port)))
           (for-each (lambda (x) (display x port)) (cdr the-error))
           (newline port)
           (flush-output-port port)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Transition to R6RS conditions and exception mechanism.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME:  This is an awful hack to connect two exception systems
; via the messages produced by Larceny.
;
; The R6RS/R7RS system uses a stack of exception handlers installed
; by with-exception-handler (which is also used by the guard syntax).
; Those exception handlers take a single argument, which is usually
; (but not always) a condition.
;
; Larceny's traditional mechanism uses a global exception-handler
; (defined in ehander.sch) that handles timer interrupts, keyboard
; interrupts, breakpoints, and signals in addition to the things
; referred to as exceptions by the R7RS.  For R7RS exceptions,
; Larceny's global exception handler calls the current value of
; the error-handler parameter with several arguments as documented 
; in ehandler.sch and described in the comments for decode-error
; above.
;
; Larceny's debugger (such as it is) is called by the error-handler
; installed at heap-building time by lib/Debugger/debug.sch.
;
; Conundrum:  We want an error-handler to repackage Larceny's
; low-level encoding of exceptions generated by the check! mechanism,
; so they can be examined by R7RS/R6RS-style exception handlers,
; but we also want an error-handler that serves as exception hander
; of last resort so the debugger (or whatever) will be called when
; all of the installed exception handlers decline to handle the
; exception.  In other words, we want a sandwich with the repackaging
; layer on top, the R7RS/R6RS-style exception handlers in the middle,
; and a debugging layer on the bottom.
;
; To accomplish that:
;     lib/Debugger/debug.sch installs an error-handler in the R5RS heap
;     the interactive-entry-point (defined in src/Lib/Repl/main.sch)
;         installs an error-handler that repackages Larceny's low-level
;         error data into a condition, parameterizes error-handler to
;         whatever it was before (most likely an error-handler that
;         will call the debugger), and calls raise to give R7RS/R6RS
;         exception handlers a chance to handle the exception before
;         it gets to the exception handler of last resort (defined at
;         the top of this file), which calls the current error-handler.
;
; To complicate things still further, the R7RS error procedure doesn't
; allow a who argument and offers no way to extract who information
; from an error object, so who information is left as part of the
; message unless we're running in R6RS mode.

(define (decode-and-raise-r6rs-exception the-error)
  (if (and (pair? the-error)
           (null? (cdr the-error))
           (condition? (car the-error)))
      (raise (car the-error))
      (let* ((r6rs? (eq? (larceny:execution-mode) 'r6rs))
             (out (open-output-string))
             (msg (begin (decode-error the-error out)
                         (get-output-string out)))
             (larceny-system-prefix "\nError: ")
             (n (string-length larceny-system-prefix))
             (larceny-style?
              (and (< n (string-length msg))
                   (string=? larceny-system-prefix (substring msg 0 n))))
             (msg (if larceny-style?
                      (substring msg n (string-length msg))
                      msg))
             (chars (if larceny-style? (string->list msg) '()))
             (colon (memq #\: chars))
             (who (if colon
                      (substring msg 0 (- (string-length msg) (length colon)))
                      #f))
             (who (if (equal? who "?") #f who))
             (msg (if (and r6rs? colon) (list->string (cdr colon)) msg))
             (c0 (make-assertion-violation))
             (c1 (make-message-condition msg)))
        (raise
         (if who
             (condition c0 (make-who-condition who) c1)
             (condition c0 c1))))))

(define (raise-r6rs-exception c0 who msg irritants)
  (let ((c1 (cond ((or (symbol? who) (string? who))
                   (make-who-condition who))
                  ((eq? who #f)
                   #f)
                  (else
                   (condition
                    (make-violation)
                    (make-who-condition 'make-who-condition)
                    (make-irritants-condition (list who))))))
        (c2 (cond ((string? msg)
                   (make-message-condition msg))
                  (else
                   (condition
                    (make-assertion-violation)
                    (make-who-condition 'make-message-condition)
                    (make-irritants-condition (list msg))))))
        (c3 (make-irritants-condition irritants)))
    (raise
     (if who
         (condition c0 c1 c2 c3)
         (condition c0 c2 c3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Warns of deprecated features.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define issue-deprecated-warnings?
  (make-parameter "issue-deprecated-warnings?" #t))

(define (issue-warning-deprecated name-of-deprecated-misfeature)
  (if (not (memq name-of-deprecated-misfeature already-warned))
      (begin
       (set! already-warned
             (cons name-of-deprecated-misfeature already-warned))
       (if (issue-deprecated-warnings?)
           (let ((out (current-error-port)))
             (display "WARNING: " out)
             (display name-of-deprecated-misfeature out)
             (newline out)
             (display "    is deprecated in Larceny.  See" out)
             (newline out)
             (display "    " out)
             (display url:deprecated out)
             (newline out))))))

(define url:deprecated
  "https://github.com/larcenists/larceny/wiki/DeprecatedFeatures")

; List of deprecated features for which a warning has already
; been issued.

(define already-warned '())

; eof
