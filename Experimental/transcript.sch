; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Transcript ports.  Simple implementation based on user ports and
; a new REPL.  Starting a transcript enters a new REPL.  Leaving the
; new REPL shuts the transcript off, and shutting the transcript off
; leaves the new REPL.
;
; The R5RS is silent about what happens when closing the standard
; input or output, so I have chosen to have CLOSE on either port,
; when a transcript is in effect, signal an error.

(define transcript-on)
(define transcript-off)

(let ()

  (define reset-continuation #f)
  (define transcript-input #f)
  (define transcript-output #f)
  (define transcript #f)

  (define cin console-input-port)
  (define cout console-output-port)

  (define (start-transcript filename)
    (if reset-continuation
        (error "Transcript is already on."))
    (set! transcript (open-output-file filename))
    (set! transcript-input
          (make-input-port
           (lambda (selector)
             (case selector
               ((read)   
                (lambda (datum buf)
                  (let ((c (read-char (cin))))
                    (if (eof-object? c)
                        'eof
                        (begin (string-set! buf 0 c)
                               (write-char c transcript)
                               1)))))
               ((ready?)
                (lambda (datum)
                  (char-ready? (cin))))
               ((close)
                (lambda (datum)
                  (error "The console input port may not be closed.")))
               ((name) 
                (lambda (datum) "transcript-input"))
               (else ???)))
           #f
           'char))
    (set! transcript-output
          (make-output-port
           (lambda (selector)
             (case selector
               ((write) 
                (lambda (datum buf count)
                  (do ((i 0 (+ i 1)))
                      ((= i count))
                    (write-char (string-ref buf i) (cout))
                    (write-char (string-ref buf i) transcript))
                  (flush-output-port (cout))))
               ((close)
                (lambda (datum) 
                  (error "The console output port may not be closed.")))
               ((name)  
                (lambda (datum) "transcript-output"))
               (else ???)))
           #f
           'char
           #t))

    ; FIXME:
    ; This is neat but won't work because the REPL is closed in a
    ; different environment.  Should the REPL be exposed more,
    ; should we re-load the REPL, or should there be a mechanism
    ; for overriding these procedures?  (call-with-console-factory ...)

    (fluid-let ((console-input-port  (lambda () transcript-input))
                (console-output-port (lambda () transcript-output)))
      (display "Transcript started on ")
      (display filename)
      (display ".")
      (newline)
      (call-with-current-continuation
       (lambda (k)
         (set! reset-continuation k)
         (repl))))
    (set! reset-continuation #f)
    (close-output-port transcript))

  (define (stop-transcript)
    (if (not reset-continuation)
        (error "Transcript is not on."))
    (reset-continuation #f))

  (set! transcript-on start-transcript)
  (set! transcript-off stop-transcript)
  'transcript)

; eof


