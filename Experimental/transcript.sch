; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Transcript ports.  Simple implementation based on user ports and
; a nested REPL.  Starting a transcript enters the nested REPL.  Leaving 
; the nested REPL shuts the transcript off, and shutting the transcript 
; off leaves the nested REPL.
;
; The R5RS is silent about what happens when closing the standard
; input or output, so I have chosen to have CLOSE on either port,
; when a transcript is in effect, signal an error.

(require 'experimental/iosys)

(define transcript-on)
(define transcript-off)

(let ()

  (define reset-continuation #f)
  (define transcript-input #f)
  (define transcript-output #f)
  (define transcript #f)

  (define cin #f)
  (define cout #f)
  (define saved '())                    ; Never contains #!eof

  (define (flush-console-input)
    (let loop ((cs '()))
      (if (char-ready? (cin))
          (let ((c (peek-char (cin))))
            (if (char? c)
                (begin (write-char c transcript)
                       (loop (cons (read-char (cin)) cs)))
                (set! saved (append saved (reverse cs)))))
          (set! saved (append saved (reverse cs))))))

  (define (read-pending-char)
    (if (null? saved)
        #f
        (let ((c (car saved)))
          (set! saved (cdr saved))
          c)))

  (define (char-pending?)
    (not (null? saved)))

  (define (start-transcript filename)
    (set! cin (console-input-port-factory))
    (set! cout (console-output-port-factory))
    (set! saved '())
    (if reset-continuation
        (error "Transcript is already on."))
    (set! transcript (open-output-file filename))
    (set! transcript-input
          (make-input-port
           (lambda (selector)
             (case selector
               ((read)   
                (lambda (datum buf)
                  (let ((c (read-pending-char)))
                    (if c
                        (begin (string-set! buf 0 c)
                               1)
                        (let ((c (read-char (cin))))
                          (if (eof-object? c)
                              'eof
                              (begin (string-set! buf 0 c)
                                     (write-char c transcript)
                                     1)))))))
               ((ready?)
                (lambda (datum)
                  (or (char-pending?)
                      (char-ready? (cin)))))
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
                  (flush-console-input)
                  (do ((i 0 (+ i 1)))
                      ((= i count))
                    (write-char (string-ref buf i) (cout))
                    (write-char (string-ref buf i) transcript))
                  (flush-output-port (cout))
                  'ok))
               ((close)
                (lambda (datum) 
                  (error "The console output port may not be closed.")))
               ((name)  
                (lambda (datum) "transcript-output"))
               (else ???)))
           #f
           'char
           #t))
    (parameterize ((console-input-port-factory  (lambda () transcript-input))
                   (console-output-port-factory (lambda () transcript-output)))
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


