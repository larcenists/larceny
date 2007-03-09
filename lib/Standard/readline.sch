;; Interface to libreadline, including repl upgrade.
;;
;; readline: -> MaybeString
;; readline: String -> MaybeString
;; Reads a line from the terminal, using libreadline; returns #f on eof.
;;
;; add-history: String ->
;; Adds a string to the readline history.  (Doesn't add blanks or
;; duplicates.)
;;
;; make-readline-console-factory: -> (-> InputPort)
;; make-readline-console-factory: StringParameter -> (-> InputPort)
;; Makes an input port factory that uses readline to read from the
;; terminal.
;;
;; install-readline!: ->
;; Installs a readline port as the repl console reader.  Currently this
;; makes the debugger look a little weird.
;;

(require 'std-ffi)
(require 'foreign-stdlib)

(find-foreign-file
  '("/usr/local/lib/" "/usr/lib/" "/lib/")
  "libreadline."
  '("so" "dylib")
  '("" ".5" ".4" ".3"))

;; readline() returns a malloc'ed string, so we can't just let the FFI
;; marshall it for us.  Rather, we get the char* back, marshall it
;; manually, and then free it before returning.  It's important to call
;; readline with a prompt rather than print the prompt and then call
;; readline, because readline needs to know the line position to do line
;; editing properly.
(define readline
  (letrec
    ((c-readline (foreign-procedure "readline" '(string) 'char*))
     (readline
       (case-lambda
         (() (readline ""))
         ((prompt)
          (let* ((char*   (c-readline prompt))
                 (result  (and char* (char*->string char*))))
            (if char* (stdlib/free char*))
            result)))))
    readline))

(define raw-add-history
  (foreign-procedure "add_history" '(string) 'void))

(define add-history
  (let ((last ""))
    (lambda (str)
      (cond
        ((zero? (string-length str)))
        ((string=? last str))
        (else
          (set! last str)
          (raw-add-history str))))))

;; For reading from the console, we use a console factory.  This lets us
;; produce a new port when the port has reached EOF, so that we can use
;; Ctrl-D to exit the debugger or a sub-repl and not have everything
;; give up and exit.  Thus, make-readline-console-factory returns a
;; readline-console-factory, which is a procedure that returns a port.
;; The port returned is always ready for input, but if multiple ports
;; are returned, they all _share_ their data, which seems to be
;; necessary for properly reading multiple data from one line.
(define make-readline-console-factory #f)
(letrec
  ((readline.s 1)  ;; Internal buffer
   (readline.i 2)  ;; Buffer offset
   (readline.p 3)  ;; Readline prompt
   (readline.c 4)  ;; Closed; reopen please

   (readline/ioproc
     (lambda (op)
       (case op
         ((read)   readline/fill-buffer)
         ((close)  (lambda (data)
                     (vector-set! data readline.c #t)))
         ((ready?) (lambda (data)
                     (not (vector-ref data readline.c))))
         ((name)   (lambda (data) "*readline*"))
         (else     (error "readline/ioproc: illegal operation: " op)))))

   (readline/fill-buffer
     (lambda (data buffer)
       (let ((s (vector-ref data readline.s))
             (i (vector-ref data readline.i))
             (p (vector-ref data readline.p)))
         (if (= i (string-length s))
           (let ((next (readline (p))))
             (p (make-string (string-length (p)) #\space))
             (cond
               (next
                 (add-history next)
                 (vector-set! data readline.s (string-append next "\n"))
                 (vector-set! data readline.i 0)
                 (readline/fill-buffer data buffer))
               (else
                 (vector-set! data readline.c #t)
                 'eof)))
           (let ((n (min (string-length buffer) (- (string-length s) i))))
             (do ((j 0 (+ j 1))
                  (k i (+ k 1)))
               ((= j n)
                (vector-set! data readline.i (+ i n))
                n)
               (string-set! buffer j (string-ref s k))))))))

   (*make-readline-console-factory*
     (case-lambda
       ((prompt)
        (let* ((data  (vector 'readline-input-port "" 0 prompt #t))
               (port  #f))
          (lambda ()
            (cond
              ((vector-ref data readline.c)
               (vector-set! data readline.c #f)
               (set! port (io/make-port readline/ioproc data 'input))))
            port)))
       (()
        (*make-readline-console-factory* (lambda any ""))))))

   (set! make-readline-console-factory *make-readline-console-factory*))

(define (install-readline!)
  (define readline-prompt
    (make-parameter "readline-prompt" "" string?))
  (repl-prompt
    (lambda (level port) ; port is ignored
      (newline)
      (readline-prompt
        (string-append (make-string level #\>) " "))))
  (console-input-port-factory
    (make-readline-console-factory readline-prompt))
  (begin))

