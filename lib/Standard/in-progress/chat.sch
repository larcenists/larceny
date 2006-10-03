; Fairly generic chat code, but Larceny-specific.  
; 2000-06-02 / lth

; To do:
;  Document it
;  Hide the internals
;  Be less Larceny-specific by moving the descriptor-related cruft 
;    out of this package -- the chat code is really more general
;  Use exceptions to signal errors as much as possible -- requires lower-
;    level support.

; Export these:
(define make-chat-connection)   ; input-port output-port -> conn
(define make-chat-connection)   ; input-port output-port log-port -> conn
(define chat-noanswer)          ; conn string ... -> void
(define chat-answer)            ; conn string ... -> (string ...)
(define chat-put-answer)        ; conn (string ...) -> void
(define chat-read-answer)       ; conn -> (string ...)
(define char-receive-data)      ; conn -> (string ...)
(define chat-send-data)         ; conn (string ...) -> (string ...)

(let ((chat-rtd (make-record-type "chat-connection" '(in out log))))
    
  (define <CR>  (integer->char 13))
  (define <LF>  (integer->char 10))
  (define <CRLF> (list->string (list <CR> <LF>)))

  (define new-chat-connection (record-constructor chat-rtd))
  (define chat-connection-in (record-accessor chat-rtd 'in))
  (define chat-connection-out (record-accessor chat-rtd 'out))
  (define chat-connection-log (record-accessor chat-rtd 'log))

  (define (chat-noanswer conn . msg)
    (let ((s (call-with-output-string
              (lambda (os)
                (for-each (lambda (x) (display x os)) msg)))))
      (write-crlf-line conn s)))

  (define (chat-answer conn . msg)
    (apply chat-noanswer conn msg)
    (chat-read-answer conn))

  (define (chat-read-answer conn)

    (define (more-lines? l)
      (let ((len (string-length l)))
        (let loop ((i 0))
          (cond ((and (< i len) (< i 4))
                 (cond ((char-numeric? (string-ref l i))
                        (loop (+ i 1)))
                       ((char=? (string-ref l i) #\-)
                        #t)
                       (else
                        #f)))
                (else
                 #f)))))                ; Really an error

    (define (parse-line l)
      (let ((len (string-length l)))
        (let loop ((i 0))
          (cond ((and (< i len) (< i 4))
                 (cond ((char-numeric? (string-ref l i))
                        (loop (+ i 1)))
                       ((or (char=? (string-ref l i) #\-)
                            (char=? (string-ref l i) #\space))
                        (cons (string->number (substring l 0 i))
                              (substring l (+ i 1) len)))))
                ((< len 3)
                 (cons (string->number l) ""))
                (else
                 (cons 0 l))))))        ; Really an error

    (let loop ((l (read-crlf-line conn)) (ans '()))
      (cond ((eof-object? l)
             (error "Connection interrupted by EOF."))
            ((not (more-lines? l))
             (reverse (cons (parse-line l) ans)))
            (else
             (loop (read-crlf-line conn) 
                   (cons (parse-line l) ans))))))

  (define (chat-receive-data conn)
    (let loop ((l '()))
      (let ((s (read-crlf-line conn)))
        (cond ((string=? s ".")
               (reverse l))
              ((string=? s "..")
               (loop (cons "." l)))
              (else
               (loop (cons s l)))))))

  ; There should be both 7-bit and 8-bit versions of this.

  (define (chat-send-data conn strings)
    (for-each (lambda (l)
                (if (string=? l ".")
                    (write-crlf-line conn "..")
                    (write-crlf-line conn l)))
              strings)
    (write-crlf-line conn ".")
    (chat-read-answer conn))

  (define (write-crlf-line conn line)
    (let ((out (chat-connection-out conn))
          (log (chat-connection-log conn)))
      (display line out)
      (display <CRLF> out)
      (when log
        (display ">>> " log)
        (display line log)
        (newline log))))

  (define (read-crlf-line conn)
    (let ((in (chat-connection-in conn))
          (log (chat-connection-log conn)))
      (let loop ((l '()))
        (let ((c (read-char in)))
          (cond ((eof-object? c) c)
                ((char=? c <CR>)
                 (let ((d (read-char in)))
                   (cond ((eof-object? d) d)
                         ((char=? d <LF>)
                          (let ((line (list->string (reverse l))))
                            (when log
                              (display "<<< " log)
                              (display line log)
                              (newline log))
                            line))
                         (else
                          (loop (cons d (cons c l)))))))
                (else
                 (loop (cons c l))))))))

  (define (call-with-output-string p)
    (let ((os (open-output-string)))
      (p os)
      (get-output-string os)))

  (set! ... ...)
  ...
  'chat)

; eof
