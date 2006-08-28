; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Some SMTP test code for sockets.

(require 'experimental/unix)
(require 'experimental/socket)
(require 'experimental/unix-descriptor)

; Example

(define mailhost "mailhost")
(define localhost "canopus")

; Sender is a string: username@domain
; Recipients is a list of strings: mail addresses
; Message is a list of strings: the message to send.

(define (send-mail-by-smtp sender recipients message)
  (call-with-current-continuation
   (lambda (return)
     (let-values (((conn response) (open-smtp-connection mailhost)))

       (define (finish reply value)
         (if reply (display-chat-response reply))
         (smtp.quit conn)
         (close-smtp-connection conn)
         (return value))

       (define (display-chat-response r . rest)
         (let ((out (if (null? rest) (current-output-port) (car rest))))
           (for-each (lambda (l)
                       (display (car l) out)
                       (display " " out)
                       (display (cdr l) out)
                       (newline out))
                     r)))

       (define delivered-to '())
       (define not-delivered-to '())

       (chat-connection-log-set! conn (current-output-port))
       (letrec-syntax ((response-case
                        (syntax-rules (else)
                          ((_ "internal" r v)
                           (finish r #f))
                          ((_ "internal" r v (else body ...))
                           (begin #t body ...))
                          ((_ "internal" r v (consts body ...) clause ...)
                           (if (memv v (quote consts))
                               (begin #t body ...)
                               (response-case "internal" r v clause ...)))
                          ((_ (var expr) clause ...)
                           (let* ((var expr)
                                  (v (caar var)))
                             (response-case "internal" var v clause ...))))))
         (response-case (reply (smtp.helo conn localhost))
           ((250)))                     ; OK
         (response-case (reply (smtp.mail conn sender))
           ((250)))                     ; OK
         (for-each (lambda (recipient)
                     (response-case (reply (smtp.rcpt conn recipient))
                       ((250 251)     ; OK; Will forward
                        (set! delivered-to (cons recipient delivered-to)))
                       ((551)         ; User not local, won't forward
                        (set! not-delivered-to
                              (cons recipient not-delivered-to)))))
                   recipients)
         (when (null? delivered-to)
           (display "Not delivered to: ")
           (display not-delivered-to)
           (newline)
           (finish #f #f))
         (response-case (reply (smtp.data conn))
           ((354)))                     ; Start data input
         (response-case (reply (chat-send-data conn message))
           ((250)))                     ; OK
         (finish #f #t))))))


; SMTP primitives

(define (open-smtp-connection host)
  (let ((conn (open-chat-client host "smtp" "tcp")))
    (values conn 
            (chat-read-answer conn))))

(define (close-smtp-connection conn)
  (close-chat-client conn))

(define (smtp.help conn)
  (chat-answer conn "HELP"))

(define (smtp.quit conn)
  (chat-answer conn "QUIT"))

(define (smtp.helo conn hostname)
  (chat-answer conn "HELO " hostname))

(define (smtp.mail conn sender)
  (chat-answer conn "MAIL FROM:" sender))

(define (smtp.rcpt conn recipient)
  (chat-answer conn "RCPT TO:" recipient))

(define (smtp.data conn)
  (chat-answer conn "DATA"))


; Fairly generic chat code

(define <CR> (integer->char 13))
(define <LF> (integer->char 10))
(define <CRLF> (list->string (list <CR> <LF>)))

(define-record chat-connection (fd in out log))

(define (open-chat-client host service proto)
  (let-values (((port proto) (get-service-by-name service proto)))
    (let* ((client (client-socket host port))
           (chat-in (open-input-descriptor client))
           (chat-out (open-output-descriptor client 'flush)))
      (make-chat-connection client chat-in chat-out #f))))

(define (close-chat-client conn)
  (unix/close (chat-connection-fd conn)))

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
               #f)))))                  ; Really an error

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
               (cons 0 l))))))          ; Really an error

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

; eof
