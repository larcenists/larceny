; data store -- remote content manager

; Given a host name, a port, a path to an object on that host, an
; indication of whether we are reading text or data, and an indication
; whether we require fresh content, return an open input-port to read
; that content.

(require 'socket)
(require 'io)

(define (open-content-stream host port object text? force-refresh?)
  (let* ((s    (make-client-socket host port))
	 (in   (apply socket-input-port s (if text? '(char) '())))
	 (out  (socket-output-port s 'char 'flush)))
    (display/crlf (string-append "GET " object) out)
    in))

(cond-expand
 (win32
  (define crlf (string #\newline)))
 (unix
  (define crlf (string (integer->char 13) (integer->char 10)))))

(define (display/crlf s out)
  (display s out)
  (display crlf out))

; Just a debugging tool

(define (treesize t)
  (cond ((string? t) (+ 6 (string-length t)))
	((null? t) 0)
	((pair? t) (+ 8 (treesize (car t)) (treesize (cdr t))))
	((symbol? t) 0)
	(else (display t) 0)))
