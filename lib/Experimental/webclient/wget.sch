(require 'socket)
(require 'io)

(define (wget path host . rest)
  (let* ((port (if (null? rest) 80 (car rest)))
	 (s    (make-client-socket host 80))
	 (in   (socket-input-port s 'char))
	 (out  (socket-output-port s 'char 'flush)))
    (display/crlf (string-append "GET " path) out)
    (let ((cs (slurp in)))
      (close-input-port in)
      (close-output-port out)
      cs)))

(cond-expand
 (win32
  (define crlf (string #\newline)))
 (unix
  (define crlf (string (integer->char 13) (integer->char 10)))))

(define (display/crlf s out)
  (display s out)
  (display crlf out))
