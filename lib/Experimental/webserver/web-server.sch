; No Scheme implementation is complete without a web server!!
;
; This one starts a server on a designated port and then runs a repl
; at the same time, thus demonstrating that non-blocking I/O works and
; allowing modification of the running server.
;
; The protocol is roughly HTTP 0.9 -- the returned pages contain no
; headers, just raw data.  The server only handles GETs.
;
; WEB-SERVER port-number => unspecified
;   Start the web server on the port and enter a nested REPL
;   in the tasking context.
;
; REGISTER-CGI absolute-path procedure => unspecified
;   Register the procedure as a CGI script for the given absolute
;   path.  The procedure accepts the query string (the URL after
;   the '?') and an output port.  The script will only be triggered
;   if the URL contains a '?'.
;
;
; Bugs to fix:
;  - Translate references to a directory name without a trailing /
;    into a reference to the index.html file in that directory.
;  - There is feeble exception handling in HTTP-GET: would be
;    better to use the SRFI exception handling and I/O conditions.
;  - Something wrong with the running of the CGI script 'eval'
;    before running 'echo': eval is fine after echo, but not before.

(require 'io)
(require 'tasking-with-io)
(require 'define-record)
(require 'string)
(require 'socket)

(define *www-root*    "/home/lth/www")	; base path for resolving names
(define *www-default* "index.html")	; name to use if request ends in / 
(define *www-cgi*     '())		; assoc list of (path . (lambda (query out) ...))
(define *www-log*     #t)		; #f or #t

(define (web-server port)
  (with-tasking
   (lambda ()
     (spawn (http-server-thread port))
     (repl)
     (end-tasking))))

(define (http-server-thread port)
  (lambda ()
    (let ((s (make-server-socket port 'nonblocking)))
      (let accept-loop ()
        (let-values (((ns addr) (server-socket-accept s)))
          (spawn (http-service-thread ns addr)))
        (accept-loop)))))

(define (http-service-thread ns addr)
  (lambda ()
    (let ((in  (socket-input-port ns 'nonblocking 'char))
          (out (socket-output-port ns 'nonblocking 'char 'flush)))
      (let ((request (read-line in)))
	(if (not (eof-object? request))
	    (begin 
	      (if *www-log*
		  (format #t "~a: ~a~%" addr request))
	      (let ((req (string-split request (lambda (x)
						 (not (char-whitespace? x))))))
		(if (and (not (null? req))
			 (string-ci=? (car req) "GET"))
		    (http-get req in out)
		    (display/crlf (string-append "Unrecognized request " request)
				  out)))))
	(close-input-port in)
	(close-output-port out)))))

(define (http-get tokens in out)
  (let* ((path				; the user's requested path
	  (cadr tokens))
	 (qpos				; leftmost position of ?, or #f
	  (string-index path #\?))
	 (qstring			; the stuff to the right of ?
	  (if (not qpos) "" (substring path (+ qpos 1) (string-length path))))
	 (clean-path			; user's path without ?...
	  (if (not qpos) path (substring path 0 qpos)))
	 (path				; user's path with index.html if necessary
	  (if (and (not qpos)
		   (char=? #\/ (string-ref path (- (string-length path) 1))))
	      (string-append path *www-default*)
	      path))
	 (fn				; complete file name
	  (string-append *www-root* path)))
    (if (not (call-with-current-continuation
	      (lambda (k)
		(parameterize ((error-handler (lambda e (k #f))))
		  (cond ((assoc clean-path *www-cgi*)
			 => (lambda (probe)
			      ((cdr probe) qstring out)))
			(else
			 (call-with-input-file fn
			   (lambda (in)
			     (do ((l (read-line in) (read-line in)))
				 ((eof-object? l))
			       (display/crlf l out)))))))
		#t)))
	(display/crlf (string-append "Request for " path " failed!") out))))

(define (register-cgi path fn)
  (without-interrupts
   (let ((probe (assoc path *www-cgi*)))
     (if probe
	 (set! *www-cgi* (remq probe *www-cgi*))))
   (set! *www-cgi* (cons (cons path fn) *www-cgi*))))

(cond-expand
 (win32
  (define crlf (string #\newline)))
 (unix
  (define crlf (string (integer->char 13) (integer->char 10)))))

(define (display/crlf s out)
  (display s out)
  (display crlf out))


