; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Interface to Unix socket system calls 
; This works for SunOS 5, don't know (yet) about SunOS 4.
;
; You must first load Experimental/unix.sch (for the system calls).

;;; Some sample code that uses the socket stuff.

; 'user' is a string.
; 'host' is a string (hostname), list (IP number), or exact int (IP number)

(define (finger user host)
  (let ((s (client-socket host inet.finger/tcp))
	(nl "\n"))
    (unix/write s user (string-length user))
    (unix/write s nl (string-length nl))
    (let ((buf (make-bytevector 1024)))
      (do ((n (unix/read s buf 1024) (unix/read s buf 1024)))
	  ((<= n 0))
	(do ((i 0 (+ i 1)))
	    ((= i n))
	  (write-char (integer->char (bytevector-ref buf i))))))
    (unix/close s)))

; FIXME: ignoring byte order because network byte order is big-endian,
; like the SPARC.

(define (server-socket port)
  (let ((s (unix/socket unix/PF_INET unix/SOCK_STREAM unix/IPPROTO_TCP)))
    (if (= s -1)
	(begin (unix/perror "socket")
	       #f)
	(let ((addr (make-sockaddr_in)))
	  (sockaddr_in.sin_family-set! addr unix/AF_INET)
	  (sockaddr_in.sin_addr-set! addr (make-ip-addr 127 0 0 1))
	  (sockaddr_in.sin_port-set! addr port)
	  (set-socket-option:reuseaddr s)
	  (let ((r (unix/bind s addr (bytevector-length addr))))
	    (if (= r -1)
		(begin (unix/perror "bind")
		       #f)
		(let ((r (unix/listen s 5)))
		  (if (= r -1)
		      (begin (unix/perror "listen")
			     #f)
		      s))))))))

(define (wait-for-connection-on-server-socket s)
  (let ((addr    (make-sockaddr_in))
	(addrlen (make-bytevector sizeof:int)))
    (sockaddr_in.sin_family-set! addr unix/AF_INET)
    (%set-int addrlen 0 (bytevector-length addr))
    (let ((ns (unix/accept s addr addrlen)))
      (if (= ns -1)
	  (begin (unix/perror "accept")
		 (values #f #f))
	  (values ns addr)))))

(define (client-socket host port)
  (let ((ip-number
	 (cond ((list? host)
		(apply make-ip-addr host))
	       ((and (integer? host) (exact? host))
		host)
	       (else
		(call-with-values
		 (lambda () (get-host-by-name host))
		 (lambda (addrtype aliases addresses)
		   (if addresses
		       (apply make-ip-addr (car addresses))
		       (error "No such host: " host)))))))
	(s (unix/socket unix/PF_INET unix/SOCK_STREAM unix/IPPROTO_TCP)))
    (if (= s -1)
	(begin (unix/perror "socket")
	       #f)
	(let ((addr (make-sockaddr_in)))
	  (sockaddr_in.sin_family-set! addr unix/AF_INET)
	  (sockaddr_in.sin_addr-set! addr ip-number)
	  (sockaddr_in.sin_port-set! addr port)
	  (let ((r (unix/connect s addr (bytevector-length addr))))
	    (if (= r -1)
		(begin (unix/perror "connect")
		       #f)
		s))))))

(define (get-host-by-name hostname)
  (let ((ptr (unix/gethostbyname hostname))
	(buf (make-hostent)))
    (if (ffi/null-pointer? ptr)
	(begin
	  ; The error code is in h_errno, which means perror won't do.
	  ; Will need to fetch h_errno and call strerror.  FIXME.
	  (display "gethostbyname failed.") (newline)
	  (values #f #f #f))
	(begin
	  (peek-bytes ptr buf (bytevector-length buf))
	  (values (hostent.h_addrtype buf)
		  (hostent.h_aliases buf)
		  (hostent.h_addr_list buf))))))

(define (get-service-by-name name . rest)
  (let ((ptr (unix/getservbyname name (if (null? rest) #f (car rest))))
	(buf (make-servent)))
    (if (ffi/null-pointer? ptr)
	(begin 
	  (display "get-service-by-name: ")
	  (display name)
	  (display " is not a known service.")
	  (newline)
	  (values #f #f))
	(begin
	  (peek-bytes ptr buf (bytevector-length buf))
	  (values (servent.s_port buf)
		  (servent.s_proto buf))))))

(define (set-socket-option:reuseaddr socket)
  (let ((one (make-bytevector sizeof:int)))
    (%set-int one 0 1)
    (let ((r (unix/setsockopt socket unix/SOL_SOCKET unix/SO_REUSEADDR one
			      sizeof:int)))
      (if (= r -1)
	  (begin (perror "setsockopt")
		 #f)
	  #t))))


;;; Libraries.

; I haven't figured out how to make this work (yet).
;(foreign-file "./Experimental/socket-support.so")

; struct hostent *gethostbyname( const char *host )
; struct servent *getservbyname( const char *service, const char *proto )

(define unix/gethostbyname
  (foreign-procedure "gethostbyname" '(string) 'unsigned))
(define unix/getservbyname
  (foreign-procedure "getservbyname" '(string string) 'unsigned))

;;; Constants

; From /etc/services

(define inet.echo/tcp 7)
(define inet.ftp/tcp 20)
(define inet.telnet/tcp 23)
(define inet.smtp/tcp 25)
(define inet.finger/tcp 79)

; From <sys/socket.h>

(define unix/AF_UNIX         1)
(define unix/AF_INET         2)

(define unix/PF_UNIX         1)		; Local
(define unix/PF_INET         2)		; TCP and UDP

(define unix/SOCK_DGRAM      1)		; Datagram socket
(define unix/SOCK_STREAM     2)		; Stream socket

(define	unix/SO_DEBUG        #x0001)	; turn on debugging info recording
(define	unix/SO_REUSEADDR    #x0004)	; keep connections alive
(define	unix/SO_KEEPALIVE    #x0008)	; keep connections alive
(define unix/SO_DONTROUTE    #x0010)	; just use interface addresses
(define	unix/SO_BROADCAST    #x0020)	; permit sending of broadcast msgs
(define	unix/SO_USELOOPBACK  #x0040)	; bypass hardware when possible
(define	unix/SO_LINGER       #x0080)	; linger on close if data present
(define	unix/SO_OOBINLINE    #x0100)	; leave received OOB data in line
(define	unix/SO_DGRAM_ERRIND #x0200)	; Application wants delayed error

(define	unix/SOL_SOCKET	     #xffff)	; options for socket level

; From <netinet/in.h>

(define unix/IPPROTO_TCP 6)		; TCP
(define unix/IPPROTO_UDP 17)		; UDP

;;; Socket support code

(define (make-ip-addr a b c d)
  (+ (* a 16777216)
     (* b 65536)
     (* c 256)
     d))

(define (parse-ip-addr n)
  (list (quotient n 16777216)
	(remainder (quotient n 65536) 256)
	(remainder (quotient n 256) 256)
	(remainder n 256)))

; From <netinet/in.h>

(define (make-sockaddr_in)
  (make-bytevector 16))

(define (sockaddr_in.sin_family x) (%get-short x 0))
(define (sockaddr_in.sin_port x)   (%get-short x 2))
(define (sockaddr_in.sin_addr x)   (%get-int x 4))

(define (sockaddr_in.sin_family-set! x fam) (%set-short x 0 fam))
(define (sockaddr_in.sin_port-set! x port)  (%set-short x 2 port))
(define (sockaddr_in.sin_addr-set! x addr)  (%set-int x 4 addr))

; From <netdb.h>

(define (make-hostent)
  (make-bytevector 20))

(define (hostent.h_name x) (%peek-string (%get-pointer x 0)))
(define (hostent.h_aliases x)
  (%peek-pointer-array (%get-pointer x 4) %peek-string))
(define (hostent.h_addrtype x) (%get-int x 8))
(define (hostent.h_addr_list x)
  (map parse-ip-addr
       (%peek-pointer-array (%get-pointer x 16) %peek-pointer)))

; From <netdb.h>

(define (make-servent)
  (make-bytevector 16))

(define (servent.s_name x) (%peek-string (%get-pointer x 0)))
(define (servent.s_aliases x)
  (%peek-pointer-array (%get-pointer x 4) %peek-string))
(define (servent.s_port x) (%get-int x 8))
(define (servent.s_proto x) (%peek-string (%get-pointer x 12)))

; eof
