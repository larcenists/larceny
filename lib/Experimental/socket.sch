; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Interface to Unix socket system calls .
;
; This works for SunOS 5, don't know (yet) about SunOS 4.
;
; Linux support believed to be complete (even byte order issues)
; but not sure.  gethostbyname works, at least.

(require 'ffi)
(require 'experimental/unix)
(require 'common-syntax)

(define (server-socket port)
  (call-with-current-continuation
   (lambda (return)
     (let ((s (unix/socket unix/PF_INET unix/SOCK_STREAM unix/IPPROTO_TCP)))
       (when (= s -1)
         (unix/perror "socket")
         (return #f))
       (let ((addr (make-sockaddr_in)))
         (sockaddr_in.sin_family-set! addr unix/AF_INET)
         (sockaddr_in.sin_addr-set! addr (htonl (make-ip-addr 127 0 0 1)))
         (sockaddr_in.sin_port-set! addr (htons port))
         (set-socket-option:reuseaddr s)
         (when (= -1 (unix/bind s addr (bytevector-length addr)))
           (unix/perror "bind")
           (return #f))
         (when (= -1 (unix/listen s 5))
           (unix/perror "listen")
           (return #f))
         s)))))

(define (wait-for-connection-on-server-socket s . flags)
  (let ((addr         (make-sockaddr_in))
	(addrlen      (make-bytevector sizeof:int))
        (nonblocking? (memq 'nonblocking flags)))
    (sockaddr_in.sin_family-set! addr unix/AF_INET)
    (%set-int addrlen 0 (bytevector-length addr))
    (if (and nonblocking?
             (null? (poll-descriptors (list s) '() #f)))
        (input-not-ready-handler s))
    (let ((ns (unix/accept s addr addrlen)))
      (if (= ns -1)
	  (begin (unix/perror "accept")
		 (values #f #f))
	  (values ns addr)))))

; FIXME: We can support asynchronous connect by setting O_NONBLOCK
; before the connect and then doing the POLL thing afterwards if connect
; returns -1 and errno is EINPROGRESS, cf connect(3XN).

(define (client-socket host port . flags)
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
	  (sockaddr_in.sin_addr-set! addr (htonl ip-number))
	  (sockaddr_in.sin_port-set! addr (htons port))
	  (let ((r (unix/connect s addr (bytevector-length addr))))
	    (if (= r -1)
		(begin (unix/perror "connect")
		       #f)
		s))))))

(define (get-host-by-name hostname)
  (let ((ptr (unix/gethostbyname hostname))
	(buf (make-hostent)))
    (if (foreign-null-pointer? ptr)
        (let ((h_errno (get-h-errno)))
	  (display "gethostbyname: ")
          (display (strerror h_errno))
          (newline)
	  (values #f #f #f))
	(begin
	  (peek-bytes ptr buf (bytevector-length buf))
	  (values (hostent.h_addrtype buf)
		  (hostent.h_aliases buf)
		  (map reverse (hostent.h_addr_list buf)))))))

(define (get-service-by-name name . rest)
  (let ((ptr (unix/getservbyname name (if (null? rest) #f (car rest))))
	(buf (make-servent)))
    (if (foreign-null-pointer? ptr)
	(begin 
	  (display "get-service-by-name: ")
	  (display name)
	  (display " is not a known service.")
	  (newline)
	  (values #f #f))
	(begin
	  (peek-bytes ptr buf (bytevector-length buf))
	  (values (ntohs (servent.s_port buf))
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

(cond-expand
 (little-endian
  (define (htonl x) 
    (+ (* 16777216 (remainder x 256))
       (* 65536 (remainder (quotient x 256) 256))
       (* 256 (remainder (quotient x 65536) 256))
       (quotient x 16777216)))
  (define (htons x) 
    (+ (* 256 (remainder x 256))
       (quotient x 256)))
  (define (ntohl x) (htonl x))
  (define (ntohs x) (htons x)) )

 (big-endian
  (define (htonl x) x)
  (define (htons x) x)
  (define (ntohl x) x)
  (define (ntohs x) x) ))

;;; Libraries.

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

; From <sys/socket.h> and <bits/socket.h> and <asm/socket.h>

(define unix/AF_UNIX         1)
(define unix/AF_INET         2)

(define unix/PF_UNIX         1)		; Local
(define unix/PF_INET         2)		; TCP and UDP

(cond-expand
 (linux
  (define unix/SOCK_STREAM     1)
  (define unix/SOCK_DGRAM      2)

  (define unix/SOL_SOCKET      1)
  (define unix/SO_DEBUG        1)	; turn on debugging info recording
  (define unix/SO_REUSEADDR    2)	; keep connections alive
  (define unix/SO_TYPE         3)
  (define unix/SO_ERROR        4)
  (define unix/SO_DONTROUTE    5)
  (define unix/SO_BROADCAST    6)
  (define unix/SO_SNDBUF       7)
  (define unix/SO_RCVBUF       8)
  (define unix/SO_KEEPALIVE    9)
  (define unix/OOBINLINE       10)
  (define unix/SO_NO_CHECK     11)
  (define unix/SO_PRIORITY     12)
  (define unix/SO_LINGER       13)
  (define unix/SO_BSDCOMPAT    14) )

 (solaris
  (define unix/SOCK_DGRAM      1)	; Datagram socket
  (define unix/SOCK_STREAM     2)	; Stream socket

  (define unix/SO_DEBUG        #x0001)	; turn on debugging info recording
  (define unix/SO_REUSEADDR    #x0004)	; keep connections alive
  (define unix/SO_KEEPALIVE    #x0008)	; keep connections alive
  (define unix/SO_DONTROUTE    #x0010)	; just use interface addresses
  (define unix/SO_BROADCAST    #x0020)	; permit sending of broadcast msgs
  (define unix/SO_USELOOPBACK  #x0040)	; bypass hardware when possible
  (define unix/SO_LINGER       #x0080)	; linger on close if data present
  (define unix/SO_OOBINLINE    #x0100)	; leave received OOB data in line
  (define unix/SO_DGRAM_ERRIND #x0200)	; Application wants delayed error

  (define unix/SOL_SOCKET      #xffff) )) ; options for socket level

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

(define (sockaddr_in.sin_family x) (%get-ushort x 0))
(define (sockaddr_in.sin_port x)   (%get-ushort x 2))
(define (sockaddr_in.sin_addr x)   (%get-uint x 4))

(define (sockaddr_in.sin_family-set! x fam) (%set-ushort x 0 fam))
(define (sockaddr_in.sin_port-set! x port)  (%set-ushort x 2 port))
(define (sockaddr_in.sin_addr-set! x addr)  (%set-uint x 4 addr))

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
