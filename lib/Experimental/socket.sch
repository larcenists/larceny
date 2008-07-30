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

(require 'std-ffi)
(require 'common-syntax)
(require 'srfi-0)

(require 'foreign-ctools)
(require 'foreign-cstructs)

(define-syntax define-for-include-syntax
  (transformer
   (lambda (exp ren cmp)

     ;; this makes the binding available for eval calls during
     ;; expansion of define-c-info

     (eval (cons 'define (cdr exp)))

     (cons (ren 'define) (cdr exp)))))

(cond-expand
  (unix
    (require "Experimental/unix")
    (define-for-include-syntax socket/sys/socket.h "sys/socket.h")
    (define-for-include-syntax socket/netinet/in.h "netinet/in.h")
    (define-for-include-syntax socket/netdb.h      "netdb.h")

    (define (socket/invalid? s)
      (= s -1))
    (define (socket/error? s)
      (= s -1))

    (define socket/gethostbyname
      (foreign-procedure "gethostbyname" '(string) 'unsigned))

    (define socket/getservbyname
      (foreign-procedure "getservbyname" '(string string) 'unsigned))

    (define socket/accept      unix/accept)
    (define socket/bind        unix/bind)
    (define socket/connect     unix/connect)
    (define socket/listen      unix/listen)
    (define socket/setsockopt  unix/setsockopt)
    (define socket/socket      unix/socket)
    (define socket/perror      unix/perror)
    )
  (win32
    (require "Experimental/winsock")
    (define-for-include-syntax socket/sys/socket.h "Winsock2.h")
    (define-for-include-syntax socket/netinet/in.h "Winsock2.h")
    (define-for-include-syntax socket/netdb.h      "Winsock2.h")

    (define (socket/invalid? s)
      (= s winsock/INVALID_SOCKET))
    (define (socket/error? s)
      (= s winsock/SOCKET_ERROR))

    (define socket/gethostbyname
      (foreign-procedure "gethostbyname" '(string) 'unsigned 'stdcall))

    (define socket/getservbyname
      (foreign-procedure "getservbyname" '(string string) 'unsigned 'stdcall))

    (define socket/accept      winsock/accept)
    (define socket/bind        winsock/bind)
    (define socket/connect     winsock/connect)
    (define socket/listen      winsock/listen)
    (define socket/setsockopt  winsock/setsockopt)
    (define socket/socket      winsock/socket)
    (define socket/perror      winsock/error)
    ))

(define (server-socket port)
  (call-with-current-continuation
   (lambda (return)
     (let ((s (socket/socket socket/PF_INET socket/SOCK_STREAM
                              socket/IPPROTO_TCP)))
       (when (socket/invalid? s)
         (socket/perror "socket")
         (return #f))
       (let ((addr (make-sockaddr_in)))
         (sockaddr_in.sin_family-set! addr socket/AF_INET)
         (sockaddr_in.sin_addr-set! addr (htonl (make-ip-addr 127 0 0 1)))
         (sockaddr_in.sin_port-set! addr (htons port))
         (set-socket-option:reuseaddr s)
         (when (socket/error? (socket/bind s addr (bytevector-length addr)))
           (socket/perror "bind")
           (return #f))
         (when (socket/error? (socket/listen s 5))
           (socket/perror "listen")
           (return #f))
         s)))))

(define (wait-for-connection-on-server-socket s . flags)
  (let ((addr         (make-sockaddr_in))
	(addrlen      (make-bytevector sizeof:int))
        (nonblocking? (memq 'nonblocking flags)))
    (sockaddr_in.sin_family-set! addr socket/AF_INET)
    (%set-int addrlen 0 (bytevector-length addr))
    (if (and nonblocking?
             (null? (poll-descriptors (list s) '() #f)))
        (input-not-ready-handler s))
    (let ((ns (socket/accept s addr addrlen)))
      (if (socket/invalid? ns)
	  (begin (socket/perror "accept")
		 (values #f #f))
	  (values ns addr)))))

; FIXME: We can support asynchronous connect by setting O_NONBLOCK
; before the connect and then doing the POLL thing afterwards if connect
; returns error and errno is EINPROGRESS, cf connect(3XN).

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
	(s (socket/socket socket/PF_INET socket/SOCK_STREAM
                           socket/IPPROTO_TCP)))
    (if (socket/invalid? s)
	(begin (socket/perror "socket")
	       #f)
	(let ((addr (make-sockaddr_in)))
	  (sockaddr_in.sin_family-set! addr socket/AF_INET)
	  (sockaddr_in.sin_addr-set! addr (htonl ip-number))
	  (sockaddr_in.sin_port-set! addr (htons port))
	  (let ((r (socket/connect s addr (bytevector-length addr))))
	    (if (socket/error? r)
		(begin (socket/perror "connect")
		       #f)
		s))))))

(define (get-host-by-name hostname)
  (let ((ptr (socket/gethostbyname hostname))
	(buf (make-hostent)))
    (if (foreign-null-pointer? ptr)
        (begin
          (socket/perror "gethostbyname")
	  (values #f #f #f))
	(begin
	  (peek-bytes ptr buf (bytevector-length buf))
	  (values (hostent.h_addrtype buf)
		  (hostent.h_aliases buf)
		  (hostent.h_addr_list buf))))))

(define (get-service-by-name name . rest)
  (let ((ptr (socket/getservbyname name (if (null? rest) #f (car rest))))
	(buf (make-servent)))
    (if (foreign-null-pointer? ptr)
	(begin 
          (socket/perror "getservicebyname")
	  (values #f #f))
	(begin
	  (peek-bytes ptr buf (bytevector-length buf))
	  (values (ntohs (servent.s_port buf))
		  (servent.s_proto buf))))))

(define (set-socket-option:reuseaddr socket)
  (let ((one (make-bytevector sizeof:int)))
    (%set-int one 0 1)
    (let ((r (socket/setsockopt socket socket/SOL_SOCKET socket/SO_REUSEADDR
                                one sizeof:int)))
      (if (socket/error? r)
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


;;; Constants

; From /etc/services

(define inet.echo/tcp 7)
(define inet.ftp/tcp 20)
(define inet.telnet/tcp 23)
(define inet.smtp/tcp 25)
(define inet.finger/tcp 79)


(define-c-info (include<> socket/sys/socket.h)
  (const socket/AF_UNIX      int "AF_UNIX")
  (const socket/AF_INET      int "AF_INET")
  ;(const socket/PF_UNIX      int "PF_UNIX") ; Local
  (const socket/PF_INET      int "PF_INET") ; TCP and UDP
  (const socket/SOCK_STREAM  int "SOCK_STREAM") ; Stream socket
  (const socket/SOCK_DGRAM   int "SOCK_DGRAM")  ; Datagram socket
  (const socket/SOL_SOCKET   int "SOL_SOCKET")  ; options for socket level
  (const socket/SO_DEBUG     int "SO_DEBUG") ; turn on debugging info recording
  (const socket/SO_REUSEADDR int "SO_REUSEADDR") ; keep connections alive
  (const socket/SO_TYPE      int "SO_TYPE")
  (const socket/SO_ERROR     int "SO_ERROR")
  (const socket/SO_DONTROUTE int "SO_DONTROUTE") ; just use interface addresses
  (const socket/SO_BROADCAST int "SO_BROADCAST") ; permit sending of broadcast msgs
  ;(const socket/SO_USELOOPBACK int "SO_USELOOPBACK") ; bypass hardware when possible
  (const socket/SO_SNDBUF    int "SO_SNDBUF")    ; send buffer size
  (const socket/SO_RCVBUF    int "SO_SNDBUF")    ; receive buffer size
  (const socket/SO_KEEPALIVE int "SO_KEEPALIVE") ; keep connections alive
  ;(const socket/OOBINLINE    int "SS_OOBINLINE") ; leave received OOB data in line

  ;(define socket/SO_NO_CHECK     11)
  ;(define socket/SO_PRIORITY     12)
  
  (const socket/SO_LINGER    int "SO_LINGER") ; linger on close if data present (in seconds)
  
  ;(define socket/SO_BSDCOMPAT    14) 

  )

(define-c-info (include<> socket/netinet/in.h)
  (const socket/IPPROTO_TCP int "IPPROTO_TCP")
  (const socket/IPPROTO_UDP int "IPPROTO_UDP"))

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

(define (%get-string object offset)
  (%peek-string (%get-pointer object offset)))

(define-c-offset-based-struct 
  ("struct sockaddr_in" make-sockaddr_in
                        (include<> socket/netinet/in.h))
  ("sin_family"
    (sockaddr_in.sin_family         %get-ushort)
    (sockaddr_in.sin_family-set!    %set-ushort))
  ("sin_port"
    (sockaddr_in.sin_port           %get-ushort)
    (sockaddr_in.sin_port-set!      %set-ushort))
  ("sin_addr"
    (sockaddr_in.sin_addr           %get-uint)
    (sockaddr_in.sin_addr-set!      %set-uint)))


(define-c-offset-based-struct 
  ("struct hostent" make-hostent
                    (include<> socket/netdb.h))
  ("h_name"
    (hostent.h_name %get-string))
  ("h_aliases"
    (hostent.h_aliases
      (lambda (x off)
        (%peek-pointer-array (%get-pointer x off) %peek-string))))
  ("h_addrtype"
    (hostent.h_addrtype %get-int))
  ("h_addr_list"
    (hostent.h_addr_list
      (lambda (x off)
        (%peek-pointer-array (%get-pointer x off)
                             (lambda (addr)
                               ;; 4 is the size of an AF_INET address
                               (let ((x (make-bytevector 4)))
                                 (peek-bytes addr x 4)
                                 (bytevector->list x))))))))


(define-c-offset-based-struct 
  ("struct servent" make-servent
                    (include<> socket/netdb.h))
  ("s_name"
    (servent.s_name %get-string))
  ("s_aliases"
    (servent.s_aliases
      (lambda (x off)
        (%peek-pointer-array (%get-pointer x off) %peek-string))))
  ("s_port"
    (servent.s_port %get-int))
  ("s_proto"
    (servent.s_proto %get-string)))

; eof
