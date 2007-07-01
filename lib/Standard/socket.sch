; Somewhat (though not entirely) portable, reasonable socket abstraction
; 2004-02-04 / lth

; SOCKET-REPRESENTATION? obj => boolean
;   Test whether an object is of the opaque type SOCKET-REPRESENTATION
;
; MAKE-SERVER-SOCKET port [flag ...] => socket-representation
;
;   Attempt to open a server socket on the given PORT on the local 
;   machine and return its representation if successful.  The FLAGS
;   control what kind of socket that is created.  Current flags are:
;
;      NONBLOCKING  -  do not block Larceny when waiting for connections,
;                      requires that the tasking package is active
;
; SERVER-SOCKET-ACCEPT socket-representation => [socket-representation ip-addr]
;
;   Wait for connections on the server socket, returning a new socket
;   and the IP address of the remote host when a connection is made.
;
; MAKE-CLIENT-SOCKET host port [flag ...] => socket-representation
;
;   Connect to the given PORT on the given HOST and return a socket
;   for the connection.  HOST can be a string, a number representing
;   an IP-address, or a 4-tuple list of bytes representing an IP-address.
;   FLAGS control how the connection is made; currently no flags are
;   supported and connecting is always blocking.  FIXME.
;
; SOCKET-INPUT-PORT socket-representation [flag ...] => input-port
;
;   Return an input port for reading from the socket.  The type of
;   port is controlled by the FLAGS.  Flags are:
;
;      CHAR         -  character port rather than binary port
;      NONBLOCKING  -  do not block Larceny if I/O cannot be completed,
;                      requires that the tasking package is active
;
; SOCKET-OUTPUT-PORT socket-representation [flag ...] => output-port
;
;   Return an output port for writing to the socket.  The type of
;   port is controlled by the FLAGS.  Flags are:
;
;      CHAR         -  character port rather than binary port
;      NONBLOCKING  -  do not block Larceny if I/O cannot be completed,
;                      requires that the tasking package is active
;      FLUSH        -  flush the output port after every write operation
;
; GET-HOST-BY-NAME hostname => [address-type names addresses]
;
;   HOSTNAME is a string.  It is looked up through some name service
;   mechanism and three values are returned: an address type, a
;   list of names for the host, and a list of IP addresses represented 
;   as lists of four bytes in host order.  If the host cannot be found
;   then #f is returned for all values.
;
; GET-SERVICE-BY-NAME service => [port protocol]
;
;   SERVICE is a string.  It is looked up in some service database
;   and two values are returned: a standard port number for the service
;   and the name of the name of the protocol to use for the port,
;   normally either "tcp" or "udp".  If the service is not known, #f
;   is returned for both values.

; FIXME: there must be a way of closing a socket?  Or is it sufficient
; to close the ports?  Then what happens if multiple ports are created
; on the same socket?
;
; FIXME: error reporting?

(require 'srfi-0)
(require 'define-record)

(cond-expand
 (unix

  ; GET-HOST-BY-NAME and GET-SERVICE-BY-NAME are supplied by
  ; experimental/socket

  (require "Experimental/socket")
  (require 'unix-descriptor)

  (define-record socket-representation (fd flags in out))

  (define (make-server-socket port . flags)
    (let ((fd (server-socket port)))
      (make-socket-representation fd flags #f #f)))

  (define (server-socket-accept s) 
    (let-values (((ns addr)
		  (apply wait-for-connection-on-server-socket 
			 (socket-representation-fd s)
			 (if (memq 'nonblocking 
				   (socket-representation-flags s))
			     (error 'server-socket-accept ": nonblocking ports support disabled.")
			     '()))))
	(values (make-socket-representation ns '() #f #f) 
		(parse-ip-addr (ntohl (sockaddr_in.sin_addr addr))))))

  (define (make-client-socket host port . flags)
    (let ((fd (client-socket host port)))
      (make-socket-representation fd flags #f #f)))

  (define (socket-input-port s) 
    (or (socket-representation-in s)
	(let ((p (open-input-descriptor 
                  (socket-representation-fd s))))
	  (socket-representation-in-set! s p)
	  p)))

  (define (socket-output-port s)
    (or (socket-representation-out s)
	(let ((p (open-output-descriptor 
                  (socket-representation-fd s))))
	  (socket-representation-out-set! s p)
	  p)))
  ))
