; $Id: $
;
; Interface to Windows winsock API -- should be sufficient to support
; lib/Experimental/socket.sch on Win32.

(require 'std-ffi)
(require 'srfi-0)

(require 'foreign-ctools)

(cond-expand
 (win32
  ;; http://msdn2.microsoft.com/en-us/library/ms737526.aspx says this is
  ;; the right library to load:
  (foreign-file "Ws2_32.dll")))

(define-c-info (include<> "Winsock2.h")
  (sizeof winsock/size:WSADATA  int "WSADATA")
  (const winsock/INVALID_SOCKET int "INVALID_SOCKET")
  (const winsock/SOCKET_ERROR   int "SOCKET_ERROR")
  (const winsock/SD_RECEIVE     int "SD_RECEIVE")
  (const winsock/SD_SEND        int "SD_SEND")
  (const winsock/SD_BOTH        int "SD_BOTH")
  )

(define winsock/startup (foreign-procedure "WSAStartup" '(boxed boxed) 'int
                          'stdcall))

(define winsock/socket  (foreign-procedure "socket" '(int int int) 'int
                          'stdcall))

(define winsock/bind    (foreign-procedure "bind" '(int boxed int) 'int
                          'stdcall))

(define winsock/listen  (foreign-procedure "listen" '(int int) 'int
                          'stdcall))

(define winsock/accept  (foreign-procedure "accept" '(int boxed boxed) 'int
                          'stdcall))

(define winsock/connect (foreign-procedure "connect" '(int boxed int) 'int
                          'stdcall))

(define winsock/recv    (foreign-procedure "recv" '(int boxed int int) 'int
                          'stdcall))

(define winsock/send    (foreign-procedure "send" '(int boxed int int) 'int
                          'stdcall))

(define winsock/shutdown
  (foreign-procedure "shutdown" '(int int) 'int 'stdcall))

(define winsock/closesocket
  (foreign-procedure "closesocket" '(int) 'int 'stdcall))

(define winsock/setsockopt
  (foreign-procedure "setsockopt" '(int int int boxed int) 'int 'stdcall))

(define winsock/get-last-error
  (foreign-procedure "WSAGetLastError" '() 'int 'stdcall))

;; XXX current-error-port is not yet implemented.
;; Also this sucks.
(define (winsock/error str)
  (display str)
  (display ": winsock error ")
  (display (winsock/get-last-error))
  (newline))

;; Winsock needs to be STARTED:
(let ((version '#vu8(2 2))
      (data    (make-bytevector winsock/size:WSADATA)))
  (if (= winsock/INVALID_SOCKET
         (winsock/startup version data))
     (winsock/error "WSAStartup")))

