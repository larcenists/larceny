; Creates ports (blocking only, binary mode) on Winsock SOCKETs.

(require 'std-ffi)
(require "Experimental/winsock")

; Flags are:
;   char        -- char port (default)
;   byte        -- byte port
; Currently char and byte ports are indistinguishable.
(define (open-input-winsocket sock . flags)
  (let ((type (cond ((memq 'byte flags) 'byte)
                    ((memq 'char flags) 'char)
                    (else 'char))))
    (make-input-port
     (lambda (selector)
       (case selector
         ((read)   winsockio/read)
         ((close)  winsockio/close)
         ((name)   winsockio/name)
         ((ready)  (lambda anything #t))
         (else     ???)))
       sock
       type)))

; Flags are:
;   flush       -- use discretionary flushing
;   char        -- char port (default)
;   byte        -- byte port
; Currently char and byte ports are indistinguishable.
(define (open-output-winsocket sock . flags)
  (let ((type (cond ((memq 'byte flags) 'byte)
                    ((memq 'char flags) 'char)
                    (else 'char)))
        (flush? (memq 'flush flags)))
    (make-output-port
     (lambda (selector)
       (case selector
         ((write)  winsockio/write)
         ((close)  winsockio/close)
         ((name)   winsockio/name)
         (else     ???)))
       sock
       type
       flush?)))

(define (winsockio/read data buf)
  #f)

(define (winsockio/write data buf count)
  #f)

(define (winsockio/close data)
  (if (= (winsock/closesocket data)
         winsock/SOCKET_ERROR)
    'error
    'ok))

(define (winsockio/name data)
  (string-append "*winsocket " 
                 (number->string data)
                 "*"))
