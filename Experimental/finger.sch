;;; Some sample code that uses the socket stuff.

; 'user' is a string.
; 'host' is a string (hostname), list (IP number), or exact int (IP number)

(require 'experimental/unix)
(require 'experimental/socket)

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

