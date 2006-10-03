;;; md5.scm  --  Jens Axel Søgaard, 16 oct 2002  

; tests for lib/md5.sch
; 2003-02-16 / lth

(define (fail token . more)
  (display "Error: test failed: ")
  (display token)
  (newline)
  #f)

(or (equal? (md5 "") "d41d8cd98f00b204e9800998ecf8427e")
    (fail 'md5:1))

(or (equal? (md5 "a") "0cc175b9c0f1b6a831c399e269772661")
    (fail 'md5:2))

(or (equal? (md5 "abc") "900150983cd24fb0d6963f7d28e17f72")
    (fail 'md5:3))

(or (equal? (md5 "message digest") "f96b697d7cb7938d525a2f31aaf161d0")
    (fail 'md5:4))

(or (equal? (md5 "abcdefghijklmnopqrstuvwxyz") "c3fcd3d76192e4007dfb496cca67e13b")
    (fail 'md5:5))

(or (equal? (md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
	    "d174ab98d277d9f5a5611c2c9f419d9f")
    (fail 'md5:6))

(or (equal? (md5 "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
	    "57edf4a22be3c955ac49da2e2107b67a")
    (fail 'md5:7))

(display "Done.")
(newline)
