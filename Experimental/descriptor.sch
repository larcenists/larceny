; A very crude hack to open ports on descriptors.
; (I can't believe I'm doing this.)
;
; USE AT YOUR OWN RISK.  
; Works with 1.0a1 and 0.42, at least.  Will be replaced by general ports.
;
; 19 April 1999 / lth
;
; For example:
;  (define stderr (open-output-descriptor 2))
;  (port/set-discretionary-flush-flag! stderr #t)

(define (open-input-descriptor fd)
  (let ((p (open-input-file "/dev/null")))
    (let ((port-datum (vector-like-ref p 2)))
      (set-car! port-datum fd)
      (set-cdr! port-datum (string-append "*descriptor "
                                          (number->string fd)
                                          "*"))
      p)))

(define (open-output-descriptor fd)
  (let ((p (open-output-file "/dev/null")))
    (let ((port-datum (vector-like-ref p 2)))
      (set-car! port-datum fd)
      (set-cdr! port-datum (string-append "*descriptor "
                                          (number->string fd)
                                          "*"))
      p)))

; _flag_ is a boolean: 
;  #t means flush following every output operation except write-char.
;  #f means don't.

(define (port/set-discretionary-flush-flag! output-port flag)
  (vector-like-set! output-port 9 flag))

; eof
