; $Id$
; Tests output procedures.
; FIXME: Not yet comprehensive.

(define (print-test)
  (write-char #\@)
  (write-char #\newline)
  (write "Hello, world.") (newline)              ; string
  (write 'hello) (newline)                       ; symbol
  (write '()) (newline)                          ; null
  (write #t) (newline)                           ; bool
  (write #f) (newline)                           ; ditto
  (write '(hello world)) (newline)               ; proper list
  (write '(hello . world)) (newline)             ; pair
  (write '#(hello world)) (newline)              ; vector
  (write (make-bytevector 5)) (newline)          ; bytevector
  (write (make-vector 10 #t)) (newline)          ; vector again
  (write (current-output-port)) (newline)        ; port
  (write print-test) (newline)                   ; procedure
  (write 4) (newline)                            ; single-digit fixnum
  (write 37) (newline)                           ; multi-digit fixnum

  )

; eof
