; Copyright 1999 Lars T Hansen
;
; $Id$
;
; Test program to exhibit the nonblocking I/O system and tasking along 
; with sockets.

; You must call begin-tasking before calling test.

(require 'experimental/unix-descriptor)
(require 'experimental/iosys)
(require 'experimental/tasking-unix)
(require 'experimental/socket)

(define server #f)                      ; The server or #f
(define server-trips 0)                 ; just a counter
(define client-trips 0)                 ; just a counter

(define (test)
  (set! server-trips 0)
  (set! client-trips 0)
  (if (not server)
      (spawn (make-server 12345)))
  (spawn (make-client 12345 square 'square))
  (spawn (make-client 12345 double 'double)))

(define (square x) (* x x))
(define (double x) (+ x x))

(define (make-server port)
  (lambda ()
    (let ((s (server-socket port)))
      (set! server (current-task))
      (let accept-loop ()
        (let-values ((ns addr)
                     (wait-for-connection-on-server-socket s 'nonblocking))
          (spawn
           (make-server-thread ns)))
        (accept-loop)))))

(define (make-server-thread ns)
  (lambda ()
    (let ((in  (open-input-descriptor ns 'nonblocking 'char))
          (out (open-output-descriptor ns 'nonblocking 'char 'flush)))
      (let loop ((x (read in)))
        (set! server-trips (+ server-trips 1))
        (cond ((eq? (car x) 'done)
               (write 'done out)
               (newline out)
               (close-input-port in)
               (close-output-port out))
              ((eq? (car x) 'double)
               (write (double (cadr x)) out)
               (newline out)
               (loop (read in)))
              ((eq? (car x) 'square)
               (write (square (cadr x)) out)
               (newline out)
               (loop (read in)))
              (else ???))))))

(define (make-client port fn tag)
  (lambda ()
    (while (not server)
      (yield))
    (let ((s (client-socket "localhost" port)))
      (let ((in (open-input-descriptor s 'nonblocking 'char))
            (out (open-output-descriptor s 'nonblocking 'char 'flush)))
        (let loop ((i 0))
          (if (< i 10)
              (begin
                (set! client-trips (+ client-trips 1))
                (let ((secret (random 100)))
                  (write `(,tag ,secret) out)
                  (newline out)
                  (let ((ans (read in)))
                    (if (not (= ans (fn secret)))
                        (begin
                          (display "Error! ")
                          (display secret)
                          (display " ")
                          (display ans)
                          (newline)))
                    (sleep 2)
                    (loop (+ i 1)))))
              (begin
                (write '(done) out)
                (newline out)
                (read in)
                (close-input-port in)
                (close-output-port out))))))))

; No, this isn't right but it's OK for now.

(define (sleep n)
  (do ((i 0 (+ i 1)))
      ((= i n))
    (yield)))

; eof
