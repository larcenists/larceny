; Copyright 1999 Lars T Hansen
;
; $Id$
;
; A wrapper around the poll(2) system call.
; Life is nasty, brutish, and short.

(define (poll-descriptors inputs outputs block?)

  (define sizeof:struct_pollfd (+ sizeof:int sizeof:short sizeof:short))

  (define (pollfdv-fd-set! v i fd) 
    (%set-int v (* i sizeof:struct_pollfd) fd))

  (define (pollfdv-events-set! v i events)
    (%set-short v (+ (* i sizeof:struct_pollfd) sizeof:int) events))

  (define (pollfdv-fd-get v i) 
    (%get-int v (* i sizeof:struct_pollfd)))

  (define (pollfdv-revents-get v i) 
    (%get-short v (+ (* i sizeof:struct_pollfd) sizeof:int sizeof:short)))

  (let* ((n-i (length inputs))
         (n-o (length outputs))
         (n   (+ n-i n-o))
         (v   (make-bytevector (* n sizeof:struct_pollfd))))
    (bytevector-fill! v 0)
    (do ((i 0 (+ i 1))
         (fds inputs (cdr fds)))
        ((null? fds))
      (pollfdv-fd-set! v i (car fds))
      (pollfdv-events-set! v i (+ unix/POLLRDNORM unix/POLLRDBAND)))
    (do ((i n-i (+ i 1))
         (fds outputs (cdr fds)))
        ((null? fds))
      (pollfdv-fd-set! v i (car fds))
      (pollfdv-events-set! v i (+ unix/POLLWRNORM unix/POLLWRBAND)))
    (let ((r (unix/poll v n (if block? -1 0))))
      (cond ((< r 0)
             (error "Poll: " (unix/strerror (get-errno))))
            ((= r 0)
             '())
            (else
             (let loop ((ready '()) (i 0))
               (cond ((= i n) ready)
                     ((not (zero? (logand (pollfdv-revents-get v i)
                                          (+ unix/POLLERR
                                             unix/POLLNVAL))))
                      (error "Poll: error on descriptor "
                             (pollfdv-fd-get v i)))
                     ((not (zero? (pollfdv-revents-get v i)))
                      (loop (cons (pollfdv-fd-get v i) ready)
                            (+ i 1)))
                     (else
                      (loop ready (+ i 1))))))))))

; eof
