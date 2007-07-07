; Creates ports (blocking only, binary mode) on Winsock SOCKETs.

(require "Experimental/winsock")

;; This file is based on lib/Standard/unix-descriptor.sch -- look there
;; for what this is all about.

(define-values (add-sock-ref! rem-sock-ref!)
  (let ()
    (define bag '(sentinel)) ;; sentinel simplifies set! logic below
    (define (add-ref! sock)
      (set-cdr! bag (cons sock (cdr bag))))
    (define (rem-ref! sock) ;; returns #t if sock not in result bag.  O/w #f.
      (let loop ((l bag))
        (cond ((null? (cdr l)) 
               (error 'rem-ref! ": no references left!"))
              ((eqv? sock (cadr l))
               (set-cdr! l (cddr l)))
              (else 
               (loop (cdr l)))))
      (not (memv sock (cdr bag))))
    (values add-ref! rem-ref!)))

(define (open-input-winsocket sock)
  (define id (string-append "winsocket "
                            (number->string sock)))
  (define (read! buf start count)
    (let* ((bv (make-bytevector count))
           (ret (winsock/recv sock bv count 0)))
      (do ((i 0 (+ i 1))
           (j start (+ j 1)))
        ((= i ret) ret)
        (bytevector-like-set! buf j (bytevector-like-ref bv i)))))
  (define (close)
    (winsockio/close sock winsock/SD_RECEIVE))
  (add-sock-ref! sock)
  (make-custom-binary-input-port id read! #f #f close))

(define (open-output-winsocket sock)
  (define id (string-append "winsocket "
                            (number->string sock)))
  (define (subbytevector bv start-incl end-excl)
    (let ((ret (make-bytevector (- end-excl start-incl))))
      (do ((i start-incl (+ i 1))
           (j 0 (+ j 1)))
          ((= i end-excl) ret)
        (bytevector-like-set! ret j (bytevector-like-ref bv i)))))
  (define (write! buf idx count)
    (winsock/send sock (subbytevector buf idx (+ idx count)) count 0))
  (define (close)
    (winsockio/close sock winsock/SD_SEND))
  (add-sock-ref! sock)
  (make-custom-binary-output-port id write! #f #f close))

(define (winsockio/close sock how)
  (let ((res (if (rem-sock-ref! sock)
               (winsock/closesocket sock)
               (winsock/shutdown sock how))))
    (cond ((< res 0)
           (error 'close ": error " res 
                  " closing winsocket port " sock)))))

