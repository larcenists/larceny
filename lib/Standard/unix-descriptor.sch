; Copyright 2007 Felix S Klock II
; 
; $Id: unix-descriptor.sch 4581 2007-06-19 14:55:05Z pnkfelix $
;
; Code that creates (blocking) ports on Unix file descriptors.  Use
; OPEN-INPUT-DESCRIPTOR and OPEN-OUTPUT-DESCRIPTOR to wrap an already
; open descriptor in an input or output port, respectively.
;
; This is a port of Experimental/unix-descriptor.sch to R6RS custom
; ports; note that nonblocking IO support was dropped during the port.

(require "Experimental/unix")

;; Sockets will open the same descriptor multiple times.  We do not
;; want to actually close the "file" until the last one.  Solving
;; this problem by tracking open file descriptors in a bag.
;; FIXME O(n) yucko
(define-values (add-fd-ref! rem-fd-ref!)
  (let ()
    (define bag '(sentinel)) ;; sentinel simplifies set! logic below
    (define (add-ref! fd)
      (set-cdr! bag (cons fd (cdr bag))))
    (define (rem-ref! fd) ;; returns #t if fd not in result bag.  O/w #f.
      (let loop ((l bag))
        (cond ((null? (cdr l)) 
               (error 'rem-ref! ": no references left!"))
              ((eqv? fd (cadr l))
               (set-cdr! l (cddr l)))
              (else 
               (loop (cdr l)))))
      (not (memv fd (cdr bag))))
    (values add-ref! rem-ref!)))
  

(define (open-input-descriptor fd)
  (define id (string-append "input descriptor port " (number->string fd)))
  (define (read! buf start count)
    (cond ((zero? start) ;; fast path
           (unix/read fd buf count))
          (else 
           ;; FIXME wouldn't it be nice to avoid copying; look into
           ;; revising unix.sch accordingly
           (let* ((bv (make-bytevector count))
                  (ret (unix/read fd bv count)))
             (do ((i 0 (+ i 1))
                  (j start (+ j 1)))
                 ((= i ret))
               (bytevector-like-set! buf j (bytevector-like-ref bv i)))
             ret))))
  (define (close)
    (cond ((rem-fd-ref! fd)
           (let ((res (unix/close fd)))
             (cond ((< res 0)
                    (error 'close ": error " res 
                           " closing input descriptor port " fd)))))))
  (add-fd-ref! fd)
  (make-custom-binary-input-port id read! #f #f close))

(define (open-output-descriptor fd)
  (define id (string-append "output descriptor port " (number->string fd)))
  (define (subbytevector bv start-incl end-excl)
    (let ((ret (make-bytevector (- end-excl start-incl))))
      (do ((i start-incl (+ i 1))
           (j 0 (+ j 1)))
          ((= i end-excl) ret)
        (bytevector-like-set! ret j (bytevector-like-ref bv i)))))
  ;; FIXME wouldn't it be nice to avoid copying here; look into
  ;; revising unix.sch accordingly
  (define (write-some buf idx count) 
    (unix/write fd (subbytevector buf idx (+ idx count)) count))
  (define (write! source-bv start count-orig)
    ;; FIXME R6RS says count of 0 should have effect of passing EOF to
    ;; byte sink.  What does that mean in this context?
    ;; FIXME after Will fixes custom ports to support writing < count,
    ;; get rid of loop here
    (let loop ((idx start) (count count-orig))
      (cond ((zero? count) count-orig)
            (else 
             (let ((written (write-some source-bv idx count)))
               (if (< written 0)
                   (error 'write! 
                          ": error writing to custom output "
                          "descriptor port " fd)
                   (loop (+ idx written) (- count written))))))))
  (define (close) 
    (cond ((rem-fd-ref! fd)
           (let ((res (unix/close fd)))
             (cond ((not (zero? res))
                    (error 'close ": error " res " closing custom output "
                           "descriptor port " fd)))))))
  (add-fd-ref! fd)
  (make-custom-binary-output-port id write! #f #f close))

; eof
