; Bytevector primitives for the Racket host, implemented with Racket's
; byte strings (which are disjoint from vectors and strings).

(define (make-bytevector n . rest)
  (if (null? rest)
      (make-bytes n 0)
      (make-bytes n (car rest))))

(define bytevector-ref bytes-ref)
(define bytevector-set! bytes-set!)
(define bytevector-length bytes-length)
(define bytevector-copy bytes-copy)
(define bytevector? bytes?)

(define (bytevector-fill! bv b)
  (bytes-fill! bv b))

(define (list->bytevector l)
  (let* ((n (length l))
         (bv (make-bytes n 0)))
    (do ((i 0 (+ i 1))
         (l l (cdr l)))
        ((null? l) bv)
      (bytes-set! bv i (car l)))))

(define (bytevector->list bv)
  (do ((i (- (bytes-length bv) 1) (- i 1))
       (l '() (cons (bytes-ref bv i) l)))
      ((< i 0) l)))

;; for Sassy (SRFI-66 pseudo-compatibility)
(define list->u8vector list->bytevector)

(define bytevector-tag-set! (lambda (x y) '()))

(define (write-bytevector-like bv . rest)
  (let ((port (if (null? rest) (current-output-port) (car rest))))
    (cond ((bytes? bv)
           (write-bytes bv port))
          ((string? bv)
           (do ((i 0 (+ i 1)))
               ((= i (string-length bv)))
             (write-byte (char->integer (string-ref bv i)) port)))
          (else
           (error "write-bytevector-like: not a bytevector" bv)))))

; eof
