; Copyright 1998 Lars T Hansen.
; Modifed from stringio.sch for bytevectors by Will Clinger in 2007.
;
; $Id$
;
; Bytevector I/O ports, only partially compatible with the draft R6RS.
; FIXME:  Doesn't support set-port-position!
;
; FIXME: inefficient representation for output strings

; Offsets in the bytevector port data structure.

(define bytevector-io.type 0)  ; symbol
(define bytevector-io.bv 1)    ; Input: bytevector; output: list of bytevectors
(define bytevector-io.i 2)     ; Input: fixnum; output: garbage

(define (bytevector-io/open-input-bytevector bv)
  (if (not (bytevector? bv))
      (assertion-violation 'open-bytevector-input-port "illegal argument" bv))
  (bytevector-io/open-input-bytevector-no-copy (bytevector-copy bv)))

(define (bytevector-io/open-input-bytevector-no-copy bv)
  (if (not (bytevector? bv))
      (assertion-violation 'open-bytevector-input-port "illegal argument" bv))
  (io/make-port bytevector-io/ioproc
                (vector 'bytevector-input-port bv 0)
                'input 'binary))

(define (bytevector-io/open-output-bytevector)
  (io/make-port bytevector-io/ioproc
                (vector 'bytevector-output-port '() 0) 'output 'binary))

(define (bytevector-io/get-output-bytevector port)
  (if (not (bytevector-output-port? port))
      (assertion-violation 'get-output-bytevector "illegal argument" port))
  (flush-output-port port)
  (let* ((data (vector-like-ref port port.iodata))
         (bufs (vector-ref data bytevector-io.bv)))
    (define (loop1 rbufs n bufs)
      (if (null? rbufs)
          (loop2 (make-bytevector n) 0 bufs)
          (loop1 (cdr rbufs)
                 (+ n (bytevector-length (car rbufs)))
                 (cons (car rbufs) bufs))))
    (define (loop2 bvec i bufs)
      (if (null? bufs)
          (begin (assert (= i (bytevector-length bvec)))
                 bvec)
          (let* ((buf (car bufs))
                 (n (bytevector-length buf)))
            (bytevector-copy! buf 0 bvec i n)
            (loop2 bvec (+ i n) (cdr bufs)))))
    (let ((result (loop1 bufs 0 '())))
      (vector-set! data bytevector-io.bv (list result))
      result)))

(define (bytevector-io/reset-output-bytevector port)
  (if (not (bytevector-output-port? port))
      (assertion-violation 'reset-output-bytevector "illegal argument" port))
  (flush-output-port port)
  (let ((data (vector-like-ref port port.iodata)))
    (vector-like-set! data bytevector-io.bv '())
    (unspecified)))

(define (bytevector-io/ioproc op)
  (case op
    ((read)   bytevector-io/fill-buffer)
    ((write)  bytevector-io/flush-buffer)
    ((close)  (lambda (data) #t))
    ((ready?) (lambda (data) #t))
    ((name)   (lambda (data) "*bytevector*"))
    (else     (error "bytevector-io/ioproc: illegal operation: " op))))

(define (bytevector-io/fill-buffer data buffer)
  (let ((b (vector-ref data bytevector-io.bv))
	(i (vector-ref data bytevector-io.i)))
    (let ((n (min (bytevector-like-length buffer)
                  (- (bytevector-length b) i))))
      (if (= n 0)
	  'eof
          (begin (bytevector-copy! b i buffer 0 n)
                 (vector-like-set! data bytevector-io.i (+ i n))
                 n)))))

(define (bytevector-io/flush-buffer data buffer count)
  (let ((bv (make-bytevector count)))
    (bytevector-copy! buffer 0 bv 0 count)
    (vector-set! data bytevector-io.bv
                 (cons bv (vector-ref data bytevector-io.bv))))
  'ok)

(define (bytevector-input-port? port)
  (let ((d (vector-like-ref port port.iodata)))
    (and (vector? d)
	 (> (vector-length d) 0)
	 (eq? (vector-ref d 0) 'bytevector-input-port))))

(define (bytevector-output-port? port)
  (let ((d (vector-like-ref port port.iodata)))
    (and (vector? d)
	 (> (vector-length d) 0)
	 (eq? (vector-ref d 0) 'bytevector-output-port))))

; eof
