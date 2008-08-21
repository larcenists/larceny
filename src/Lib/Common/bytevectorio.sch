; Copyright 1998 Lars T Hansen.
; Modifed from stringio.sch for bytevectors by Will Clinger in 2007.
;
; $Id$
;
; Bytevector I/O ports.

; Offsets in the bytevector port data structure.
;
; The type is one of the symbols
;     bytevector-input-port
;     bytevector-output-port
;     bytevector-input/output-port
; The value of i is the current position in bytes;
;     note that i may be greater than limit,
;     thanks to the R6RS semantics of set-port-position!,
;     which is modelled after the Posix semantics of lseek.
; The value of limit is the current size of the output.

(define bytevector-io.type 0)  ; symbol
(define bytevector-io.bv 1)    ; bytevector
(define bytevector-io.i 2)     ; fixnum (current position)
(define bytevector-io.limit 3) ; fixnum (always <= length of bv)

; Initial size for output ports.
; Not very critical; small sizes improve test coverage.

(define bytevector-io:headroom 10)

(define (bytevector-io/open-input-bytevector bv)
  (if (not (bytevector? bv))
      (assertion-violation 'open-input-bytevector "illegal argument" bv))
  (bytevector-io/open-input-bytevector-no-copy (bytevector-copy bv)))

(define (bytevector-io/open-input-bytevector-no-copy bv)
  (if (not (bytevector? bv))
      (assertion-violation 'open-bytevector-input-port "illegal argument" bv))
  (io/make-port bytevector-io/ioproc
                (vector 'bytevector-input-port bv 0 (bytevector-length bv))
                'input 'binary 'set-position!))

(define (bytevector-io/open-output-bytevector)
  (io/make-port bytevector-io/ioproc
                (vector 'bytevector-output-port
                        (make-bytevector bytevector-io:headroom 0)
                        0 0)
                'output 'binary 'set-position!))

(define (bytevector-io/open-input/output-bytevector bv)
  (if (not (bytevector? bv))
      (assertion-violation 'open-input/output-bytevector
                           "illegal argument" bv))
  (bytevector-io/open-input/output-bytevector-no-copy (bytevector-copy bv)))

(define (bytevector-io/open-input/output-bytevector-no-copy bv)
  (io/make-port bytevector-io/ioproc1
                (vector 'bytevector-input/output-port
                        bv 0 (bytevector-length bv))
                'input 'output 'binary 'set-position!))

(define (bytevector-io/get-output-bytevector port)
  (if (not (bytevector-output-port? port))
      (assertion-violation 'get-output-bytevector "illegal argument" port))
  (flush-output-port port)
  (let* ((data  (vector-like-ref port port.iodata))
         (bv    (vector-ref data bytevector-io.bv))
         (limit (vector-ref data bytevector-io.limit))
         (r     (make-bytevector limit)))
    (bytevector-copy! bv 0 r 0 limit)
    r))

(define (bytevector-io/reset-output-bytevector port)
  (if (not (bytevector-output-port? port))
      (assertion-violation 'reset-output-bytevector "illegal argument" port))
  (flush-output-port port)
  (let ((data (vector-like-ref port port.iodata)))
    (vector-set! data
                 bytevector-io.bv
                 (make-bytevector bytevector-io:headroom 0))
    (vector-set! data bytevector-io.i 0)
    (vector-set! data bytevector-io.limit 0)
    (unspecified)))

(define (bytevector-io/ioproc op)
  (case op
    ((read)          bytevector-io/fill-buffer)
    ((write)         bytevector-io/flush-buffer)
    ((close)         (lambda (data) #t))
    ((ready?)        (lambda (data) #t))
    ((name)          (lambda (data) "*bytevector*"))
    ((set-position!) bytevector-io/set-position!)
    (else
     (error "bytevector-io/ioproc: illegal operation: " op))))

; Same as above, but for input/output bytevector ports,
; which transfer only 1 byte at a time.

(define (bytevector-io/ioproc1 op)
  (case op
    ((read)          bytevector-io/fill-buffer1)
    ((write)         bytevector-io/flush-buffer)
    ((close)         (lambda (data) #t))
    ((ready?)        (lambda (data) #t))
    ((name)          (lambda (data) "*bytevector*"))
    ((set-position!) bytevector-io/set-position!)
    (else
     (error "bytevector-io/ioproc: illegal operation: " op))))

(define (bytevector-io/fill-buffer data buffer)
  (let* ((b     (vector-ref data bytevector-io.bv))
	 (i     (vector-ref data bytevector-io.i))
         (limit (vector-ref data bytevector-io.limit))
         (n     (bytevector-length buffer))
         (count (max (min n (- limit i)))))
    (if (<= count 0)
        'eof
        (begin (bytevector-copy! b i buffer 0 count)
               (vector-like-set! data bytevector-io.i (+ i count))
               count))))

; For input/output bytevector ports.  Notice the definition of n.

(define (bytevector-io/fill-buffer1 data buffer)
  (let* ((b     (vector-ref data bytevector-io.bv))
	 (i     (vector-ref data bytevector-io.i))
         (limit (vector-ref data bytevector-io.limit))
         (n     1)
         (count (max 0 (min n (- limit i)))))
    (if (<= count 0)
        'eof
        (begin (bytevector-copy! b i buffer 0 count)
               (vector-like-set! data bytevector-io.i (+ i count))
               count))))

(define (bytevector-io/flush-buffer data buffer count)
  (let* ((bv    (vector-ref data bytevector-io.bv))
         (i     (vector-ref data bytevector-io.i))
         (limit (vector-ref data bytevector-io.limit))
         (n     (bytevector-length bv))
         (new-i (+ i count)))
    (if (< n new-i)
        (begin (bytevector-io/expand-buffer! data count)
               (bytevector-io/flush-buffer data buffer count))
        (begin (bytevector-copy! buffer 0 bv i count)
               (vector-set! data bytevector-io.i new-i)
               (if (< limit new-i)
                   (vector-set! data bytevector-io.limit new-i))
               'ok))))

(define (bytevector-io/set-position! data posn)
  (case (vector-ref data bytevector-io.type)
   ((bytevector-input-port
     bytevector-output-port
     bytevector-input/output-port)
    (let* ((bv (vector-ref data bytevector-io.bv))
           (n  (bytevector-length bv)))
      (if (<= 0 posn)
          (begin (vector-set! data bytevector-io.i posn)
                 'ok)
          'error)))
   (else
    (assert #f)
    'error)))

; Doubles the size of the bytevector (approximately).

(define (bytevector-io/expand-buffer! data count)
  (let* ((bv    (vector-ref data bytevector-io.bv))
         (i     (vector-ref data bytevector-io.i))
         (limit (vector-ref data bytevector-io.limit))
         (n     (bytevector-length bv))
         (new-i (+ i count)))
    (let ((bv2 (make-bytevector (+ (* 2 new-i) bytevector-io:headroom) 0)))
      (bytevector-copy! bv 0 bv2 0 limit)
      (vector-set! data bytevector-io.bv bv2))))

(define (bytevector-input-port? port)
  (let ((d (vector-like-ref port port.iodata)))
    (and (vector? d)
	 (> (vector-length d) 0)
         (memq (vector-ref d 0)
               '(bytevector-input-port bytevector-input/output-port)))))

(define (bytevector-output-port? port)
  (let ((d (vector-like-ref port port.iodata)))
    (and (vector? d)
	 (> (vector-length d) 0)
         (memq (vector-ref d 0)
               '(bytevector-output-port bytevector-input/output-port)))))

; eof
