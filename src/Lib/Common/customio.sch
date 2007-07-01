; Copyright 2007 William D Clinger
;
; $Id$
;
; R6RS-style custom I/O ports.

($$trace "customio")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Custom ports.
;
; FIXME:  This is as close as we can come without extending the
; ioproc conventions of Larceny v0.93.
; That leaves get-position and set-position! for v0.95.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 'ioproc' is a procedure of one argument: a symbol that denotes the 
; operation to perform.  It returns a port-specific procedure that, when
; called, performs the operation.  The operations are:
;
;   read : iodata * buffer -> { fixnum, 'eof, 'error }
;   write : iodata * buffer * count -> { 'ok, 'error }
;   close : iodata -> { 'ok, 'error }
;   ready? : iodata -> boolean
;   name : iodata -> string

(define (customio/make-binary-input-port
         id read! get-position set-position! close)
  (if (and (string? id)
           (procedure? read!)
           (or (procedure? get-position)  (eq? #f get-position))
           (or (procedure? set-position!) (eq? #f set-position!))
           (or (procedure? close)         (eq? #f close)))
      (let* ((name (string-copy id))
             (this '*)
             (read-method
              (lambda (data buffer)
                (let ((n (read! buffer 0 (bytevector-length buffer))))
                  (cond ((not (fixnum? n)) 'error)
                        ((fx= n 0) 'eof)
                        ((fx> n 0) n)
                        (else 'error))))))
        (define (customio/ioproc op)
          (case op
           ((read)   read-method)
           ((close)  (lambda (data) (if close (close))))
           ((name)   (lambda (data) name))
           (else
            (customio/unsupported-operation
             'make-custom-binary-input-port this op))))
        (set! this (io/make-port customio/ioproc name 'input 'binary))
        this)
      (assertion-violation 'make-custom-binary-input-port
                           "illegal argument(s)"
                           id read! get-position set-position! close)))

(define (customio/make-binary-output-port
         id write! get-position set-position! close)
  (if (and (string? id)
           (procedure? write!)
           (or (procedure? get-position)  (eq? #f get-position))
           (or (procedure? set-position!) (eq? #f set-position!))
           (or (procedure? close)         (eq? #f close)))
      (let* ((name (string-copy id))
             (this '*)
             (write-method
              (lambda (data buffer count)
                (let loop ((start 0)
                           (count count))
                  (let ((n (write! buffer start count)))
                    (cond ((not (fixnum? n)) 'error)
                          ((fx= n count) 'ok)
                          ((fx= n 0) 'error)        ; no progress, or bogus eof
                          ((fx< n count)
                           (loop (+ start n) (- count n)))
                          (else 'error)))))))
        (define (customio/ioproc op)
          (case op
           ((write)  write-method)
           ((close)  (lambda (data) (if close (close))))
           ((name)   (lambda (data) name))
           (else
            (customio/unsupported-operation
             'make-custom-binary-output-port this op))))
        (set! this (io/make-port customio/ioproc name 'output 'binary))
        this)
      (assertion-violation 'make-custom-binary-output-port
                           "illegal argument(s)"
                           id write! get-position set-position! close)))

; FIXME:  This won't work yet.
; FIXME:  The return values need to be handled as above.

(define (customio/make-binary-input/output-port
         id read! write! get-position set-position! close)
  (if (and (string? id)
           (procedure? read!)
           (procedure? write!)
           (or (procedure? get-position)  (eq? #f get-position))
           (or (procedure? set-position!) (eq? #f set-position!))
           (or (procedure? close)         (eq? #f close)))
      (let* ((name (string-copy id))
             (this '*)
             (read-method
              (lambda (data buffer)
                (read! buffer 0 (bytevector-length buffer))))
             (write-method
              (lambda (data buffer count)
                (write! buffer 0 count))))
        (define (customio/ioproc op)
          (case op
           ((read)   read-method)
           ((write)  write-method)
           ((close)  (lambda (data) (if close (close))))
           ((name)   (lambda () name))
           (else
            (customio/unsupported-operation
             'make-custom-binary-input/output-port this op))))
        (set! this (io/make-port customio/ioproc name 'input 'output 'binary))
        this)
      (assertion-violation 'make-custom-binary-input/output-port
                           "illegal argument(s)"
                           id write! get-position set-position! close)))

; FIXME:  The three textual analogues of the above aren't implemented yet.

(define (customio/unsupported-operation who port op)
  (assertion-violation who "unsupported-operation" port op))
