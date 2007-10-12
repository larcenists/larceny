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
        (if (and #f set-position!) ; FIXME
            (set! this
                  (io/make-port customio/ioproc name 'input 'output 'binary
                                'set-position!))
            (set! this
                  (io/make-port customio/ioproc name 'input 'binary)))
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
        (if (and #f set-position!) ; FIXME
            (set! this
                  (io/make-port customio/ioproc name 'input 'output 'binary
                                'set-position!))
            (set! this
                  (io/make-port customio/ioproc name 'output 'binary)))
        this)
      (assertion-violation 'make-custom-binary-output-port
                           "illegal argument(s)"
                           id write! get-position set-position! close)))

; Input/output ports always read or write exactly one byte or character
; at a time.

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
                (let ((n (read! buffer 0 1)))
                  (cond ((not (fixnum? n)) 'error)
                        ((fx= n 0) 'eof)
                        ((fx> n 0) n)
                        (else 'error)))))
             (write-method
              (lambda (data buffer count)
                (let loop ((start 0)
                           (count count))
                  (let ((n (write! buffer start 1)))
                    (cond ((not (fixnum? n)) 'error)
                          ((fx= n count) 'ok)
                          ((fx= n 0) 'error)        ; no progress, or bogus eof
                          ((fx< n count)
                           (loop (+ start n) (- count n)))
                          (else 'error)))))))
        (define (customio/ioproc op)
          (case op
           ((read)   read-method)
           ((write)  write-method)
           ((close)  (lambda (data) (if close (close))))
           ((name)   (lambda (data) name))
           (else
            (customio/unsupported-operation
             'make-custom-binary-input/output-port this op))))
        (if set-position!
            (set! this
                  (io/make-port customio/ioproc name 'input 'output 'binary
                                'set-position!))
            (set! this
                  (io/make-port customio/ioproc name 'input 'output 'binary)))
        this)
      (assertion-violation 'make-custom-binary-input/output-port
                           "illegal argument(s)"
                           id write! get-position set-position! close)))

; Custom textual ports.

(define (customio/make-textual-input-port
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
                (let* ((n0 (bytevector-length buffer))
                       (n4 (quotient n0 4))
                       (s  (make-string n4))
                       (n  (read! s 0 n4)))
                  (cond ((not (fixnum? n)) 'error)
                        ((fx= n 0) 'eof)
                        ((fx> n 0)
                         (let* ((s  (if (= n n4) s (substring s 0 n)))
                                (bv (string->utf8 s))
                                (k  (bytevector-length bv)))
                           (bytevector-copy! bv 0 buffer 0 k)
                           k))
                        (else 'error))))))
        (define (customio/ioproc op)
          (case op
           ((read)   read-method)
           ((close)  (lambda (data) (if close (close))))
           ((name)   (lambda (data) name))
           (else
            (customio/unsupported-operation
             'make-custom-textual-input-port this op))))
        (if (and #f set-position!) ; FIXME
            (set! this
                  (io/make-port customio/ioproc name 'input 'output 'binary
                                'set-position!))
            (set! this
                  (io/make-port customio/ioproc name 'input 'binary)))
        (set! this (io/custom-transcoded-port this))
        this)
      (assertion-violation 'make-custom-textual-input-port
                           "illegal argument(s)"
                           id read! get-position set-position! close)))

; This procedure does not have to check for decoding errors,
; because the code units in the buffer were derived from
; legitimate Unicode scalar values.

(define (customio/make-textual-output-port
         id write! get-position set-position! close)
  (if (and (string? id)
           (procedure? write!)
           (or (procedure? get-position)  (eq? #f get-position))
           (or (procedure? set-position!) (eq? #f set-position!))
           (or (procedure? close)         (eq? #f close)))
      (let* ((name (string-copy id))
             (this '*)
             (write-method
              (lambda (data buffer count0)

                ; Starting with (bytevector-ref buffer start),
                ; decodes up to count bytes and stores the decoded
                ; characters into the string s.  i is the number of
                ; characters that have been stored into s so far,
                ; and is also the next index into s at which a
                ; character should be stored.

                (define (loop1 start count s i)
                  (if (<= count 0)
                      (loop2 s 0 i)

                      (let ((unit1 (bytevector-ref buffer start)))

                        (define (decode1)
                          (string-set! s i (integer->char unit1))
                          (continue 1))

                        (define (decode2)
                          (if (<= 2 count)
                              (let* ((unit2
                                      (bytevector-ref buffer (+ start 1)))
                                     (sv
                                      (fxlogior
                                       (fxlsh (fxlogand unit1 #b00011111) 6)
                                       (fxlogand unit2 #b00111111))))
                                (string-set! s i (integer->char sv))
                                (continue 2))
                              (loop2 s 0 i)))

                        (define (decode3)
                          (if (<= 3 count)
                              (let* ((unit2
                                      (bytevector-ref buffer (+ start 1)))
                                     (unit3
                                      (bytevector-ref buffer (+ start 2)))
                                     (sv
                                      (fxior
                                       (fxlsh (fxlogand unit1 #b00001111) 12)
                                       (fxlsh (fxlogand unit2 #b00111111) 6)
                                       (fxlogand unit3 #b00111111))))
                                (string-set! s i (integer->char sv))
                                (continue 3))
                              (loop2 s 0 i)))

                        (define (decode4)
                          (if (<= 4 count)
                              (let* ((unit2
                                      (bytevector-ref buffer (+ start 1)))
                                     (unit3
                                      (bytevector-ref buffer (+ start 2)))
                                     (unit4
                                      (bytevector-ref buffer (+ start 3)))
                                     (sv
                                      (fxior
                                       (fxlsh (fxlogand unit1 #b00000111) 18)
                                       (fxlsh (fxlogand unit2 #b00111111) 12)
                                       (fxlsh (fxlogand unit3 #b00111111) 6)
                                       (fxlogand unit4 #b00111111))))
                                (string-set! s i (integer->char sv))
                                (continue 4))
                              (loop2 s 0 i)))

                        (define (continue k)
                          (loop1 (+ start k) (- count k) s (+ i 1)))

                        (cond ((<= unit1 #x7f)
                               (decode1))
                              ((<= unit1 #xdf)
                               (decode2))
                              ((<= unit1 #xef)
                               (decode3))
                              (else
                               (decode4))))))

                (define (loop2 s start count)
                  (let ((n (write! s start count)))
                    (cond ((not (fixnum? n)) 'error)
                          ((fx= n count) 'ok)
                          ((fx= n 0) 'error)        ; no progress, or bogus eof
                          ((fx< n count)
                           (loop2 s (+ start n) (- count n)))
                          (else 'error))))

                (loop1 0 count0 (make-string count0) 0))))

        (define (customio/ioproc op)
          (case op
           ((write)  write-method)
           ((close)  (lambda (data) (if close (close))))
           ((name)   (lambda (data) name))
           (else
            (customio/unsupported-operation
             'make-custom-textual-output-port this op))))
        (if (and #f set-position!) ; FIXME
            (set! this
                  (io/make-port customio/ioproc name 'input 'output 'binary
                                'set-position!))
            (set! this
                  (io/make-port customio/ioproc name 'output 'binary)))
        (set! this (io/custom-transcoded-port this))
        this)
      (assertion-violation 'make-custom-textual-output-port
                           "illegal argument(s)"
                           id write! get-position set-position! close)))

; Input/output ports always read or write exactly one byte or character
; at a time.

(define (customio/make-textual-input/output-port
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
                (let* ((s (make-string 1))
                       (n (read! s 0 1)))
                  (cond ((not (fixnum? n)) 'error)
                        ((eq? n 0) 'eof)
                        ((eq? n 1)
                         (let* ((bv (string->utf8 s))
                                (k  (bytevector-length bv)))
                           (bytevector-copy! bv 0 buffer 0 k)
                           k))
                        (else 'error)))))
             (write-method
              (lambda (data buffer count)
                (assert (<= count 4))
                (let ((bv (make-bytevector count)))
                  (bytevector-copy! buffer 0 bv 0 count)
                  (let ((s (utf8->string bv)))
                    (assert (= (string-length s) 1))
                    (let ((n (write! s 0 1)))
                      (cond ((eq? n 1) 'ok)
                            (else 'error))))))))    ; no progress, or bogus eof
        (define (customio/ioproc op)
          (case op
           ((read)   read-method)
           ((write)  write-method)
           ((close)  (lambda (data) (if close (close))))
           ((name)   (lambda (data) name))
           (else
            (customio/unsupported-operation
             'make-custom-textual-input/output-port this op))))
        (if set-position!
            (set! this
                  (io/make-port customio/ioproc name 'input 'output 'binary
                                'set-position!))
            (set! this
                  (io/make-port customio/ioproc name 'input 'output 'binary)))
        (set! this (io/custom-transcoded-port this))
        this)
      (assertion-violation 'make-custom-textual-input/output-port
                           "illegal argument(s)"
                           id write! get-position set-position! close)))

(define (customio/unsupported-operation who port op)
  (assertion-violation who "unsupported-operation" port op))
