; Copyright 2007 William D Clinger.
;
; $Id$
;
; UTF-16 transcoding.

($$trace "utf16")

; Given a binary port for a source or sink of UTF-16 code units,
; returns an equivalent binary port for UTF-8 code units.

(define (utf16/transcoded-binary-port p)
  (let* ((bufsize 1024)
         (buffer (make-bytevector bufsize))
         (read-method
          (utf16/make-read-method p))
         (write-method
          (utf16/make-write-method p))
         (get-position-method
          (if (port-has-port-position? p)
              (lambda () (get-port-position p))
              #f))
         (set-position-method
          (if (port-has-set-port-position!? p)
              (lambda (posn) (set-port-position! p posn))
              #f))
         (close-method
          (lambda () (close-port p))))
    (cond ((and (input-port? p) (output-port? p))
           (make-custom-binary-input-port
            "utf-16"
            read-method write-method
            get-position-method set-position-method close-method))
          ((input-port? p)
           (make-custom-binary-input-port
            "utf-16"
            read-method
            get-position-method set-position-method close-method))
          ((output-port? p)
           (make-custom-binary-output-port
            "utf-16"
            write-method
            get-position-method set-position-method close-method))
          (else
           (assert #f) #t))))

(define (utf16/make-read-method p)
  (let ((read-method #f))
    (lambda (bv start count)
      (if read-method
          (read-method bv start count)
          (let* ((byte1 (get-u8 p))
                 (byte2 (get-u8 p)))
            (cond ((and (= byte1 #xfe) (= byte2 #xff))
                   (set! read-method
                         (utf16/make-read-method-endian p 'big))
                   (read-method bv start count))
                  ((and (= byte1 #xff) (= byte2 #xfe))
                   (set! read-method
                         (utf16/make-read-method-endian p 'little))
                   (read-method bv start count))
                  (else
                   (set! read-method
                         (utf16/make-read-method-endian p 'big))
                   (let ((buf (make-bytevector 2)))
                     (bytevector-set! buf 0 byte1)
                     (bytevector-set! buf 1 byte2)
                     (let* ((bv2 (string->utf8 (utf16->string buf)))
                            (n (bytevector-length bv2)))
                       (bytevector-copy! bv2 0 bv start n)
                       n)))))))))

(define (utf16/make-read-method-endian p endianness)
  (assert (memq endianness '(big little)))
  (let* ((bufsize 1024)
         (buffer (make-bytevector bufsize))
         (offset1 (case endianness ((big) 0) ((little) 1)))
         (offset2 (- 1 offset1)))

    (lambda (bv start count)
      (let ((count (max 2 (quotient (min bufsize count) 2)))
            (n (get-bytevector-n! p buffer 0 count)))

        (define (loop i j)
          (if (= i n)
              (- j start)
              (let ((byte1 (bytevector-ref buffer (+ i offset1)))
                    (byte2 (bytevector-ref buffer (+ i offset2))))

                ; Pass surrogate along to UTF-8 transcoder,
                ; where it will cause a decoding error.

                (define (decoding-error i j)
                  (decode3 i j))

                (define (decode3 i j)
                  (let ((b1 (fxior
                             #b11100000
                             (fxrshl byte1 4)
                             (fxlogand byte2 #b00000011)))
                        (b2 (fxlogior
                             #b10000000
                             (fxlogior
                              (fxlsh (fxlogand byte1 #b00001111) 2)
                              (fxrshl byte2 6))))
                        (b3 (fxlogior
                             #b10000000
                             (fxlogand byte2 #b00111111))))
                    (bytevector-set! bv j b1)
                    (bytevector-set! bv (+ j 1) b2)
                    (bytevector-set! bv (+ j 2) b3)
                    (loop (+ i 2) (+ j 3))))

                (cond ((= byte1 0)
                       (if (<= byte2 #x7f)
                           (begin (bytevector-set! bv j byte2)
                                  (loop (+ i 2) (+ j 1)))
                           (let ((b1 (fxlogior
                                      #b11000000
                                      (fxlogand byte2 #b00000011)))
                                 (b2 (fxlogior
                                      #b10000000
                                      (fxlogand byte2 #b00111111))))
                             (bytevector-set! bv j b1)
                             (bytevector-set! bv (+ j 1) b2)
                             (loop (+ i 2) (+ j 2)))))
                      ((<= byte1 7)
                       (let ((b1 (fxior
                                  #b11000000
                                  (fxlsh byte1 2)
                                  (fxlogand byte2 #b00000011)))
                             (b2 (fxlogior
                                  #b10000000
                                  (fxlogand byte2 #b00111111))))
                         (bytevector-set! bv j b1)
                         (bytevector-set! bv (+ j 1) b2)
                         (loop (+ i 2) (+ j 2))))
                      ((or (<= byte1 #xd7)
                           (<= #xe0 byte1))
                       (decode3 i j))
                      ((<= #xdc byte1)
                       (decoding-error i j))
                      ((= (+ i 2) n)
                       (let ((r (get-bytevector-n! p buffer n 2)))
                         (if (= r 2)
                             (loop i j)
                             (decoding-error i j))))
                      (else
                       (let ((byte3 (bytevector-ref buffer (+ i 2 offset1)))
                             (byte4 (bytevector-ref buffer (+ i 2 offset2))))
                         (if (<= #xdc byte3 #xdf)
                             (let* ((bits (fxior
                                           (fxlsh (fxlogand byte1 3) 18)
                                           (fxlsh byte2 10)
                                           (fxlsh (fxlogand byte3 3) 8)
                                           byte4))
                                    (sv (+ bits #x10000))
                                    (b1 (fxlogior #b11110000
                                                  (fxrshl bits 18)))
                                    (b2 (fxlogior #b10000000
                                                  (fxlogand #xb00111111
                                                            (rxrshl bits 12))))
                                    (b3 (fxlogior #b10000000
                                                  (fxlogand #xb00111111
                                                            (rxrshl bits 6))))
                                    (b3 (fxlogior #b10000000
                                                  (fxlogand #xb00111111
                                                            bits))))
                               (bytevector-set! bv j b1)
                               (bytevector-set! bv (+ j 1) b2)
                               (bytevector-set! bv (+ j 2) b3)
                               (bytevector-set! bv (+ j 3) b4)
                               (loop (+ i 4) (+ j 4)))
                             (decoding-error i j))))))))

        (assert (even? n))
        (loop 0 start)))))

; FIXME: Always uses big-endian UTF-16, which is R6RS-conforming
; but not always the right thing.

(define (utf16/make-write-method p)
  (utf16/make-write-method-endian p 'big))

; Translates UTF-8 to UTF-16 and writes UTF-16 code units to p.

(define (utf16/make-write-method-endian p endianness)

  (define (write16 b0 b1)
    (case endianness
     ((big)    (put-u8 p b0) (put-u8 p b1))
     ((little) (put-u8 p b1) (put-u8 p b0))
     (else
      (assertion-violation 'utf16/make-write-method-endian
                           "illegal argument"
                           endianness))))

  (assert (memq endianness '(big little)))

  ; Write byte-order mark.

  (write16 #xfe #xff)

  (lambda (bv start count)

    (define (return i)
      (- i start))

    (define (loop i limit)
      (if (= i limit)
          (return i)
          (let ((byte1 (bytevector-ref bv i)))
            (cond ((<= byte1 #x7f)
                   (write16 0 byte1)
                   (loop (+ i 1) limit))
                  ((= (+ i 1) limit)
                   (return i))
                  ((<= byte1 #xdf)
                   (let ((byte2 (bytevector-ref bv (+ i 1))))
                     (write16 (fxlogand #b00000111 (fxrshl byte1 2))
                              (fxlogior (fxlogand #b11000000 (fxlsh byte1 6))
                                        (fxlogand #b00111111 byte2)))
                     (loop (+ i 2) limit)))
                  ((= (+ i 2) limit)
                   (return i))
                  ((<= byte1 #xef)
                   (let ((byte2 (bytevector-ref bv (+ i 1)))
                         (byte3 (bytevector-ref bv (+ i 2))))
                     (write16 (fxlogior
                               (fxlogand #b11110000 (fxlsh byte1 4))
                               (fxlogand #b00001111 (fxrshl byte2 2)))
                              (fxlogior (fxlogand #b11000000 (fxlsh byte2 6))
                                        (fxlogand #b00111111 byte3)))
                     (loop (+ i 3) limit)))
                  ((= (+ i 3) limit)
                   (return i))
                  (else
                   (let* ((byte2 (bytevector-ref bv (+ i 1)))
                          (byte3 (bytevector-ref bv (+ i 2)))
                          (byte4 (bytevector-ref bv (+ i 3)))
                          (sv (fxior
                               (fxlsh (fxlogand byte1 #b00000111) 18)
                               (fxlsh (fxlogand byte2 #b00111111) 12)
                               (fxlsh (fxlogand byte3 #b00111111) 6)
                               (fxlogand byte4 #b00111111)))
                          (bits (- sv #x10000)))
                     (write16 (fxlogior #xd800 (fxrshl bits 10))
                              (fxlogior #xdc00 (fxlogand bits #x03ff)))
                     (loop (+ i 4) limit)))))))

    (loop start (+ start count))))

