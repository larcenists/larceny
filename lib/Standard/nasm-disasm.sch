(define (nasm-disassemble-bytevector bv)
  (let ((tempfile "nasmtemp.o"))
    (call-with-output-file tempfile
      (lambda (out)
        (for-each (lambda (byte)
                    (write-char (integer->char byte) out))
                  (bytevector->list bv))))
    (system (string-append "ndisasm -b32 " tempfile))))

(define (nasm-disassemble-procedure p)
  (nasm-disassemble-bytevector (procedure-ref p 0)))

(define (nasm-disassemble x)
  (cond
   ((procedure? x) 
    (nasm-disassemble-procedure x))
   ((bytevector? x)
    (nasm-disassemble-bytevector x))
   (else
    (error 'nasm-disassemble "Unknown input type ~a" x))))
