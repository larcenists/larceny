;;; Experimental library to get us semi-reliable IA32 disassembly by
;;; delegating the job to the nasm disassembler ndisasm. 
;;; 
;;; Note that the output can be misleading especially because we
;;; currently encode exception codes directly in the instruction
;;; stream, which the disassembler has no knowledge of. (To find this
;;; in the IAssassin backend, just search for the token 'dwords'; that
;;; is the directive for emitting constants in Sassy.)

(define (nasm-disassemble-bytevector bv)
  (let ((tempfile "nasmtemp.o"))
    (let ((out (open-file-output-port tempfile)))
      (put-bytevector out bv)
      (flush-output-port out)
      (close-output-port out))
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
