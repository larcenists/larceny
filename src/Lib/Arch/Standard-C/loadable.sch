; Copyright 1999 Lars T Hansen
;
; $Id$

(define petit-shared-filenames '())   ; List of (filename id table-address)
(define petit-next-shared-id    -1)   ; Next ID to use (always negative)

; Given a Petit Larceny compiled code-less toplevel thunk, and an 
; ID/number pair that denote its code vector, fetch the code vector 
; from system tables and patch the procedure, returning the procedure.
;
; If the ID is negative then it denotes a dynamically loaded object;
; otherwise it denotes an object linked into the runtime system.
;
; WARNING: no error checking.  

(define (.petit-patch-procedure id number proc)
  (procedure-set! proc 
		  0 
		  (if (negative? id)
		      (lookup-code-pointer-in-table
		       (caddr (list-ref petit-shared-filenames (- (- id) 1)))
		       number)
		      (syscall syscall:segment-code-address id number)))
  proc)

; This code really wants BYTEVECTOR-WORD-REF.

(define lookup-code-pointer-in-table 
  (let ((endianness #f)
	(word-size  0)
	(shift      0))
    (lambda (table idx)
      (if (not endianness)
	  (let ((features (system-features)))
	    (set! word-size  (quotient (cdr (assq 'arch-word-size features)) 8))
	    (set! endianness (cdr (assq 'arch-endianness features)))
	    (set! shift (let ((cvr (cdr (assq 'codevector-representation features))))
			  (case cvr
			    ((address) 0)
			    ((address-shifted-1) 1)
			    ((address-shifted-2) 2)
			    (else (error "loadable.sch: Bad codevector representation: " cvr)))))))
      (let ((bv (make-bytevector word-size)))
	(syscall syscall:peek-bytes (+ table (* idx word-size)) bv word-size)
	(let ((x (let loop ((i 0) (x 0))
                   ; assemble the word from its bytes
                   (if (= i word-size)
                       x
                       (loop (+ i 1)
                             (+ (* 256 x)
                                (bytevector-ref bv (if (eq? endianness 'big)
                                                       i
                                                       (- word-size i 1)))))))))
          ; The result is the fixnum whose representation is the address.
	  (fxlsh (let ((p (quotient x 4)))
		 (if (fixnum? p)
		     p
		     (- p (expt 2 (- (* 8 word-size) 2)))))
	       shift))))))

; Given the name of a shared object file that contains compiled code for
; one or more FASL files, load the file if it is not already loaded.  Register
; the code pointer table in the shared object with the runtime, and return an
; identifying fixnum for the shared object to allow lookups.
;
; WARNING: filenames are not canonical.

(define (.petit-shared-object filename)

  (define (string->asciiz str)
    (let ((b (make-bytevector (+ (string-length str) 1))))
      (bytevector-set! b (string-length str) 0)
      (do ((i (- (string-length str) 1) (- i 1)))
	  ((< i 0) b)
	(bytevector-set! b i (char->integer (string-ref str i))))))

  (define (load-and-register filename)
    (let ((shared-object 
	   (syscall syscall:c-ffi-dlopen (string->asciiz filename))))
      (if (zero? shared-object)
	  (error "Unable to open shared object " filename))
      (let ((table (syscall syscall:c-ffi-dlsym
			    shared-object
			    (string->asciiz "twobit_load_table"))))
	(if (zero? table)
	    (error "Unable to find 'twobit_load_table' in shared object " 
		   filename))
	(let ((id petit-next-shared-id))
	  (set! petit-next-shared-id (- petit-next-shared-id 1))
	  (set! petit-shared-filenames 
		(append! petit-shared-filenames
			 (list (list (string-copy filename) id table))))
	  id))))

  (cond ((assoc filename petit-shared-filenames) 
	 => cadr)
	(else
	 (load-and-register filename))))

; eof
