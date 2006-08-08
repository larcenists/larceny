(require-extension syntax-case)
(require-extension numbers)
(require-extension srfi-1)

(define (write-byte n . port)
  (apply write-char (cons (integer->char n) port)))
(define (read-byte . port)
  (let ((n (apply read-char port)))
    (if (eof-object? n)
	n
	(char->integer n))))

;=================================;
; 				  ;
; SRFI 60 integers as bits	  ;
; 				  ;
;=================================;
; Sassy uses the following subset:
(define logior bitwise-ior)
(define logand bitwise-and)
(define lognot bitwise-not)
(define ash arithmetic-shift)
(include "other/srfi-60-pieces.scm")
(include "other/srfi-56-pieces.scm")

;==============;
; 	       ;
; Sassy	       ;
; 	       ;
;==============;
(include "extras.scm")
(include "units.scm")
(include "push-stacks.scm")
(include "api.scm")
(include "intern.scm")
(include "macros.scm")
(include "numbers.scm")
(include "operands.scm")
(include "text-block.scm")
(include "opcodes.scm")
(include "text.scm")
(include "parse.scm")
(include "main.scm")

;=======================;
; 		        ;
; Output Modules        ;
; 		        ;
;=======================;
(include "flat-bin.scm")
(include "elf.scm")

;========================;
; 			 ;
; The test suite	 ;
; 			 ;
;========================;
; eval the following two expressions in order to run all the tests.

; (load "tests/run-tests.scm")
; (sassy-run-tests 'all)
