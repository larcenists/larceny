;For csi (chicken) 2.2 with libffi (and syntax-case, numbers eggs)

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
(load "other/srfi-60-pieces.scm")

