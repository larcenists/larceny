;For scheme48-1.3
; scheme48 -h 6000000
; ,open tables bitwise srfi-1 srfi-23 srfi-9 posix-files ascii

; only actually redefine these when hacking without s48 module system:

; (define integer->char ascii->char)
; (define char->integer char->ascii)

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
; (define logcount bit-count)

; from srfi-60 (see "other/srfi-60-pieces.scm" for license):
(define (bit-field n start end)
  (logand (lognot (ash -1 (- end start)))
	  (ash n (- start))))


;==================================;
; 				   ;
; SRFI 69 basic hash-tables	   ;
; 				   ;
;==================================;
; Sassy uses the following subset:
(define make-hash-table make-table)
(define hash-table-set!    table-set!)
(define (hash-table-ref t k . thunk)
  (let ((no (lambda ()
	      (if (null? thunk)
		  (error "key not in hash-table" k)
		  (apply (car thunk) '())))))
    (if (not (symbol? k))
	(no)
	(let ((r (table-ref t k)))
	  (or r (no))))))
(define (alist->hash-table lst)
  (let ((t (make-table)))
    (for-each (lambda (x)
		(table-set! t (car x) (cdr x)))
	      lst)
    t))
(define (hash-table-values table)
  (let ((r '()))
    (table-walk (lambda (k v) (set! r (cons v r))) table)
    r))
(define (hash-table-keys   table)
  (let ((r '()))
    (table-walk (lambda (k v) (set! r (cons k r))) table)
    r))
(define hash-table? table?)

;=============================;
; 			      ;
; MISC file operations	      ;
; 			      ;
;=============================;
; The file-targed output-modules use these
(define (file-exists? file)
  (accessible? file (access-mode exists)))
(define delete-file unlink)

