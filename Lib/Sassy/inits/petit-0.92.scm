; for petit-larceny (bin-nasm-linux86) 0.92
; Assumes a correctly set up LARCENY_ROOT

(for-each 
 require
 '(srfi-0 srfi-1 srfi-9 
   srfi-23
   srfi-60))

; This all sassy needs from deprecated srfi-56, and ONLY if you are
; using sassy-make-elf,sassy-make-bin
(define (read-byte . port)
  (let ((x (apply read-char port)))
    (if (eof-object? x)
	x
	(char->integer x))))

(define (write-byte b . port)
  (apply write-char (integer->char b) port))

(define make-hash-table #f)
(define hash-table-set! #f)
(define hash-table-ref  #f)
(define hash-table-values #f)
(define hash-table-keys   #f)
(define hash-table-walk   #f)

(define (hash-table? obj)
  (and (vector? obj) (equal? (vector-ref obj 0) (list "HASHTABLE"))))

(define (alist->hash-table alst)
  (let ((t (make-hash-table)))
    (for-each (lambda (kv)
		(hash-table-set! t (car kv) (cdr kv)))
	      alst)
    t))

; in case something re-defs these native routines, bury them below and
; assign them.
(let ((equal-hash equal-hash)
      (make-hashtable make-hashtable)
      (hashtable-put! hashtable-put!)
      (hashtable-get  hashtable-get)
      (hashtable-map  hashtable-map)
      (hashtable-for-each hashtable-for-each))

  (set! make-hash-table (lambda rst (make-hashtable equal-hash assoc 997)))
  (set! hash-table-set! hashtable-put!)
  (set! hash-table-ref (lambda (t k . opt)
			 (cond ((hashtable-get t k))
			       (else (if (null? opt)
					 (error "key not in hash-table " t k)
					 (apply (car opt) '()))))))
  (set! hash-table-values (lambda (table)
			    (hashtable-map (lambda (k v) v) table)))
  (set! hash-table-keys (lambda (table)
			  (hashtable-map (lambda (k v) k) table)))
  (set! hash-table-walk hashtable-for-each))


; (parameterize ((current-require-path
;                 (cons "Asm/Intel/Sassy" 
;                       (current-require-path))))
;   (let ((r (lambda (x)
;              (begin (display `(require ,x)) (newline))
;              (require x))))
;     (r 'extras)
;     (r 'push-stacks-bv)
;     (r 'api)
;     (r 'intern)
;     (r 'macros)
;     (r 'numbers)
;     (parameterize ((current-require-path
;                     (cons "Asm/Intel/Sassy/other" 
;                           (current-require-path))))
;       (r 'srfi-56-pieces))
;     (r 'operands)
;     (r 'text-block)
;     (r 'opcodes)
;     (r 'text)
;     (r 'parse)
;     (r 'main)))
