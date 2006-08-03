; For guile-1.7.91

(use-syntax (ice-9 syncase))
(require-extension (srfi 1 9 60))

(debug-set! stack 0)

(define old-make-hash-table make-hash-table)

(define (make-hash-table . arg)
  (old-make-hash-table 997))

(define (write-byte x . port) (apply write-char (integer->char x) port))
(define (read-byte . port)
  (let ((t (apply read-char port)))
    (if (eof-object? t)
	t
	(char->integer t))))

(define (hash-table-ref t k . th)
  (cond ((hash-ref t k))
	(else (if (null? t) #f ((car th))))))

(define hash-table-set! hash-set!)

(define (alist->hash-table lst)
  (let ((t (make-hash-table)))
    (for-each (lambda (itm)
		(hash-table-set! t (car itm) (cdr itm)))
	      lst)
    t))

(define (hash-table-values t)
  (hash-map->list (lambda (k v) v) t))
