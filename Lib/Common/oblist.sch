; Copyright 1991 Lightship Software, Incorporated.
;
; $Id$
;
; Larceny library -- symbol table management.
;
; A symbol is a vector-like structure with typetag sys$tag.symbol-typetag.
; It has three fields: the print name, the hash code, and the property list.
;
; symbol? is integrable (see Lib/primops.sch).

($$trace "oblist")

; Private variables.

(define *obvector* #f)			; the hash table -- a vector.
(define *symbol-count* 0)		; number of symbols in the table.
(define *oblist-ratio* 2)               ; divisor to compute watermark.
(define *oblist-watermark* 0)           ; watermark.


; Public procedures.

(define (symbol->string sym)
  (if (symbol? sym)
      (symbol.printname sym)
      (begin (error "symbol->string: " sym " is not a symbol.")
	     #t)))

(define (string->symbol s)
  (if (string? s)
      (intern s)
      (begin (error "string->symbol: " s " is not a string.")
	     #t)))

(define (symbol-hash sym)
  (if (symbol? sym)
      (symbol.hashname sym)
      (begin (error "symbol-hash: " sym " is not a symbol.")
	     #t)))

(define (oblist)
  (call-without-interrupts
    (lambda ()
      (define (loop i l)
	(if (< i 0)
	    l
	    (loop (- i 1) (append (vector-ref *obvector* i) l))))
      (loop (- (vector-length *obvector*) 1) '()))))
      
(define (oblist-set! symbols . rest)
  (let ((tablesize
	 (cond ((null? rest) (* (+ *oblist-ratio* 1) (length symbols)))
	       ((null? (cdr rest)) (car rest))
	       (else (error "oblist-set!: too many arguments.")))))
    (call-without-interrupts
      (lambda ()
	(let ((v (make-vector tablesize '())))
	  (do ((symbols symbols (cdr symbols))
	       (i 0 (+ i 1)))
	      ((null? symbols)
	       (set! *obvector* v)
	       (set! *symbol-count* i)
	       (set! *oblist-watermark* (quotient tablesize *oblist-ratio*))
	       (unspecified))
	    (let ((s (car symbols)))
	      (if (symbol? s)
		  (let ((h (string-hash (symbol.printname s))))
		    (symbol.hashname! s h)
		    (install-symbol s v))
		  (begin (error "oblist-set!: " s " is not a symbol.")
			 #t)))))))))

(define gensym
  (let ((n 1000))
    (lambda (x)
      (call-without-interrupts
	(lambda ()
	  (set! n (+ n 1))
	  (make-uninterned-symbol (string-append x (number->string n))))))))

(define (putprop sym name value)
  (if (not (symbol? sym))
      (begin (error "putprop: " sym " is not a symbol.")
	     #t)
      (call-without-interrupts
	(lambda ()
	  (let ((plist (symbol.proplist sym)))
	    (let ((probe (assq name plist)))
	      (if probe
		  (set-cdr! probe value)
		  (symbol.proplist! sym (cons (cons name value) plist)))))))))

(define (getprop sym name)
  (if (not (symbol? sym))
      (begin (error "getprop: " sym " is not a symbol.")
	     #t)
      (call-without-interrupts
	(lambda ()
	  (let ((plist (symbol.proplist sym)))
	    (let ((probe (assq name plist)))
	      (if probe 
		  (cdr probe) 
		  #f)))))))

(define (remprop sym name)
  (if (not (symbol? sym))
      (begin (error "remprop: " sym " is not a symbol.")
	     #t)
      (call-without-interrupts
	(lambda ()
	  (symbol.proplist! sym (remq name (symbol.proplist sym)))))))


; Private procedures.

; Symbol data structure.

(define (make-symbol string hash props)
  (let ((v (vector string hash props)))
    (typetag-set! v sys$tag.symbol-typetag)
    v))

(define (symbol.printname s) (vector-like-ref s 0))
(define (symbol.hashname s) (vector-like-ref s 1))
(define (symbol.proplist s) (vector-like-ref s 2))

(define (symbol.hashname! s h) (vector-like-set! s 1 h))
(define (symbol.proplist! s p) (vector-like-set! s 2 p))

(define (make-uninterned-symbol s)
  (if (string? s)
      (make-symbol (string-copy s) (string-hash s) '())
      (begin (error "make-uninterned-symbol: " s " is not a string.")
	     #t)))

; Given a string, interns it in the current obvector, updating the
; count of symbols in the vector.

(define (intern s)

  (define (search-bucket bucket)
    (if (null? bucket)
	#f
	(let ((symbol (car bucket)))
	  (if (string=? s (symbol.printname symbol))
	      symbol
	      (search-bucket (cdr bucket))))))

  (call-without-interrupts
    (lambda ()
      (let* ((h     (string-hash s))
	     (probe (search-bucket
		     (vector-ref *obvector*
				 (remainder h (vector-length *obvector*))))))
	(if probe
	    probe
	    (let ((s (install-symbol (make-symbol (string-copy s) h '())
				     *obvector*)))
	      (set! *symbol-count* (+ *symbol-count* 1))
	      (if (> *symbol-count* *oblist-watermark*)
		  (oblist-set! (oblist) (* (vector-length *obvector*) 2)))
	      s))))))


; Given a symbol, adds it to the given obvector, whether a symbol with the 
; same pname (or even the same symbol) is already there or not.
;
; Must run in critical section!
         
(define (install-symbol s obvector)
  (let ((i (remainder (symbol.hashname s) (vector-length obvector))))
    (vector-set! obvector i (cons s (vector-ref obvector i)))
    s))

; eof
