; Symbol table management for Larceny, based on same from MacScheme.
; Parts of this code is copyright 1991 lightship software
;
; $Id$
;
; symbol?
; symbol->string
; string->symbol
; make-symbol
; string-hash
; symbol-hash
; namespace
; namespace-set!
; install-symbols (used only for bootstrapping)
;
; In macscheme, symbols are represented as vector-like structures
; with a structure tag of -16:
;
;     element 0    -16
;             1    string (print name)
;             2    fixnum (hash code)
;             3    list (property list, unused by macscheme)
;
; In larceny, a symbol is a vector-like structure with a typetag given by
; the value of `symbol-typetag' (below).
;
; The namespace procedure takes no arguments, and returns a list of
; all symbols appearing in the symbol table.
;
; namespace-set! takes two arguments:
;
;       list of symbols with which to fill the symbol table
;       number of buckets for symbol table
;
; the namespace-set! procedure takes a list of symbols and a table size
; and replaces the current symbol table with those symbols.


;-----------------------------------------------------------------------------
; IMPLEMENTATION-DEPENDENT

; These are for Larceny.

(define symbol-typetag ???)

(define (make-symbol-structure string hash props)
  (let ((v (make-vector string hash props)))
    (typetag-set! v symbol-typetag)
    v))

(define (symbol.printname s) (vector-like-ref s 0))
(define (symbol.hashname s) (vector-like-ref s 1))
(define (symbol.proplist s) (vector-like-ref s 2))

(define (symbol.proplist! s p) (vector-like-set! s 2 p))


; These are for MacScheme
;
; (define (make-symbol-structure string hash props)
;   (->structure (vector -16 string hash props)))
;
; (define (symbol.printname s) (vector-ref (->vector s) 1))
; (define (symbol.hashname s)  (vector-ref (->vector s) 2))
; (define (symbol.proplist s)  (vector-ref (->vector s) 3))
;
; (define (symbol.proplist! s p) (vector-set! (->vector s) 3 p))

;-----------------------------------------------------------------------------


; symbol? is integrable.
; For MacScheme, it could have been written as
;
; (define symbol?
;   (lambda (x)
;     (and (structure? x)
;          (eq? (symbol-ref x 0) -16))))
;
; and for Larceny:
;
;  (define (symbol? x) (and (vector-like? x) (= (typetag x) symbol-typetag)))

(define (symbol? x) (symbol? x)))

(define symbol->string
  (lambda (symbol)
    (if (symbol? symbol)
        (symbol.printname symbol)
        (error "non-symbol -- symbol->string" symbol))))

; given a string, return a new uninterned symbol.

(define make-symbol
  (lambda (string)
    (if (string? string)
	(make-symbol-structure (string-copy string) (string-hash string) '())
        (error "non-string -- make-symbol" string))))

; with the following definitions,
;
;    (= (string-hash s) (symbol-hash (string->symbol s)))
;
; is always true.  this is convenient for the string->symbol
; procedure, but is it otherwise desirable?

(define string-hash
  (letrec ((loop (lambda (s i h)
                   (if (negative? i)
                       h
                       (loop s
                             (- i 1)
                             (logand 65535
                                     (+ (char->integer
                                         (string-ref s i))
                                        h h h)))))))
    (lambda (string)
      (let ((n (string-length string)))
        (loop string (- n 1) n)))))

(define symbol-hash
  (lambda (symbol)
    (if (symbol? symbol)
        (symbol.hashname symbol)
        0)))

; these definitions can be commented out so this file can be loaded
; into a running scheme system without breaking the reader.

(define string->symbol #f)
(define namespace #f)
(define namespace-set! #f)

(define (install-symbols oblist tablesize)
  (let ((obvector (make-vector tablesize '())))
      
    (letrec
        (
         
	 ; given a string, interns it.
         
         (intern
          (lambda (s)
            (optimize speed)
            (let ((h (string-hash s)))
              (call-without-interrupts
               (lambda ()
                 (or (search-bucket
                      s
                      h
                      (vector-ref obvector 
                                  (- h (* tablesize
                                          (quotient h tablesize)))))
                     (install-symbol (make-symbol s) h obvector)))))))
         
         (search-bucket
          (lambda (s h bucket)
            (optimize speed)
            (if (null? bucket)
                #f
                (let ((symbol (car bucket)))
                  (if (and (eq? h (symbol-hash symbol))
                           (string=? s (symbol->string symbol)))
                      symbol
                      (search-bucket s h (cdr bucket)))))))
         
	 ; given a symbol, adds it to the obvector, whether a symbol
	 ; with the same pname (or even the same symbol) is already
	 ; there or not.
         
         (install-symbol
          (lambda (s h obvector)
            (optimize speed)
            (let ((i (remainder h (vector-length obvector))))
              (vector-set! obvector i (cons s (vector-ref obvector i)))
              s)))
         
         )
        
      (set! string->symbol
	    (lambda (s) (intern s)))
         
      ; Returns a list of all symbols in the obvector.
        
      (set! namespace
	    (lambda ()
	      (optimize space)
	      (letrec ((loop
			(lambda (i l)
			  (if (< i 0)
			      l
			      (loop (- i 1)
				    (append (vector-ref obvector i) l))))))
		(loop (- tablesize 1) '()))))
        
      (set! namespace-set!
	    (lambda (symbols new-tablesize)
	      (optimize space)
	      (set! tablesize new-tablesize)
	      (let ((v (make-vector tablesize '())))
		(for-each (lambda (s)
			    (if (symbol? s)
				(install-symbol s (symbol-hash s) v)))
			  symbols)
		(set! obvector v)
		#t)))
        
      ; Initialize obvector
        
      (namespace-set! oblist tablesize)
      
      ; Forget the oblist so the garbage collector can reclaim
      ; its storage.
        
      (set! oblist '())
        
      (set! install-symbols #f)
        
      #t))))

