; -*- Scheme -*-
;
; Symbol table management for Larceny, based on same from MacScheme.
; Parts of this code is copyright 1991 lightship software
;
; $Id: oblist.sch,v 1.4 1992/05/15 22:18:12 lth Exp $
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

(define (make-symbol-structure string hash props)
  (let ((v (vector string hash props)))
    (typetag-set! v sys$tag.symbol-typetag)
    v))

(define (symbol.printname s) (vector-like-ref s 0))
(define (symbol.hashname s) (vector-like-ref s 1))
(define (symbol.proplist s) (vector-like-ref s 2))

(define (symbol.hashname! s h) (vector-like-set! s 1 h))
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
;
; COMMON

(define symbol? (lambda (x) (symbol? x)))   ; integrable

(define symbol->string
  (lambda (symbol)
    (if (symbol? symbol)
        (symbol.printname symbol)
        (begin ; (break)
	       (error "non-symbol -- symbol->string" symbol)))))

; Given a string, return a new uninterned symbol.

(define make-symbol
  (lambda (string)
    (if (string? string)
	(make-symbol-structure (string-copy string) (string-hash string) '())
	(error "non-string -- make-symbol" string))))

;; Property lists. What we don't do for some backward compatibility.

(define (putprop sym name value)
  (if (not (symbol? sym))
      (error "putprop:" sym "is not a symbol.")
      (let ((plist (symbol.proplist sym)))
	(let ((probe (assq name plist)))
	  (if probe
	      (set-cdr! probe value)
	      (symbol.proplist! sym (cons (cons name value) plist)))))))

(define (getprop sym name)
  (if (not (symbol? sym))
      (error "getprop:" sym "is not a symbol.")
      (let ((plist (symbol.proplist sym)))
	(let ((probe (assq name plist)))
	  (if probe 
	      (cdr probe) 
	      #f)))))

(define (remprop sym name)
  (if (not (symbol? sym))
      (error "remprop:" sym "is not a symbol.")
      (symbol.proplist! sym (remq name (symbol.proplist sym)))))


; With the following definitions,
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
	(error "symbol-hash: " symbol " is not a symbol."))))

(define string->symbol #f)
(define namespace #f)
(define namespace-set! #f)


; The oblist is a list of symbols all with 0 as their hash value.

(define (install-symbols oblist tablesize)

  (define obvector #f)
      
  ; Given a string, interns it.

  (define (intern s)
    (let ((h (string-hash s)))
      (call-without-interrupts
       (lambda ()
	 (or (search-bucket
	      s
	      h
	      (vector-ref obvector (remainder h (vector-length obvector))))
	     (install-symbol (make-symbol s) h obvector))))))

  (define (search-bucket s h bucket)
    (if (null? bucket)
	#f
	(let ((symbol (car bucket)))
	  (if (and (eq? h (symbol-hash symbol))
		   (string=? s (symbol->string symbol)))
	      symbol
	      (search-bucket s h (cdr bucket))))))
         
  ; Given a symbol, adds it to the obvector, whether a symbol
  ; with the same pname (or even the same symbol) is already
  ; there or not.
         
  (define (install-symbol s h obvector)
    (let ((i (remainder h (vector-length obvector))))
      (symbol.hashname! s h)
      (vector-set! obvector i (cons s (vector-ref obvector i)))
      s))
         
  (define (%string->symbol s)
    (if (string? s)
	(intern s)
	(error "String->symbol: " s " is not a string.")))
         
  (define (%namespace)
    (letrec ((loop
	      (lambda (i l)
		(if (< i 0)
		    l
		    (loop (- i 1)
			  (append (vector-ref obvector i) l))))))
      (loop (- tablesize 1) '())))
        
  (define (%namespace-set! symbols new-tablesize)
    (set! tablesize new-tablesize)
    (let ((v (make-vector tablesize '())))
      (for-each 
       (lambda (s)
	 (if (symbol? s)
	     (install-symbol s (string-hash (symbol->string s)) v)
	     (error "namespace-set!: " s " is not a symbol.")))
       symbols)
      (set! obvector v)
      #t))

  ; Initialize obvector
    
  (%namespace-set! oblist tablesize)

  ; Globals

  (set! string->symbol %string->symbol)
  (set! namespace %namespace)
  (set! namespace-set! %namespace-set!)

  ; Forget the oblist so the garbage collector can reclaim its storage.

  (set! oblist '())
  (set! install-symbols #f)
        
  #t)


