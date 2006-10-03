; 2000-11-19 / not done yet
;
; A pickled file is a byte stream where each item is identified by a
; one-byte prefix code.
;
; (pickle object output-port) => unspecified
; (unpickle input-port) => object
;
; The implementation is biased in favor of pickling speed and makes one
; pass over the input.  Every object that has identity is assigned an
; ID on the fly and entered into a hash table.  This contrasts with 
; a two-pass strategy where only shared objects are given an ID.  A single
; pass will assign more IDs, and unpickling will require a larger table.

(define (pickle obj port)
  (let ((context (pkl/make-context port)))
    (pkl/pickle-obj obj context)))

(define (unpickle port)
  (let ((n (pkl/must-read-byte port)))
    (pkl/unpickle-obj n port)))

(define pkl/null        0)
(define pkl/char        1)
(define pkl/true        2)
(define pkl/false       3)
(define pkl/unspecified 4)
(define pkl/undefined   5)
(define pkl/eof         6)
(define pkl/fixnum      7)

(define pkl/vector      11)
(define pkl/rectnum     12)
(define pkl/ratnum      13)
(define pkl/symbol      14)
(define pkl/port        15)
(define pkl/structure   16)
(define pkl/environment 17)

(define pkl/bytevector  20)
(define pkl/string      21)
(define pkl/flonum      22)
(define pkl/compnum     23)
(define pkl/bignum      24)

(define pkl/pair        40)

(define pkl/definition  50)
(define pkl/reference   51)

; User-defined from 60 and up.

(define (pkl/make-context port)
  (list port
        (make-hashtable assq equal-hash)
        0))

(define (pkl/pickle-obj obj context)
  (let ((port (pkl/context-port context)))
    (cond ((null? obj)
           (pkl/write-byte pkl/null port))
          ((eq? obj #t)
           (pkl/write-byte pkl/true port))
          ((eq? obj #f)
           (pkl/write-byte pkl/false port))
          ((eq? obj (unspecified)) 
           (pkl/write-byte pkl/unspecified port))
          ((eq? obj (undefined))
           (pkl/write-byte pkl/undefined port))
          ((eq? obj (eof-object))
           (pkl/write-byte pkl/eof port))
          ((char? obj)  
           (pkl/write-char obj port))
          ((fixnum? obj)
           (pkl/write-byte pkl/fixnum port)
           (pkl/write-word f port))
          ((pair? obj)
           (pkl/write-object obj pkl/pickle-pair context pkl/pair))
          ((vector? obj)
           (pkl/write-object obj pkl/pickle-vector-like obj context 
                             pkl/vector))
          ((structure? obj)
           (pkl/write-object obj pkl/pickle-vector-like obj context
                             pkl/structure))
          ((environment? obj)
           (pkl/write-object obj pkl/pickle-vector-like obj context
                             pkl/environment))
          ((symbol? obj)
           (pkl/pickle-symbol obj port))
          ((bytevector? obj)
           (pkl/write-object obj pkl/pickle-bytevector-like obj context
                             pkl/string))
          ((string? obj)
           (pkl/write-object obj pkl/pickle-bytevector-like obj context
                             pkl/string))
          ((rectnum? obj)
           (pkl/pickle-vector-like obj context pkl/rectnum))
          ((ratnum? obj)
           (pkl/pickle-vector-like obj context pkl/ratnum))
          ((bignum? obj)
           (pkl/pickle-bytevector-like obj context pkl/bignum))
          ((flonum? obj)
           (pkl/pickle-bytevector-like obj context pkl/flonum))
          ((compnum? obj)
           (pkl/pickle-bytevector-like obj context pkl/compnum))
          ((port? obj)
           (error "Cannot pickle a port: " obj))
          ((procedure? obj)
           (error "Cannot pickle a procedure: " obj))
          (else
           (error "Unknown object: " obj)))))

(define (pkl/write-object obj pickler context tag)

  (define (lookup)
    (let ((probe (hashtable-fetch (pkl/context-hash context) obj)))
      (if probe
          (values probe #f)
          (let ((id (pkl/new-id context)))
            (hashtable-put! (pkl/context-hash context) obj id)
            (values id #t)))))

  (let-values (((id first-time?) (lookup)))
    (if first-time?
        (begin (pkl/write-byte pkl/definition (pkl/context-port context))
               (pkl/write-id id context)
               (pickler obj context tag))
        (begin (pkl/write-byte pkl/reference (pkl/context-port context))
               (pkl/write-id id context)))))

(define (pkl/pickle-pair pair context ignored)
  (pkl/write-byte pkl/pair (pkl/context-port context))
  (pkl/pickle-obj (car pair) context)
  (pkl/pickle-obj (cdr pair) context))

(define (pkl/pickle-symbol s context)
  (pkl/write-byte pkl/symbol (pkl/context-port context))
  (pkl/pickle-obj (symbol->string s) context))

(define (pkl/pickle-vector-like vec context tag)
  (pkl/write-byte tag (pkl/context-port context))
  (let ((len (vector-like-length vec)))
    (pkl/write-word len (pkl/context-port context))
    (do ((i 0 (+ i 1)))
        ((= i len))
      (pkl/pickle-obj (vector-like-ref vec i) context))))

(define (pkl/pickle-bytevector-like vec context tag)
  (pkl/write-byte tag (pkl/context-port context))
  (let ((len (bytevector-like-length vec)))
    (pkl/write-word len (pkl/context-port context))
    (do ((i 0 (+ i 1)))
        ((= i len))
      (pkl/write-byte (bytevector-like-ref vec i) context))))

(define (pkl/unpickle-obj n context)
  ((vector-ref unpicklers n) context))

(define unpicklers)

...

(let ((v (make-vector ...)))
  (vector-set! v pkl/pair unpickle-pair)
  ...
  (set! unpicklers v))
  
;; Larceny

(define (pkl/read-byte port)
  (let ((c (read-char port)))
    (if (eof-object? c)
        c
        (char->integer c))))

(define (pkl/write-byte b port)
  (write-char (integer->char b) port))

(define (pkl/write-char c port)
  (write-char c port))

(define (pkl/write-word w port)
  (do ((w w (rsha w 8))
       (i 4 (- i 1)))
      ((zero? i))
    (pkl/write-byte (logand w 255) port)))

; eof
