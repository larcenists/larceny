;;; Copyright 2015 William D Clinger.
;;;
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright and permission notice in full.
;;;
;;; I also request that you send me a copy of any improvements that you
;;; make to this software so that they may be incorporated within it to
;;; the benefit of the Scheme community.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This R7RS code implements (rnrs hashtables) on top of SRFI 69.

;;; Private stuff.

;;; Although SRFI 69 is mostly written as though hash functions take
;;; just one argument, its reference implementation routinely passes
;;; a second argument to hash functions, and that arguably incorrect
;;; behavior has undoubtedly found its way into many implementations
;;; of SRFI 69.
;;;
;;; A unary hash function passed to R6RS make-hashtable is therefore
;;; unlikely to work when passed to SRFI 69 make-hash-table.  We need
;;; to convert the unary hash function so it will accept a second
;;; optional argument, and we also need to arrange for the original
;;; unary hash function to be returned by hashtable-hash-function.
;;;
;;; We'd like to accomplish this while preserving interoperability
;;; between R6RS hashtables and SRFI 69 hash tables.  That argues
;;; against implementing R6RS hashtables by records that encapsulate
;;; a SRFI 69 hash table, which would otherwise be the easy way to
;;; go about this.
;;;
;;; This association list implements a bidirectional mapping between
;;; one-argument hash functions of R6RS and their representations as
;;; two-argument hash functions that will work with SRFI 69.

(define table-of-hash-functions '())

;;; Given a unary hash function, returns a hash function that will
;;; be acceptable to SRFI 69.

(define (make-srfi-69-hash-function hash-function)
  (lambda (x . rest)
    (if (null? rest)
        (hash-function x)
        (modulo (hash-function x) (car rest)))))

(define (r6rs->srfi69 hash-function)
  (let ((probe (assoc hash-function table-of-hash-functions)))
    (if probe
        (cdr probe)
        (let ((hasher (make-srfi-69-hash-function hash-function)))
          (set! table-of-hash-functions
                (cons (cons hash-function hasher)
                      table-of-hash-functions))
          hasher))))

(define (srfi69->r6rs hasher)
  (define (loop table)
    (cond ((null? table)
           hasher)
          ((equal? hasher (cdr (car table)))
           (car (car table)))
          (else
           (loop (cdr table)))))
  (loop table-of-hash-functions))  

;;; SRFI 69 doesn't define a hash function that's suitable for use
;;; with the eqv? predicate, and we need one for make-eqv-hashtable.
;;;
;;; The R7RS eqv? predicate behaves the same as eq? for these types:
;;;
;;;     symbols
;;;     booleans
;;;     empty list
;;;     pairs
;;;     records
;;;     non-empty strings
;;;     non-empty vectors
;;;     non-empty bytevectors
;;;
;;; eqv? might behave differently when its arguments are:
;;; 
;;;     procedures that behave the same but have equal location tags
;;;     numbers
;;;     characters
;;;     empty strings
;;;     empty vectors
;;;     empty bytevectors
;;; 
;;; If eqv? and eq? behave differently on two arguments x and y,
;;; eqv? returns true and eq? returns false.
;;; 
;;; FIXME: There is no portable way to define a good hash function
;;; that's compatible with eqv? on procedures and also runs in
;;; constant time.  This one is compatible with eqv? and runs in
;;; constant time (on procedures), but isn't any good.

;;; The main thing these numerical constants have in common is that
;;; they're positive and fit in 24 bits.

(define hash:procedure         9445898)
(define hash:character        13048478)
(define hash:empty-string     14079236)
(define hash:empty-vector      1288342)
(define hash:empty-bytevector 11753202)
(define hash:inexact           1134643)
(define hash:infinity+         2725891)
(define hash:infinity-         5984233)
(define hash:nan               7537847)
(define hash:complex           9999245)

(define (hash-for-eqv x)
  (cond ((procedure? x)
         hash:procedure)
        ((number? x)
         (cond ((exact-integer? x)
                x)
               ((not (real? x))
                (+ hash:complex (complex-hash x)))
               ((exact? x)
                (+ (numerator x) (denominator x)))
               (else
                (+ hash:inexact (inexact-hash x)))))
        ((char? x)
         (+ hash:character (char->integer x)))
        ((eqv? x "")
         hash:empty-string)
        ((eqv? x '#())
         hash:empty-vector)
        ((eqv? x '#u8())
         hash:empty-bytevector)
        (else
         (hash-by-identity x))))

;;; The R6RS distinguishes mutable from immutable hashtables,
;;; so we have to keep track of that somehow.  Here we remember
;;; all of the immutable hashtables within a SRFI 69 hash-table.
;;;
;;; FIXME: That means the storage occupied by an immutable
;;; hashtable won't be reclaimed if it becomes otherwise
;;; inaccessible.

(define immutable-hashtables
  (make-hash-table eqv? (r6rs->srfi69 hash-table-size)))

(define (complain-if-immutable ht complainant)
  (if (hash-table-ref/default immutable-hashtables ht #f)
      (error (string-append (symbol->string complainant)
                            ": hashtable is immutable")
             ht)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exported procedures.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The R6RS make-eq-hashtable procedure is normally called with
;;; no arguments, but an optional argument specifies the initial
;;; capacity of the table.  That optional argument, if present,
;;; will be ignored by this implementation because it has no
;;; counterpart in SRFI 69.

(define (make-eq-hashtable . rest)
  (make-hash-table eq? hash-by-identity))

(define (make-eqv-hashtable . rest)
  (make-hash-table eqv? (r6rs->srfi69 hash-for-eqv)))

;;; As with make-eq-hashtable and make-eqv-hashtable, the optional
;;; initial capacity will be ignored.

(define (make-hashtable hash-function equiv . rest)
  (make-hash-table equiv (r6rs->srfi69 hash-function)))

(define (hashtable? x)
  (hash-table? x))

(define (hashtable-size ht)
  (hash-table-size ht))

(define (hashtable-ref ht key default)
  (hash-table-ref/default ht key default))

(define (hashtable-set! ht key obj)
  (complain-if-immutable ht 'hashtable-set!)
  (hash-table-set! ht key obj))

(define (hashtable-delete! ht key)
  (complain-if-immutable ht 'hashtable-delete!)
  (hash-table-delete! ht key))

(define (hashtable-contains? ht key)
  (hash-table-exists? ht key))

(define (hashtable-update! ht key proc default)
  (complain-if-immutable ht 'hashtable-update!)
  (hash-table-set! ht
                   key
                   (proc (hash-table-ref/default ht key default))))

;;; By default, hashtable-copy returns an immutable hashtable.
;;; The copy is mutable only if a second argument is passed and
;;; that second argument is true.

(define (hashtable-copy ht . rest)
  (let ((mutable? (and (pair? rest) (car rest)))
        (the-copy (hash-table-copy ht)))
    (if (not mutable?)
        (hash-table-set! immutable-hashtables the-copy #t))
    the-copy))

;;; As usual, the optional "initial" capacity is ignored.

(define (hashtable-clear! ht . rest)
  (complain-if-immutable ht 'hashtable-update!)
  (hash-table-walk ht
                   (lambda (key value)
                     (hash-table-delete! ht key))))

(define (hashtable-keys ht)
  (list->vector (hash-table-keys ht)))

(define (hashtable-entries ht)
  (let* ((keys (hashtable-keys ht))
         (vals (vector-map (lambda (key)
                             (hash-table-ref ht key))
                           keys)))
    (values keys vals)))

(define (hashtable-equivalence-function ht)
  (hash-table-equivalence-function ht))

(define (hashtable-hash-function ht)
  (srfi69->r6rs (hash-table-hash-function ht)))

(define (hashtable-mutable? ht)
  (not (hash-table-ref/default immutable-hashtables ht #f)))

(define (equal-hash obj)
  (hash obj))

;;; string-hash is exported by SRFI 69.
;;; string-ci-hash is exported by SRFI 69.

(define (r6rs:symbol-hash sym)
  (hash-by-identity sym))

