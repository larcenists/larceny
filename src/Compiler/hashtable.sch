; Copyright 1999 William Clinger
;
; $Id$
;
; Hash tables for Twobit.
;
; FIXME:
;
; In the past, this file has been a near-copy of
; Lib/Common/hashtable.sch, but the separate copy is
; necessary so Twobit can run as a cross-compiler on
; systems other than Larceny.
;
; As Larceny converts to a more efficient implementation
; of R6RS hashtables, this file should become a portable
; (and therefore less efficient) implementation of R6RS
; hashtables.  When Larceny is built, this less efficient
; implementation should not replace Larceny's native
; implementation.
;
; Requires CALL-WITHOUT-INTERRUPTS.
; This code should be thread-safe provided VECTOR-REF is atomic.
;
; (make-hashtable <hash-function> <bucket-searcher> <size>)
;
;     Returns a newly allocated mutable hash table
;     using <hash-function> as the hash function
;     and <bucket-searcher>, e.g. ASSQ, ASSV, ASSOC, to search a bucket
;     with <size> buckets at first, expanding the number of buckets as needed.
;     The <hash-function> must accept a key and return a non-negative exact
;     integer.
;
; (make-hashtable <hash-function> <bucket-searcher>)
;
;     Equivalent to (make-hashtable <hash-function> <bucket-searcher> n)
;     for some value of n chosen by the implementation.
;
; (make-hashtable <hash-function>)
;
;     Equivalent to (make-hashtable <hash-function> assv).
;
; (make-hashtable)
;
;     Equivalent to (make-hashtable object-hash assv).
;
; (hashtable-contains? <hashtable> <key>)
;
;     Returns true iff the <hashtable> contains an entry for <key>.
;
; (hashtable-fetch <hashtable> <key> <flag>)
;
;     Returns the value associated with <key> in the <hashtable> if the
;     <hashtable> contains <key>; otherwise returns <flag>.
;
; (hashtable-get <hashtable> <key>)
;
;     Equivalent to (hashtable-fetch <hashtable> <key> #f)
;
; (hashtable-put! <hashtable> <key> <value>)
;
;     Changes the <hashtable> to associate <key> with <value>, replacing
;     any existing association for <key>.
;
; (hashtable-remove! <hashtable> <key>)
;
;     Removes any association for <key> within the <hashtable>.
;
; (hashtable-clear! <hashtable>)
;
;     Removes all associations from the <hashtable>.
;
; (hashtable-size <hashtable>)
;
;     Returns the number of keys contained within the <hashtable>.
;
; (hashtable-for-each <procedure> <hashtable>)
;
;     The <procedure> must accept two arguments, a key and the value
;     associated with that key.  Calls the <procedure> once for each
;     key-value association.  The order of these calls is indeterminate.
;
; (hashtable-map <procedure> <hashtable>)
;
;     The <procedure> must accept two arguments, a key and the value
;     associated with that key.  Calls the <procedure> once for each
;     key-value association, and returns a list of the results.  The
;     order of the calls is indeterminate.
;
; (hashtable-copy <hashtable> <mutable>)
;
;     Returns a copy of the <hashtable>.

; FIXME: temporary hacks so we can tell which implementation of
; hashtables is active.

(define (hashtable-implementation) 'twobit)

; Larceny's old-style hashtables are now deprecated.

(define (make-hashtable . args)
  (display "WARNING: delegating to make-r6rs-hashtable;")
  (newline)
  (display "    for Larceny's old hashtables, call make-oldstyle-hashtable")
  (newline)
  (apply make-r6rs-hashtable args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIXME: temporary implementation of R6RS hashtables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-r6rs-hashtable hash equiv? . rest)
  (define (searcher key bucket)
    (assp (lambda (x) (equiv? key x)) bucket))
  (cond ((null? rest)
         (make-oldstyle-hashtable hash searcher))
        (else
         (make-oldstyle-hashtable hash searcher (car rest)))))

(define (make-eq-hashtable . rest)
  (cond ((null? rest)
         (make-oldstyle-hashtable object-hash assq))
        (else
         (make-oldstyle-hashtable object-hash assq (car rest)))))

(define (make-eqv-hashtable . rest)
  (cond ((null? rest)
         (make-oldstyle-hashtable object-hash assv))
        (else
         (make-oldstyle-hashtable object-hash assv (car rest)))))

(define (hashtable-ref ht key default)
  (hashtable-fetch ht key default))

(define (hashtable-set! ht key value)
  (hashtable-put! ht key value))

(define (hashtable-delete! ht key)
  (hashtable-remove! ht key))

(define (hashtable-update! ht key proc default)
  (let ((x (proc (hashtable-ref ht key default))))
    (hashtable-set! ht key x)))

(define (hashtable-keys ht)
  (list->vector (hashtable-map (lambda (key value) key) ht)))

(define (hashtable-entries ht)
  (let ((entries (hashtable-map cons ht)))
    (values (list->vector (map car entries))
            (list->vector (map cdr entries)))))

; FIXME: The inspection procedures aren't implemented yet.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Larceny's old-style hashtables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; These global variables are assigned new values later.

(define make-oldstyle-hashtable      (lambda args '*))
(define hashtable?          (lambda (arg) #f))
(define hashtable-contains? (lambda (ht key) #f))
(define hashtable-fetch     (lambda (ht key flag) flag))
(define hashtable-get       (lambda (ht key) (hashtable-fetch ht key #f)))
(define hashtable-put!      (lambda (ht key val) '*))
(define hashtable-remove!   (lambda (ht key) '*))
(define hashtable-clear!    (lambda (ht) '*))
(define hashtable-size      (lambda (ht) 0))
(define hashtable-for-each  (lambda (ht proc) '*))
(define hashtable-map       (lambda (ht proc) '()))
(define hashtable-copy      (lambda (ht mutable?) ht))

; Implementation.
; A hashtable is represented as a vector of the form
;
;     #(("HASHTABLE") <count> <hasher> <searcher> <buckets>)
;
; where <count> is the number of associations within the hashtable,
; <hasher> is the hash function, <searcher> is the bucket searcher,
; and <buckets> is a vector of buckets.
;
; The <hasher> and <searcher> fields are constant, but
; the <count> and <buckets> fields are mutable.
;
; For thread-safe operation, the mutators must modify both
; as an atomic operation.  Other operations do not require
; critical sections provided VECTOR-REF is an atomic operation
; and the operation does not modify the hashtable, does not
; reference the <count> field, and fetches the <buckets>
; field exactly once.

(let ((doc      (list "HASHTABLE"))
      (count    (lambda (ht)   (vector-ref ht 1)))
      (count!   (lambda (ht n) (vector-set! ht 1 n)))
      (hasher   (lambda (ht)   (vector-ref ht 2)))
      (searcher (lambda (ht)   (vector-ref ht 3)))
      (buckets  (lambda (ht)   (vector-ref ht 4)))
      (buckets! (lambda (ht v) (vector-set! ht 4 v)))
      (defaultn 10))
  (let ((%hashtable? (lambda (ht)
                      (and (vector? ht)
                           (= 5 (vector-length ht))
                           (eq? doc (vector-ref ht 0)))))
        (hashtable-error (lambda (procedure x)
                           (display "ERROR: ")
                           (display procedure)
                           (display ": Bad hash table: ")
                           (newline)
                           (write x)
                           (newline))))

    (define (guarantee-hashtable procedure object)
      (if (not (%hashtable? object))
          (begin (hashtable-error procedure object)
                 #t)))

    ; Internal operations.
    
    (define (make-ht hashfun searcher size)
      (vector doc 0 hashfun searcher (make-vector size '())))
    
    ; Substitute x for the first occurrence of y within the list z.
    ; y is known to occur within z.
    
    (define (substitute1 x y z)
      (cond ((eq? y (car z))
             (cons x (cdr z)))
            (else
             (cons (car z)
                   (substitute1 x y (cdr z))))))
    
    ; Remove the first occurrence of x from y.
    ; x is known to occur within y.
    
    (define (remq1 x y)
      (cond ((eq? x (car y))
             (cdr y))
            (else
             (cons (car y)
                   (remq1 x (cdr y))))))
    
    (define (resize ht0)
      (call-without-interrupts
       (lambda ()
         (let ((ht (make-ht (hasher ht0)
                            (searcher ht0)
                            (+ 1 (* 2 (count ht0))))))
           (ht-for-each (lambda (key val)
                          (put! ht key val))
                        ht0)
           (buckets! ht0 (buckets ht))))))
    
    ; Returns the contents of the hashtable as a vector of pairs.
    
    (define (contents ht)
      (let* ((v (buckets ht))
             (n (vector-length v))
             (z (make-vector (count ht) '())))
        (define (loop i bucket j)
          (cond ((pair? bucket)
                 (let ((entry (car bucket)))
                   (vector-set! z j (cons (car entry) (cdr entry)))
                   (loop i
                         (cdr bucket)
                         (+ j 1))))
                ((null? bucket)
                 (if (= i n)
                     (if (= j (vector-length z))
                         z
                         (begin (display "BUG in hashtable")
                                (newline)
                                '#()))
                     (loop (+ i 1)
                           (vector-ref v i)
                           j)))
                (else (error "Illegal hashtable structure."))))
        (loop 0 '() 0)))
    
    (define (contains? ht key)
      (guarantee-hashtable 'contains? ht)
      (let* ((v (buckets ht))
             (n (vector-length v))
             (h (remainder ((hasher ht) key) n))
             (b (vector-ref v h)))
        (if ((searcher ht) key b)
            #t
            #f)))

    (define (fetch ht key flag)
      (guarantee-hashtable 'fetch ht)
      (let* ((v (buckets ht))
             (n (vector-length v))
             (h (remainder ((hasher ht) key) n))
             (b (vector-ref v h))
             (probe ((searcher ht) key b)))
        (if (pair? probe)
            (cdr probe)
            flag)))

    (define (put! ht key val)
      (guarantee-hashtable 'put! ht)
      (call-without-interrupts
       (lambda ()
         (let* ((v (buckets ht))
                (n (vector-length v))
                (h (remainder ((hasher ht) key) n))
                (b (vector-ref v h))
                (probe ((searcher ht) key b)))
           (if probe
               ;; Using SET-CDR! on the probe would make it necessary
               ;; to synchronize the CONTENTS routine.
               (vector-set! v h (substitute1 (cons key val) probe b))
               (begin (count! ht (+ (count ht) 1))
                      (vector-set! v h (cons (cons key val) b))
                      (if (> (count ht) n)
                          (resize ht)))))
         #f)))

    (define (remove! ht key)
      (guarantee-hashtable 'remove! ht)
      (call-without-interrupts
       (lambda ()
         (let* ((v (buckets ht))
                (n (vector-length v))
                (h (remainder ((hasher ht) key) n))
                (b (vector-ref v h))
                (probe ((searcher ht) key b)))
           (if probe
               (begin (count! ht (- (count ht) 1))
                      (vector-set! v h (remq1 probe b))
                      (if (< (* 2 (+ defaultn (count ht))) n)
                          (resize ht))))
           #f))))

    (define (clear! ht)
      (guarantee-hashtable 'clear! ht)
      (call-without-interrupts
       (lambda ()
         (begin (count! ht 0)
                (buckets! ht (make-vector defaultn '()))
                #f))))

    (define (size ht)
      (guarantee-hashtable 'size ht)
      (count ht))

    ; This code must be written so that the procedure can modify the
    ; hashtable without breaking any invariants.
    
    (define (ht-for-each f ht)
      (guarantee-hashtable 'ht-for-each ht)
      (let* ((v (contents ht))
             (n (vector-length v)))
        (do ((j 0 (+ j 1)))
            ((= j n))
          (let ((x (vector-ref v j)))
            (f (car x) (cdr x))))))

    (define (ht-map f ht)
      (guarantee-hashtable 'ht-map ht)
      (let* ((v (contents ht))
             (n (vector-length v)))
        (do ((j 0 (+ j 1))
             (results '() (let ((x (vector-ref v j)))
                            (cons (f (car x) (cdr x))
                                  results))))
            ((= j n)
             (reverse results)))))

    (define (ht-copy ht)
      (guarantee-hashtable 'ht-copy ht)
      (let* ((newtable (make-oldstyle-hashtable (hasher ht) (searcher ht) 0))
             (v (buckets ht))
             (n (vector-length v))
             (newvector (make-vector n '())))
        (count! newtable (count ht))
        (buckets! newtable newvector)
        (do ((i 0 (+ i 1)))
            ((= i n))
          (vector-set! newvector i (append (vector-ref v i) '())))
        newtable))

    ; External entry points.
    
    (set! make-oldstyle-hashtable
          (lambda args
            (let* ((hashfun (if (null? args) object-hash (car args)))
                   (searcher (if (or (null? args) (null? (cdr args)))
                                 assv
                                 (cadr args)))
                   (size (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                             defaultn
                             (caddr args))))
              (make-ht hashfun searcher size))))
    (set! hashtable?          (lambda (object)      (%hashtable? object)))
    (set! hashtable-contains? (lambda (ht key)      (contains? ht key)))
    (set! hashtable-fetch     (lambda (ht key flag) (fetch ht key flag)))
    (set! hashtable-get       (lambda (ht key)      (fetch ht key #f)))
    (set! hashtable-put!      (lambda (ht key val)  (put! ht key val)))
    (set! hashtable-remove!   (lambda (ht key)      (remove! ht key)))
    (set! hashtable-clear!    (lambda (ht . rest)   (clear! ht)))   ; FIXME
    (set! hashtable-size      (lambda (ht)          (size ht)))
    (set! hashtable-for-each  (lambda (ht proc)     (ht-for-each ht proc)))
    (set! hashtable-map       (lambda (ht proc)     (ht-map ht proc)))
    (set! hashtable-copy      (lambda (ht . rest)   (ht-copy ht)))  ; FIXME
    #f))

; eof
