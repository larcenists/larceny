; Copyright 1999 William Clinger
; Extensively modified in 2007 by William Clinger
;
; $Id$
;
; Hash tables.
; Requires vector-like-cas! and .internal:machine-address.
; This code should be thread-safe provided VECTOR-REF is atomic.

($$trace "hashtable")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS hashtable API.
; (For details, see R6RS library chapter 13.)
;
; (make-eq-hashtable)
; (make-eq-hashtable k)
;
; (make-eqv-hashtable)
; (make-eqv-hashtable k)
;
; (make-hashtable hasher equiv)
; (make-hashtable hasher equiv k)
;
; (hashtable? x)
; (hashtable-size ht)
; (hashtable-ref ht key x)
; (hashtable-set! ht key x)
; (hashtable-delete! ht key)
; (hashtable-contains? ht key)
; (hashtable-update! ht key proc x)
; (hashtable-copy ht)
; (hashtable-copy ht mutable)
; (hashtable-clear! ht)
; (hashtable-clear! ht k)
; (hashtable-keys ht)
; (hashtable-entries ht)
;
; (hashtable-equivalence-function ht)
; (hashtable-hash-function ht)
; (hashtable-mutable? ht)
;
; (equal-hash x)
; (string-hash s)
; (string-ci-hash s)
; (symbol-hash sym)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Larceny extends the R6RS API internally with:
;
; (hashtable-reset! ht)
;
; This procedure forces the gc-sensitive parts of ht to be
; rehashed on its next access.
;
; (reset-all-hashtables!)
;
; This procedure resets every eq? and eqv? hashtable that
; exists within the heap.  If this procedure is called just
; before a heap is dumped, then any eq? and eqv? hashtables
; that may be present within the dumped heap will continue
; to work properly.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Larceny's oldstyle hashtables.
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
; (hashtable-copy <hashtable>)
;
;     Returns a copy of the <hashtable>.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME: temporary hack so we can tell which implementation of
; hashtables is active.

(define (hashtable-implementation) 'Lib/Common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIXME: temporary implementation of make-hashtable.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Larceny's old-style hashtables are now deprecated.

(define (make-hashtable . args)
;  (display "WARNING: delegating to make-r6rs-hashtable;")
;  (newline)
;  (display "    for Larceny's old hashtables, call make-oldstyle-hashtable")
;  (newline)
  (apply make-r6rs-hashtable args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIXME: this could be faster.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hashtable-update! ht key proc default)
  (let ((x (proc (hashtable-ref ht key default))))
    (hashtable-set! ht key x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Larceny's old-style hashtable API.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hashtable-fetch ht key default)
  (hashtable-ref ht key default))

(define (hashtable-get ht key)
  (hashtable-ref ht key #f))

(define (hashtable-put! ht key value)
  (hashtable-set! ht key value))

(define (hashtable-remove! ht key)
  (hashtable-delete! ht key))

(define (hashtable-for-each proc ht)
  (call-with-values
   (lambda () (hashtable-entries ht))
   (lambda (keys vals)
     (do ((n (vector-length keys))
          (i 0 (+ i 1)))
         ((= i n)
          (unspecified))
       (proc (vector-ref keys i) (vector-ref vals i))))))

(define (hashtable-map proc ht)
  (call-with-values
   (lambda () (hashtable-entries ht))
   (lambda (keys vals)
     (vector->list (vector-map proc keys vals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS hashtables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; These global variables are assigned new values later.

(define make-eq-hashtable       (lambda args '*))
(define make-eqv-hashtable      (lambda args '*))
(define make-r6rs-hashtable     (lambda args '*))
(define make-oldstyle-hashtable (lambda args '*))
(define hashtable?              (lambda (arg) #f))
(define hashtable-contains?     (lambda (ht key) #f))
(define hashtable-ref           (lambda (ht key flag) flag))
(define hashtable-set!          (lambda (ht key val) '*))
(define hashtable-delete!       (lambda (ht key) '*))
(define hashtable-clear!        (lambda (ht . rest) '*))
(define hashtable-size          (lambda (ht) 0))
(define hashtable-keys          (lambda (ht) '()))
(define hashtable-entries       (lambda (ht) (values '() '())))
(define hashtable-copy          (lambda (ht . rest) ht))

(define hashtable-equivalence-function (lambda (ht) equal?))
(define hashtable-hash-function        (lambda (ht) equal-hash))
(define hashtable-mutable?             (lambda (ht) #t))

(define hashtable-reset!        (lambda (ht) (unspecified)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This procedure should be called just before dumping a heap
; that contains eq? or eqv? hashtables.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reset-all-hashtables!)
  (let* ((record-like-objects
          (sro sys$tag.vector-tag sys$tag.structure-typetag -1))
         (hashtables
          (filter hashtable? (vector->list record-like-objects)))
         (problematic-hashtables
          (filter (lambda (ht) (not (hashtable-hash-function ht)))
                  hashtables)))
    (for-each hashtable-reset! problematic-hashtables)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Implementation.
;
; A hashtable is represented as a record whose fields contain:
;
; <count> is the number of associations within the hashtable,
; <hasher> is the hash function,
; <equiv> is the equivalence predicate,
; <searcher> is the bucket searcher,
; <htype> is a symbol (usual, eq?, or eqv?),
; <buckets> is a vector of buckets,
; <buckets1> may be a vector of buckets rehashed after major collections,
; <buckets0> may be a vector of buckets promoted after minor collections,
; <timestamp1> is (major-gc-counter) when <buckets1> was last rehashed,
; <timestamp0> is (gc-counter) when <buckets0> was hashed,
; <mutable> is a boolean where #t means the hashtable is mutable, and
; <lock> is used to detect race conditions.
;
; If <htype> is usual, then <buckets1> and <buckets0> are #f
; and <timestamp1> and <timestamp0> are 0.
;
; If <htype> is eq?, then <buckets1> and <buckets0> are vectors,
; <equiv> is the eq? procedure, and <hasher> is eq-hash.  All keys
; whose hash might be affected by garbage collection are in
; <buckets1> or <buckets0>.
;
; If <htype> is eqv?, then <buckets1> and <buckets0> are vectors,
; <equiv> is the eqv? procedure, and <hasher> is eqv-hash.  All
; keys whose hash code might be affected by garbage collection
; are in <buckets1> or <buckets0>.  Note, however, that the hash
; for numbers, characters, and other types that are treated
; differently by eq? and eqv? is independent of garbage collection.
;
; The <doc>,  <hasher>, <equiv>, <searcher>, and <htype> fields are
; immutable, but the <count>, <buckets>, and <lock> fields are mutable.
; <htype> is usual if and only if the <buckets1>, <buckets0>,
; <timestamp1>, and <timestamp0> fields are immutable.
;
; Operations that mutate a field must first obtain the lock.
; If the lock is already held by another operation, then a
; race condition must already exist in the application code;
; this should never happen in single-threaded systems.
;
; Most operations that do not mutate a field should be able
; to complete without consulting the lock, but operations on
; eq? and eqv? hashtables may have to retry after rehashing
; <buckets1> and/or <buckets0>.
;
; The code in this file assumes car, cdr, and vector-ref are
; atomic operations.

(define (eq-hash x)
  (cond ((symbol? x) (symbol-hash x))
        (else
         (.internal:machine-address x))))

(define (eqv-hash x)
  (cond ((number? x)
         (object-hash x))
        ((char? x)
         (object-hash x))
        ((or (memv x '("" #()))
             (and (bytevector? x) (= 0 (bytevector-length x))))
         (object-hash x))
        ((symbol? x)
         (symbol-hash x))
        (else
         (.internal:machine-address x))))

; An object is gc-sensitive if and only if its eq-hash might
; be changed by a garbage collection.

(define (gc-sensitive? x)
  (cond ((pair? x) #t)
        ((symbol? x) #f)
        ((vector-like? x) #t)
        ((bytevector-like? x) #t)
        ((procedure? x) #t)
        (else #f)))
  
(define *hashtable-rtd*
  (make-rtd 'hashtable
            '#(count
               (immutable hash-function)
               (immutable safe-hash-function)
               (immutable equivalence-predicate)
               (immutable bucket-searcher)
               (immutable hashtable-type)
               main-buckets
               tenured-buckets
               nursery-buckets
               tenured-timestamp
               nursery-timestamp
               mutable-flag
               (immutable the-lock))))

(let ((%hashtable? (rtd-predicate *hashtable-rtd*))
      (make-raw-ht
       (let ((raw-maker (rtd-constructor *hashtable-rtd*))
             (make-safe-hasher-caching
              (lambda (hf)
                (let ((cache #f))
                  (lambda (key)
                    (let ((keyhash cache))
                      (if (and keyhash (eq? key (car keyhash)))
                          (cdr keyhash)
                          (let ((h (hf key)))
                            (cond ((and (fixnum? h) (<= 0 h))
                                   (set! cache (cons key h))
                                   h)
                                  ((and (exact? h) (integer? h) (<= 0 h))
                                   h)
                                  (else
                                   (assertion-violation
                                    'hashtable
                                    "illegal hash value" h key))))))))))
             (make-safe-hasher
              (lambda (hf)
                (lambda (key)
                  (let ((h (hf key)))
                    (cond ((and (fixnum? h) (<= 0 h))
                           h)
                          ((and (exact? h) (integer? h) (<= 0 h))
                           h)
                          (else
                           (assertion-violation
                            'hashtable
                            "illegal hash value" h key)))))))
             (make-lock (lambda () (vector #f))))
         (lambda (hf equiv searcher size type)
           (let* ((n (max 1 size))
                  (n (if (eq? type 'usual)
                         n
                         (+ 1 (quotient n 2))))
                  (b (make-vector n '()))
                  (b0 (if (eq? type 'usual) #f (make-vector n '())))
                  (b1 (if (eq? type 'usual) #f (make-vector n '()))))
             (raw-maker 0 hf
                        (if (eq? type 'usual)
                            (make-safe-hasher-caching hf)
                            (make-safe-hasher hf))
                        equiv searcher type
                        b b1 b0
                        (major-gc-counter)
                        (gc-counter)
                        #t
                        (make-lock))))))
      (count       (rtd-accessor *hashtable-rtd* 'count))
      (count!      (rtd-mutator  *hashtable-rtd* 'count))
      (hasher      (rtd-accessor *hashtable-rtd* 'hash-function))
      (safe-hasher (rtd-accessor *hashtable-rtd* 'safe-hash-function))
      (equiv       (rtd-accessor *hashtable-rtd* 'equivalence-predicate))
      (searcher    (rtd-accessor *hashtable-rtd* 'bucket-searcher))
      (htype       (rtd-accessor *hashtable-rtd* 'hashtable-type))
      (buckets     (rtd-accessor *hashtable-rtd* 'main-buckets))
      (buckets!    (rtd-mutator  *hashtable-rtd* 'main-buckets))
      (buckets1    (rtd-accessor *hashtable-rtd* 'tenured-buckets))
      (buckets1!   (rtd-mutator  *hashtable-rtd* 'tenured-buckets))
      (buckets0    (rtd-accessor *hashtable-rtd* 'nursery-buckets))
      (buckets0!   (rtd-mutator  *hashtable-rtd* 'nursery-buckets))
      (timestamp1  (rtd-accessor *hashtable-rtd* 'tenured-timestamp))
      (timestamp1! (rtd-mutator  *hashtable-rtd* 'tenured-timestamp))
      (timestamp0  (rtd-accessor *hashtable-rtd* 'nursery-timestamp))
      (timestamp0! (rtd-mutator  *hashtable-rtd* 'nursery-timestamp))
      (mutable?    (rtd-accessor *hashtable-rtd* 'mutable-flag))
      (immutable!  (let ((mutable-flag!
                          (rtd-mutator *hashtable-rtd* 'mutable-flag)))
                     (lambda (ht) (mutable-flag! ht #f))))
      (make-lock   (lambda () (vector #f)))
      (lock!       (let ((ht-lock (rtd-accessor *hashtable-rtd* 'the-lock)))
                     (lambda (ht)

                       ; FIXME: this hack detects races and nested locking,
                       ; but won't work with true multithreading

                       (let* ((the-lock (ht-lock ht))
                              (r  (vector-like-cas! the-lock 0 #f #t)))
                         (if (not (eq? r #f))
                             (begin (set! *ht* ht)
                                    (assertion-violation
                                     'hashtable:lock!
                                     "race detected" ht)))))))
      (unlock!     (let ((ht-lock (rtd-accessor *hashtable-rtd* 'the-lock)))
                     (lambda (ht)

                       (let* ((the-lock (ht-lock ht))
                              (r  (vector-like-cas! the-lock 0 #t #f)))
                         (if (not (eq? r #t))
                             (assertion-violation
                              'hashtable:unlock!
                              "hashtable not locked" ht))))))

      (defaultn 20))

  (let ((hashtable-error (lambda (procedure x mut?)
                           (assertion-violation
                            procedure
                            (if mut?
                                "hashtable is immutable"
                                "bad hash table")
                            x))))

    (define (guarantee-hashtable procedure object)
      (if (not (%hashtable? object))
          (hashtable-error procedure object #f)))

    (define (guarantee-mutable procedure object)
      (if (not (%hashtable? object))
          (hashtable-error procedure object #f))
      (if (not (mutable? object))
          (hashtable-error procedure object #t)))

    ; Internal operations.

    (define (make-oldstyle-ht hashfun searcher size)
      (make-raw-ht hashfun
                   (lambda (x y)
                     (if (searcher x (list (list y))) #t #f))
                   searcher
                   size
                   'usual))

    (define (make-ht hashfun equiv searcher size)
      (make-raw-ht hashfun equiv searcher size 'usual))

    (define (make-ht-eq size)
      (make-raw-ht eq-hash eq? assq size 'eq?))

    (define (make-ht-eqv size)
      (make-raw-ht eqv-hash eqv? assv size 'eqv?))
    
    ; Remove the first occurrence of x from y.
    ; x is known to occur within y.
    
    (define (remq1 x y)
      (cond ((eq? x (car y))
             (cdr y))
            (else
             (cons (car y)
                   (remq1 x (cdr y))))))
    
    ; Resizes and rehashes the hashtable.
    ; ht is not locked.

    (define (resize ht)
      (lock! ht)
      (let ((n (+ defaultn (* 2 (count ht))))
            (b (buckets ht))
            (type (htype ht)))
        (case type
         ((usual)
          (let ((v  (make-vector n '())))
            (rehash-buckets! b v (safe-hasher ht))
            (buckets! ht v)
            (unlock! ht)
            (unspecified)))
         ((eq? eqv?)
          (let* ((n/2 (+ 1 (quotient n 2)))
                 (b1 (buckets1 ht))
                 (b0 (buckets0 ht))
                 (v  (make-vector n/2 '()))
                 (hf (if (eq? type 'eq?) eq-hash eqv-hash)))
            (rehash-buckets! b v hf)
            (buckets! ht v)
            (rehash1-locked! ht n/2)
            (if (< (timestamp0 ht) (gc-counter))
                (rehash0-locked! ht n/2)
                (rehash-without-collection!
                 ht
                 (buckets0 ht)
                 (make-vector n/2 '())
                 hf gc-counter buckets0! timestamp0!))
            (unlock! ht)))
         (else
          (unlock! ht)
          (assert (memq type '(usual eq? eqv?)))))))

    ; Rehashes the older gc-sensitive entries.
    ; ht is not locked.

    (define (rehash1! ht)
      (lock! ht)
      (rehash1-locked! ht (vector-length (buckets1 ht)))
      (unlock! ht))

    ; Rehashes the older gc-sensitive entries.
    ; ht is already locked.

    (define (rehash1-locked! ht n)
      (let* ((b1 (buckets1 ht))
             (t1 (timestamp1 ht))
             (v1 (make-vector n '()))
             (hf (if (eq? (htype ht) 'eq?) eq-hash eqv-hash)))
        (rehash-without-collection!
         ht b1 v1 hf major-gc-counter buckets1! timestamp1!)))

    ; Promotes the youngest gc-sensitive entries.
    ; ht is not locked.

    (define (rehash0! ht)
      (lock! ht)
      (rehash0-locked! ht (vector-length (buckets0 ht)))
      (unlock! ht))

    ; Promotes the youngest gc-sensitive entries.
    ; ht is already locked.

    (define (rehash0-locked! ht n)
      (let* ((b0 (buckets0 ht))
             (b1 (buckets1 ht))
             (t0 (timestamp0 ht))
             (t1 (timestamp1 ht))
             (v0 (make-vector n '()))
             (hf (if (eq? (htype ht) 'eq?) eq-hash eqv-hash)))
        (assert (< t0 (gc-counter)))
        (rehash-without-collection!
         ht b0 b1 hf major-gc-counter buckets1! timestamp1!)
        (buckets0! ht v0)
        (timestamp0! ht (gc-counter))
        (if (not (= t1 (major-gc-counter)))
            (let ((v1 (make-vector (vector-length b1) '())))
              (rehash-without-collection!
               ht b1 v1 hf major-gc-counter buckets1! timestamp1!)))))

    ; ht is already locked.

    (define (rehash-without-collection!
             ht src dst hf gc-counter buckets! timestamp!)
      (let loop ((src src)
                 (dst dst)
                 (t (gc-counter))
                 (attempts 0))
        (rehash-buckets! src dst hf)
        (cond ((= t (gc-counter))
               (buckets! ht dst)
               (timestamp! ht t))
              ((< attempts 3)
               (loop dst
                     (make-vector (vector-length dst) '())
                     (gc-counter)
                     (+ 1 attempts)))
              (else
               ; FIXME: should this raise a warning exception?
               (let ((out (current-error-port)))
                 (display "WARNING: hashtable too large " out)
                 (display "for this garbage collector" out)
                 (newline out))
               (let ((dst (make-vector 1 '())))
                 (rehash-buckets! src dst hf)
                 (buckets! ht dst)
                 (timestamp! ht t))))))

    ; Copies all entries in the src vector to the dst vector,
    ; rehashing each key using the hash function hf.

    (define (rehash-buckets! src dst hf)
      (let ((m (vector-length src))
            (n (vector-length dst)))
        (do ((i 0 (+ i 1)))
            ((= i m))
          (do ((bucket (vector-ref src i) (cdr bucket)))
              ((null? bucket))
            (let* ((entry (car bucket))
                   (key (car entry))
                   (h (hf key))
                   (j (mod h n)))
              (vector-set! dst j (cons entry (vector-ref dst j))))))))
    
    ; Returns the keys and values of the hashtable as two vectors.
    
    (define (ht-entries ht)
      (guarantee-hashtable 'hashtable-entries ht)
      (lock! ht)
      (let* ((v (buckets ht))
             (v0 (buckets0 ht))
             (v1 (buckets1 ht))
             (k (count ht))
             (keys (make-vector k '()))
             (vals (make-vector k '())))
        (define (collect-entries v j)
          (loop v 0 (vector-length v) '() j))
        (define (loop v i n bucket j)
          (cond ((pair? bucket)
                 (let ((entry (car bucket)))
                   (vector-set! keys j (car entry))
                   (vector-set! vals j (cdr entry))
                   (loop v i n (cdr bucket) (+ j 1))))
                ((null? bucket)
                 (if (= i n)
                     j
                     (loop v (+ i 1) n (vector-ref v i) j)))
                (else (error 'hashtable-entries
                             "illegal hashtable structure"))))
        (let* ((j (if v0 (collect-entries v0 0) 0))
               (j (if v1 (collect-entries v1 j) j))
               (j (collect-entries v j)))
          (unlock! ht)
          (if (= j k)
              (values keys vals)
              (begin (error 'ht-entries "BUG in hashtable")
                     (values '#() '#()))))))

    ; Returns the keys of the hashtable as a vector.

    (define (ht-keys ht)
      (call-with-values
       (lambda () (ht-entries ht))
       (lambda (keys vals) keys)))
    
    (define (contains? ht key)
      (guarantee-hashtable 'hashtable-contains? ht)
      (let* ((type (htype ht))
             (bucket-search (searcher ht))
             (hf (safe-hasher ht))
             (h (hf key)))
        (define (tablet-contains? v)
          (let* ((n (vector-length v))
                 (i (remainder h n))
                 (b (vector-ref v i)))
            (if (bucket-search key b) #t #f)))
        (cond ((or (eq? type 'usual)
                   (not (gc-sensitive? key)))
               (tablet-contains? (buckets ht)))
              ((tablet-contains? (buckets1 ht))
               #t)
              ((tablet-contains? (buckets0 ht))
               #t)
              ((not (= (timestamp0 ht) (gc-counter)))
               (rehash0! ht)
               (contains? ht key))
              ((not (= (timestamp1 ht) (major-gc-counter)))
               (rehash1! ht)
               (contains? ht key))
              (else
               #f))))

    (define (fetch ht key flag)
      (guarantee-hashtable 'hashtable-ref ht)
      (let* ((type (htype ht))
             (bucket-search (searcher ht))
             (hf (safe-hasher ht))
             (h (hf key)))
        (define (tablet-search v)
          (let* ((n (vector-length v))
                 (i (remainder h n))
                 (b (vector-ref v i)))
            (bucket-search key b)))
        (cond ((or (eq? type 'usual)
                   (not (gc-sensitive? key)))
               (let ((entry (tablet-search (buckets ht))))
                 (if entry
                     (cdr entry)
                     flag)))
              ((tablet-search (buckets1 ht))
               => cdr)
              ((tablet-search (buckets0 ht))
               => cdr)
              ((not (= (timestamp0 ht) (gc-counter)))
               (rehash0! ht)
               (fetch ht key flag))
              ((not (= (timestamp1 ht) (major-gc-counter)))
               (rehash1! ht)
               (fetch ht key flag))
              (else
               flag))))

    (define (put! ht key val)
      (guarantee-mutable 'hashtable-set! ht)
      (lock! ht)
      (let* ((type (htype ht))
             (bucket-search (searcher ht))
             (hf (safe-hasher ht))
             (h (hf key)))
        (define (tablet-search v)
          (let* ((n (vector-length v))
                 (i (remainder h n))
                 (b (vector-ref v i)))
            (bucket-search key b)))
        (cond ((or (eq? type 'usual)
                   (not (gc-sensitive? key)))
               (let* ((v (buckets ht))
                      (entry (tablet-search v)))
                 (if entry
                     (begin (set-cdr! entry val)
                            (unlock! ht)
                            (unspecified))
                     (let* ((n (vector-length v))
                            (i (remainder h n))
                            (b (vector-ref v i)))
                       (vector-set! v i (cons (cons key val) b))
                       (count! ht (+ 1 (count ht)))
                       (unlock! ht)
                       (maybe-resize! ht)))))
              ((tablet-search (buckets1 ht))
               =>
               (lambda (entry)
                 (set-cdr! entry val) (unlock! ht) (unspecified)))
              ((tablet-search (buckets0 ht))
               =>
               (lambda (entry)
                 (set-cdr! entry val) (unlock! ht) (unspecified)))
              ((not (= (timestamp0 ht) (gc-counter)))
               (unlock! ht)
               (rehash0! ht)
               (put! ht key val))
              ((not (= (timestamp1 ht) (major-gc-counter)))
               (unlock! ht)
               (rehash1! ht)
               (put! ht key val))
              (else
               (let* ((v (buckets0 ht))
                      (t (gc-counter))
                      (h (hf key))
                      (n (vector-length v))
                      (i (remainder h n))
                      (b (vector-ref v i)))
                 (if (= t (gc-counter) (timestamp0 ht))
                     (begin (vector-set! v i (cons (cons key val) b))
                            (count! ht (+ 1 (count ht)))
                            (unlock! ht)
                            (maybe-resize! ht))
                     (begin (unlock! ht)
                            (put! ht key val))))))))

    (define (remove! ht key)
      (guarantee-mutable 'hashtable-delete! ht)
      (lock! ht)
      (let* ((type (htype ht))
             (bucket-search (searcher ht))
             (hf (safe-hasher ht))
             (h (hf key)))
        (define (tablet-remove! v)
          (let* ((n (vector-length v))
                 (i (remainder h n))
                 (b (vector-ref v i))
                 (probe (bucket-search key b)))
            (if probe
                (begin (vector-set! v i (remq1 probe b)) #t)
                #f)))
        (cond ((or (eq? type 'usual)
                   (not (gc-sensitive? key)))
               (if (tablet-remove! (buckets ht))
                   (begin (count! ht (- (count ht) 1))
                          (unlock! ht)
                          (maybe-resize! ht))
                   (unlock! ht)))
              ((tablet-remove! (buckets1 ht))
               (count! ht (- (count ht) 1))
               (unlock! ht)
               (maybe-resize! ht))
              ((tablet-remove! (buckets0 ht))
               (count! ht (- (count ht) 1))
               (unlock! ht)
               (maybe-resize! ht))
              ((not (= (timestamp0 ht) (gc-counter)))
               (unlock! ht)
               (rehash0! ht)
               (remove! ht key))
              ((not (= (timestamp1 ht) (major-gc-counter)))
               (unlock! ht)
               (rehash1! ht)
               (remove! ht key))
              (else
               (unlock! ht)
               (unspecified)))))

    ; Heuristic resizing of an unlocked hashtable.

    (define (maybe-resize! ht)
      (let* ((k (count ht))
             (v (buckets ht))
             (v1 (buckets1 ht))
             (n (+ (vector-length v)
                   (if v1 (vector-length v1) 0))))
        (if (or (< n k)
                (< (* 3 (+ defaultn k)) n))
            (resize ht))
        (unspecified)))

    (define (clear! ht n)
      (guarantee-mutable 'hashtable-clear! ht)
      (lock! ht)
      (count! ht 0)
      (buckets! ht (make-vector (+ defaultn n) '()))
      (if (not (eq? (htype ht) 'usual))
          (let* ((n/2 (+ defaultn (quotient n 2))))
            (buckets1! ht (make-vector n/2 '()))
            (buckets0! ht (make-vector n/2 '()))
            (timestamp1! ht (major-gc-counter))
            (timestamp0! ht (gc-counter))))
      (unlock! ht)
      (unspecified))

    (define (size ht)
      (guarantee-hashtable 'size ht)
      (count ht))

    (define (ht-copy ht mutable)
      (guarantee-hashtable 'ht-copy ht)
      (let* ((type (htype ht))
             (k (count ht))
             (newtable
              (case type
               ((usual)
                (make-r6rs-hashtable (hasher ht) (equiv ht) k))
               ((eq?)
                (make-eq-hashtable k))
               ((eqv?)
                (make-eqv-hashtable k))
               (else (assert (memq type '(usual eq? eqv?)))))))
        (call-with-values
         (lambda () (hashtable-entries ht))
         (lambda (keys vals)
           (vector-for-each (lambda (key val)
                              (hashtable-set! newtable key val))
                            keys vals)
           (if (not mutable)
               (immutable! newtable))
           newtable))))

    ; External entry points.
    
    (set! make-oldstyle-hashtable
          (lambda args
            (let* ((hashfun (if (null? args) eqv-hash (car args)))
                   (searcher (if (or (null? args) (null? (cdr args)))
                                 assv
                                 (cadr args)))
                   (size (if (or (null? args)
                                 (null? (cdr args))
                                 (null? (cddr args)))
                             defaultn
                             (caddr args))))
              (make-oldstyle-ht hashfun searcher size))))

    (set! make-r6rs-hashtable
          (lambda (hashfun equiv . rest)
            (define (search-bucket key bucket)
              (cond ((null? bucket) #f)
                    ((equiv key (caar bucket))
                     (car bucket))
                    (else (search-bucket key (cdr bucket)))))
            (make-ht hashfun
                     equiv
                     (lambda (key bucket) (search-bucket key bucket))
                     (if (null? rest) defaultn (car rest)))))

    (set! make-eq-hashtable
          (lambda rest
            (make-ht-eq (if (null? rest) defaultn (car rest)))))

    (set! make-eqv-hashtable
          (lambda rest
            (make-ht-eqv (if (null? rest) defaultn (car rest)))))

    (set! hashtable?          (lambda (object)      (%hashtable? object)))
    (set! hashtable-contains? (lambda (ht key)      (contains? ht key)))
    (set! hashtable-ref       (lambda (ht key flag) (fetch ht key flag)))
    (set! hashtable-set!      (lambda (ht key val)  (put! ht key val)))
    (set! hashtable-delete!   (lambda (ht key)      (remove! ht key)))
    (set! hashtable-clear!    (lambda (ht . rest)
                                (clear! ht
                                        (if (null? rest)
                                            defaultn
                                            (car rest)))))
    (set! hashtable-size      (lambda (ht)          (size ht)))
    (set! hashtable-keys      (lambda (ht)          (ht-keys ht)))
    (set! hashtable-entries   (lambda (ht)          (ht-entries ht)))
    (set! hashtable-copy      (lambda (ht . rest)
                                (ht-copy ht (if (null? rest) #f (car rest)))))

    (set! hashtable-equivalence-function (lambda (ht) (equiv ht)))
    (set! hashtable-hash-function        (lambda (ht)
                                           (if (eq? (htype ht) 'usual)
                                               (hasher ht)
                                               #f)))
    (set! hashtable-mutable?             (lambda (ht) (mutable? ht)))

    (set! hashtable-reset!    (lambda (ht)
                                (timestamp0! ht -1)
                                (timestamp1! ht -1)))

    #f))

; eof
