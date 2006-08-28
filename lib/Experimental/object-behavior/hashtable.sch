; Hash tables specialized for COMPUTE-OBJECT-BEHAVIOR.
; No longer requires CALL-WITHOUT-INTERRUPTS.
; This code should be thread-safe provided VECTOR-REF is atomic.
;
; 24 June 1999 / lth
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
; (hashtable-unsafe-put-unique! <hashtable> <key> <value>)
;
;     Changes the <hashtable> to associate <key> with <value>, assuming
;     no existing association for <key>.  This is a performance hack --
;     it makes a major difference for compute-object-behavior, where
;     some buckets are very large.
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

; These global variables are assigned new values later.

(define make-hashtable      (lambda args '*))
(define hashtable-contains? (lambda (ht key) #f))
(define hashtable-fetch     (lambda (ht key flag) flag))
(define hashtable-get       (lambda (ht key) (hashtable-fetch ht key #f)))
(define hashtable-put!      (lambda (ht key val) '*))
(define hashtable-unsafe-put-unique! (lambda (ht key val) '*))
(define hashtable-remove!   (lambda (ht key) '*))
(define hashtable-clear!    (lambda (ht) '*))
(define hashtable-size      (lambda (ht) 0))
(define hashtable-for-each  (lambda (ht proc) '*))
(define hashtable-map       (lambda (ht proc) '()))
(define hashtable-copy      (lambda (ht) ht))

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
      (defaultn 10)
      (make-vector     make-vector)
      (vector          vector)
      (list->vector    list->vector)
      (vector->list    vector->list)
      ;(cons            cons)
      (list            list)
      (map             map)
      (append          append)
      (reverse         reverse)
      (make-string     make-string)
      (string          string)
      (list->string    list->string)
      (string->list    string->list)
      (string-copy     string-copy)
      (string-append   string-append)
      (substring       substring)
      (number->string  number->string))

  (let ((hashtable? (lambda (ht)
                      (and (vector? ht)
                           (= 5 (vector-length ht))
                           (eq? doc (vector-ref ht 0)))))
        (hashtable-error (lambda (x)
                           (display "ERROR: Bad hash table: ")
                           (newline)
                           (write x)
                           (newline))))
    
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
    
    (define (remq1! key list)
      (cond ((null? list) list)
            ((eq? key (car list))
             (cdr list))
            (else
             (set-cdr! list (remq1! key (cdr list)))
             list)))

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
          (if (null? bucket)
              (if (= i n)
                  (if (= j (vector-length z))
                      z
                      (begin (display "BUG in hashtable")
                             (newline)
                             '#()))
                  (loop (+ i 1)
                        (vector-ref v i)
                        j))
              (let ((entry (car bucket)))
                (vector-set! z j (cons (car entry) (cdr entry)))
                (loop i
                      (cdr bucket)
                      (+ j 1)))))
        (loop 0 '() 0)))
    
    (define (contains? ht key)
      (if (hashtable? ht)
          (let* ((v (buckets ht))
                 (n (vector-length v))
                 (h (modulo ((hasher ht) key) n))
                 (b (vector-ref v h)))
            (if ((searcher ht) key b)
                #t
                #f))
          (hashtable-error ht)))
    
    (define (fetch ht key flag)
      (if (hashtable? ht)
          (let* ((v (buckets ht))
                 (n (vector-length v))
                 (h (remainder ((hasher ht) key) n)) ; was: modulo
                 (b (vector-ref v h))
                 (probe ((searcher ht) key b)))
            (if probe
                (cdr probe)
                flag))
          (hashtable-error ht)))
    
    (define (put! ht key val)
      (if (hashtable? ht)
          (let* ((v (buckets ht))
                 (n (vector-length v))
                 (h (modulo ((hasher ht) key) n))
                 (b (vector-ref v h))
                 (probe ((searcher ht) key b)))
            (if probe
                ; Using SET-CDR! on the probe would make it necessary
                ; to synchronize the CONTENTS routine.
                (vector-set! v h (substitute1 (cons key val) probe b))
                (begin (count! ht (+ (count ht) 1))
                       (vector-set! v h (cons (cons key val) b))
                       (if (> (count ht) n)
                           (resize ht))))
            #f)
          (hashtable-error ht)))

    (define (put-unique! ht key val)
      (let* ((v (buckets ht))
             (n (vector-length v))
             (h (remainder ((hasher ht) key) n)) ; was: modulo
             (b (vector-ref v h)))
       (count! ht (+ (count ht) 1))
       (vector-set! v h (cons (cons key val) b))
       (if (> (count ht) n)
           (resize ht))
       #f))

    (define (remove! ht key)
      (if (hashtable? ht)
          (let* ((v (buckets ht))
                 (n (vector-length v))
                 (h (modulo ((hasher ht) key) n))
                 (b (vector-ref v h))
                 (probe ((searcher ht) key b)))
            (if probe
                (begin (count! ht (- (count ht) 1))
                       (vector-set! v h (remq! probe b))
                       (if (< (* 2 (+ defaultn (count ht))) n)
                              (resize ht))))
            #f)
          (hashtable-error ht)))
    
    (define (clear! ht)
      (if (hashtable? ht)
          (call-without-interrupts
           (lambda ()
             (begin (count! ht 0)
                    (buckets! ht (make-vector defaultn '()))
                    #f)))
          (hashtable-error ht)))
    
    (define (size ht)
      (if (hashtable? ht)
          (count ht)
          (hashtable-error ht)))
    
    ; This code must be written so that the procedure can modify the
    ; hashtable without breaking any invariants.
    
    (define (ht-for-each f ht)
      (if (hashtable? ht)
          (let* ((v (contents ht))
                 (n (vector-length v)))
            (do ((j 0 (+ j 1)))
                ((= j n))
                (let ((x (vector-ref v j)))
                  (f (car x) (cdr x)))))
          (hashtable-error ht)))
    
    (define (ht-map f ht)
      (if (hashtable? ht)
          (let* ((v (contents ht))
                 (n (vector-length v)))
            (do ((j 0 (+ j 1))
                 (results '() (let ((x (vector-ref v j)))
                                (cons (f (car x) (cdr x))
                                      results))))
                ((= j n)
                 (reverse results))))
          (hashtable-error ht)))
    
    (define (ht-copy ht)
      (if (hashtable? ht)
          (let* ((newtable (make-hashtable (hasher ht) (searcher ht) 0))
                 (v (buckets ht))
                 (n (vector-length v))
                 (newvector (make-vector n '())))
            (count! newtable (count ht))
            (buckets! newtable newvector)
            (do ((i 0 (+ i 1)))
                ((= i n))
                (vector-set! newvector i (append (vector-ref v i) '())))
            newtable)
          (hashtable-error ht)))
    
    ; External entry points.
    
    (set! make-hashtable
          (lambda args
            (let* ((hashfun (if (null? args) object-hash (car args)))
                   (searcher (if (or (null? args) (null? (cdr args)))
                                 assv
                                 (cadr args)))
                   (size (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                             defaultn
                             (caddr args))))
              (make-ht hashfun searcher size))))
    
    (set! hashtable-contains? (lambda (ht key)      (contains? ht key)))
    (set! hashtable-fetch     (lambda (ht key flag) (fetch ht key flag)))
    (set! hashtable-get       (lambda (ht key)      (fetch ht key #f)))
    (set! hashtable-put!      (lambda (ht key val)  (put! ht key val)))
    (set! hashtable-unsafe-put-unique! 
                              (lambda (ht key val)  (put-unique! ht key val)))
    (set! hashtable-remove!   (lambda (ht key)      (remove! ht key)))
    (set! hashtable-clear!    (lambda (ht)          (clear! ht)))
    (set! hashtable-size      (lambda (ht)          (size ht)))
    (set! hashtable-for-each  (lambda (ht proc)     (ht-for-each ht proc)))
    (set! hashtable-map       (lambda (ht proc)     (ht-map ht proc)))
    (set! hashtable-copy      (lambda (ht)          (ht-copy ht)))
    #f))
