; $Id$
; 991118 / lth
; Not yet operational, we're missing a primitive.
;
; (object-id object)
;   Returns an object ID -- a fixnum -- for the object.  The object ID 
;   is tied to the object's identity and never changes.
;
;   OBJECT-ID is guaranteed to return a unique nonnegative object ID for
;   any boxed object, but may return an object ID for a nonboxed object
;   that is negative, equal to the object ID of another nonboxed object,
;   or equal to the object ID of some boxed object.
;
; FIXME:
;  The table collection policy may need some tuning, and it may be 
;  desirable the object table collector to allow collections to be
;  forced.  In the long run we should perhaps use GC-supported weak 
;  cells, or the GC should support object-id directly.
;
; FIXME:
;  We must implement the primitive object-approximate-representation.
;
;  Object-approximate-representation should be a primitive that 
;  returns a nonnegative fixnum that relates to the object's current
;  representation but that may lose information, as long as it always 
;  returns the same output for the same input.  Here's one possible one:
;
;        andn  %RESULT, 7, %RESULT
;        srl   %RESULT, 1, %RESULT

(define object-id)

(let ()

  (define last-gc-counter 0)            ; GC counter at last rehash
  (define tbl (make-vector 10 '()))     ; The hash table
  (define size 0)                       ; The number of elements in the table
  (define size-at-rehash 0)             ; The number of elements at last rehash
  (define additions-since-rehash 0)     ; The number added since last rehash

  (define (hash obj)
    (remainder (object-approximate-representation obj)
               (vector-length tbl)))

  (define (get obj)
    (let ((probe (assq object (vector-ref tbl (hash obj)))))
      (if probe
          (cdr probe)
          #f)))

  (define (put! obj value)
    (let ((h (hash obj)))
      (vector-set! tbl h (cons (cons obj value) (vector-ref tbl h)))
      (set! size (+ size 1))
      (if (= size (vector-length tbl))
          (grow-and-rehash!))))

  (define (remove-if-present obj)
    (let ((h (hash-obj)))
      (let ((probe (assq obj (vector-ref tbl h))))
        (if probe
            (vector-set! tbl h (remq! probe (vector-ref tbl h)))))))

  (define (grow-and-rehash!)
    (let ((new-tbl (make-vector (* (vector-length tbl) 2) '())))
      (do ((i 0 (+ i 1)))
          ((= i (vector-length tbl)))
        (vector-set! new-tbl i (vector-ref tbl i)))
      (set! tbl new-tbl)
      (rehash!)))

  (define (rehash!)
    (collect)
    (set! size-at-rehash size)
    (set! additions-since-rehash 0)
    (let ((nodes '()))
      (do ((i 0 (+ i 1)))
          ((= i (vector-length tbl)))
        (let ((bucket (vector-ref tbl i)))
          (vector-set! tbl i '())
          (if (not (null? bucket))
              (begin (set-cdr! (last-pair bucket) nodes)
                     (set! nodes bucket)))))
      (let loop ((nodes nodes))
        (if (not (null? nodes))
            (let ((n (car nodes))
                  (next (cdr nodes)))
              (let ((h (hash (caar n))))
                (set-cdr! n (vector-ref tbl i))
                (vector-set! tbl i n)
                (loop next)))))))

  (define (conditionally-rehash)
    (if (> (gc-counter) last-gc-counter)
        (begin (set! last-gc-counter (gc-counter))
               (rehash!)
               #t)
        #f))

  (define (collect-table)               ; Expensive
    (let ((referenced-once (sro -1 -1 1)))
      (conditionally-rehash)
      (do ((i 0 (+ i 1)))
          ((= i (vector-length referenced-once)))
        (remove-if-present (vector-ref referenced-once i)))))

  (define (conditionally-collect-table)
    (if (and (>= additions-since-rehash size-at-rehash)
             (> additions-since-rehash 100))
        (collect-table)))

  (define (object-id-for-constant obj)
    (object-approximate-representation obj))
  
  (set! object-id
        (lambda (object)
          (if (not (or (pair? object)
                       (vector-like? object)
                       (procedure? object)
                       (bytevector-like? object)))
              (object-approximate-representation object)
              (let loop ()
                (let ((probe (get object)))
                  (cond (probe)
                        ((conditionally-rehash)
                         (loop))
                        (else
                         (conditionally-collect-table)
                         (let ((id (next-id)))
                           (put! object id)
                           (set! additions-since-rehash
                                 (+ additions-since-rehash 1))
                           id))))))))
  'object-id)

; eof
