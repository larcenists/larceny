; Copyright 1999 Lars T Hansen
;
; $Id$
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
; Note.
;
; It is important that the implementation be robust in the face of 
; occasional garbage collections while it is running.  The reason is
; that Larceny may garbage collect even in algorithms that do not
; cons explicitly, because the stack cache overflow can cause GC.
;
; Fixme.
;
; We must implement the primitive object-approximate-representation,
; for which an approximate (!) implementation is currently used.
;
; Object-approximate-representation should be a primitive that 
; returns a nonnegative fixnum that relates to the object's current
; representation but that may lose information, as long as it always 
; returns the same output for the same input.  Here's one possible one:
;
;    andn  %RESULT, 7, %RESULT
;    srl   %RESULT, 1, %RESULT

(define object-id)

(let ()

  (define tbl (make-vector 10 '()))     ; The hash table
  (define last-gc-counter 0)            ; GC counter at last rehash
  (define size 0)                       ; The number of elements in the table
  (define size-at-collection 0)         ; Number of elements at last table gc
  (define additions-since-collection 0) ; Number added since last table gc
  (define id-counter 0)                 ; Object IDs
  (define debug #t)                     ; Set to #t for messages

  (define (message . rest)
    (if debug
        (begin (for-each display rest)
               (newline))))

  ; This is an effective stand-in; needs to be replaced.

  (define (object-approximate-representation obj)
    (cond ((pair? obj)             (syscall 36 obj))
          ((procedure? obj)        (syscall 36 obj))
          ((vector-like? obj)      (syscall 36 obj))
          ((bytevector-like? obj)  (syscall 36 obj))
          ((fixnum? obj)           obj)
          ((null? obj)             10)
          ((eq? obj #t)            6)
          ((eq? obj #f)            2)
          ((char? obj)             (char->integer obj))
          ((eq? obj (undefined))   #x316)
          ((eq? obj (unspecified)) #x116)
          ((eof-object? obj)       #x216)
          (else                    0)))

  (define (next-id)
    (set! id-counter (+ id-counter 1))
    id-counter)

  (define (hash obj)
    (remainder (object-approximate-representation obj)
               (vector-length tbl)))

  (define (get obj)
    (let ((probe (assq obj (vector-ref tbl (hash obj)))))
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
    (let ((h (hash obj)))
      (let ((probe (assq obj (vector-ref tbl h))))
        (if probe
            (begin (vector-set! tbl h (remq! probe (vector-ref tbl h)))
                   #t)
            #f))))

  (define (grow-and-rehash!)
    (let ((new-tbl (make-vector (* (vector-length tbl) 2) '())))
      (do ((i 0 (+ i 1)))
          ((= i (vector-length tbl)))
        (vector-set! new-tbl i (vector-ref tbl i)))
      (set! tbl new-tbl)
      (rehash!)))

  (define (rehash!)
    (collect)                           ; For maximum effect
    (set! last-gc-counter (gc-counter))               
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
            (let ((node nodes)
                  (next (cdr nodes)))
              (let ((h (hash (caar node))))
                (set-cdr! node (vector-ref tbl h))
                (vector-set! tbl h node)
                (loop next)))))))

  (define (conditionally-rehash)
    (if (> (gc-counter) last-gc-counter)
        (begin (rehash!)
               #t)
        #f))

  (define (collect-table)               ; Expensive
    (let ((referenced-once (sro -1 -1 1))
          (removed 0))
      (conditionally-rehash)
      (do ((i 0 (+ i 1)))
          ((= i (vector-length referenced-once))
           (set! size-at-collection size)
           (set! additions-since-collection 0)
           (message "OBJECT-ID: Removed " removed "; size=" size))
        (if (remove-if-present (vector-ref referenced-once i))
            (begin (set! removed (+ removed 1))
                   (set! size (- size 1)))))))

  (define (conditionally-collect-table)
    (if (and (>= additions-since-collection size-at-collection)
             (> additions-since-collection 100))
        (collect-table)))

  (set! object-id
        (lambda (obj)
          (if (not (or (pair? obj)
                       (vector-like? obj)
                       (procedure? obj)
                       (bytevector-like? obj)))
              (object-approximate-representation obj)
              (let* ((i (disable-interrupts))
                     (x (let loop ()    ; Robust
                          (let ((probe (get obj)))
                            (cond (probe)
                                  ((conditionally-rehash)
                                   (loop))
                                  (else
                                   (conditionally-collect-table)
                                   (let ((id (next-id)))
                                     (put! obj id)
                                     (set! additions-since-collection
                                           (+ additions-since-collection 1))
                                     id)))))))
                (if i (enable-interrupts i))
                x))))
  'object-id)

; eof
