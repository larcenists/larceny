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

;;; Private stuff, not exported.

;;; Comparators contain a type test predicate, which implementations
;;; of the hash-table-set! procedure can use to reject invalid keys.
;;; That's hard to do without sacrificing interoperability with R6RS
;;; and/or SRFI 69 hash tables.
;;;
;;; Full interoperability means the hash tables implemented here are
;;; interchangeable with the R6RS hashtables used to implement them.
;;; SRFI 69 and R6RS hashtables don't contain comparators, so any
;;; association between a hash table and its comparator would have to
;;; be maintained outside the representation of hash tables themselves,
;;; which is problematic unless weak pointers are available.
;;;
;;; Not all of the hash tables implemented here will have comparators
;;; associated with them anyway, because an equivalence procedure
;;; and hash function can be used to create a hash table instead of
;;; a comparator.
;;;
;;; One way to preserve interoperability while enforcing a comparator's
;;; type test is to incorporate that test into a hash table's hash
;;; function.  The advantage of doing that should be weighed against
;;; these disadvantages:
;;;
;;;     If the type test is slow, then hashing would also be slower.
;;;
;;;     The R6RS and SRFI 69 APIs (but not the API implemented here)
;;;     allow extraction of a hash function from some hash tables.
;;;     Some programmers might expect that hash function to be the
;;;     hash function encapsulated by the comparator (in the sense
;;;     of eq?, perhaps) even though this API makes no such guarantee.

;;; If %enforce-comparator-type-tests is true, then make-hash-table,
;;; when passed a comparator, will use a hash function that enforces
;;; the comparator's type test.

(define %enforce-comparator-type-tests #t)

;;; Given a comparator, return its hash function, possibly augmented
;;; by the comparator's type test.

(define (%comparator-hash-function comparator)
  (let ((okay? (comparator-type-test-procedure comparator))
        (hash-function (comparator-hash-function comparator)))
    (if %enforce-comparator-type-tests
        (lambda (x . rest)
          (cond ((not (okay? x))
                 (error "key rejected by hash-table comparator"
                        x
                        comparator))
                ((null? rest)
                 (hash-function x))
                (else
                 (apply hash-function x rest))))
        hash-function)))

;;; A unique (in the sense of eq?) value that will never be found
;;; within a hash-table.

(define %not-found (list '%not-found))

;;; FIXME: %not-found-irritant and %not-found-message were used
;;; to implement hash-table-key-not-found?, which is no longer
;;; part of the HashTablesCowan API.

;;; A unique (in the sense of eq?) value that escapes only as an irritant
;;; when a hash-table key is not found.

(define %not-found-irritant (list 'not-found))

;;; The error message used when a hash-table key is not found.

(define %not-found-message "hash-table key not found")

;;; FIXME: no longer exported; keeping it here in case it comes back

(define (hash-table-key-not-found? obj)
  (and (error-object? obj)
       (string=? (error-object-message obj)
                 %not-found-message)
       (memq %not-found-irritant
             (error-object-irritants obj))
       #t))

;;; FIXME: thread-safe, weak-keys, and weak-values are not supported
;;; by this portable reference implementation.

(define (%check-optional-arguments procname args)
  (if (or (memq 'thread-safe args)
          (memq 'weak-keys args)
          (memq 'weak-values args))
      (error (string-append (symbol->string procname)
                            ": unsupported optional argument(s)")
             args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exported procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constructors.

;;; The first argument can be a comparator or an equality predicate.
;;;
;;; If the first argument is a comparator, any remaining arguments
;;; are implementation-dependent, but "an error should be signalled"
;;; if those arguments include any of the symbols thread-safe,
;;; weak-keys, or weak-values.
;;;
;;; If the first argument is not a comparator, then it had better
;;; be an equality predicate.  If a second argument is present and
;;; is a procedure, then it's a hash function.  If a second argument
;;; is not a procedure, then it's an implementation-dependent
;;; optional argument, as are all arguments beyond the second.
;;;
;;; SRFI 114 allows the hash function of a comparator to return
;;; different results for mutable objects following mutations of
;;; those objects, but HashTablesCowan says "It is the programmer's
;;; responsibility to ensure that if two objects are the same in the
;;; sense of the equality predicate, then they return the same value
;;; when passed to the hash function."  That means the programmer is
;;; responsible for not using any comparators that don't have that
;;; property.  In particular, the eq-comparator and eqv-comparator
;;; specified by SRFI 114 are not guaranteed to have that property,
;;; and the eq-comparator and eqv-comparator defined by the reference
;;; implementation of SRFI 114 do not have that property.  That means
;;; passing eq-comparator or eqv-comparator to make-hash-table is
;;; likely to be an error.  There is no practical way to tell whether
;;; eq-comparator and eqv-comparator have the required property, so
;;; the implementation of make-hash-table below has special cases for
;;; eq-comparator and eqv-comparator.
;;;
;;; When a comparator is passed, HashTablesCowan says the hash
;;; function is extracted from the comparator, but also says the
;;; make-hash-table procedure can use implementation-dependent
;;; hash functions when the equality predicate is a refinement
;;; of the equal? predicate.  That sanctions the special casing
;;; of eq-comparator and eqv-comparator.

(define (make-hash-table comparator/equiv . rest)
  (if (comparator? comparator/equiv)
      (begin (%check-optional-arguments 'make-hash-table rest)
             (cond ((equal? comparator/equiv eq-comparator)
                    (make-eq-hashtable))
                   ((equal? comparator/equiv eqv-comparator)
                    (make-eqv-hashtable))
                   (else
                    (let ((equiv
                           (comparator-equality-predicate comparator/equiv))
                          (hash-function
                           (%comparator-hash-function comparator/equiv)))
                      (make-hashtable hash-function equiv)))))
      (let ((equiv comparator/equiv)
            (hash-function (if (and (not (null? rest))
                                    (procedure? (car rest)))
                               (car rest)
                               #f)))
	(%check-optional-arguments 'make-hash-table
                                   (if hash-function (cdr rest) rest))
        (cond ((equal? equiv eq?)
               (make-eq-hashtable))
              ((equal? equiv eqv?)
               (make-eqv-hashtable))
              (hash-function
               (make-hashtable hash-function equiv))
              ((equal? equiv equal?)
               (make-hashtable equal-hash equiv))
              ((equal? equiv string=?)
               (make-hashtable string-hash equiv))
              ((equal? equiv string-ci=?)
               (make-hashtable string-ci-hash equiv))
              ((equal? equiv symbol=?)
               (make-hashtable symbol-hash equiv))
              (else
               (error "make-hash-table: unable to infer hash function"
                      equiv))))))

;;; FIXME: assumes hash-table-set! goes right to left.

(define (hash-table comparator . rest)
  (let ((ht (apply make-hash-table comparator rest)))
    (apply hash-table-set!
           ht
           rest)
    ht))

;;; Returns an immutable hash table.

(define (hash-table-tabulate comparator n proc)
  (let ((ht (make-hash-table comparator)))
    (do ((i 0 (+ i 1)))
        ((= i n)
         (hashtable-copy ht))
      (call-with-values
       (lambda ()
         (proc i))
       (lambda (key val)
         (hash-table-set! ht key val))))))

(define (hash-table-unfold stop? mapper successor seed comparator . rest)
  (let ((ht (apply make-hash-table comparator rest)))
    (let loop ((seed seed))
      (if (stop? seed)
          ht
          (call-with-values
           (lambda () (mapper seed))
           (lambda (key val)
             (hash-table-set! ht key val)
             (loop (successor seed))))))))

(define (alist->hash-table alist comparator/equiv . rest)
  (let ((ht (apply make-hash-table comparator/equiv rest))
        (entries (reverse alist)))
    (for-each (lambda (entry)
                (hash-table-set! ht (car entry) (cdr entry)))
              entries)
    ht))

;;; Predicates.

(define (hash-table? obj)
  (hashtable? obj))

(define (hash-table-contains? ht key)
  (hashtable-contains? ht key))

(define (hash-table-empty? ht)
  (= 0 (hashtable-size ht)))

;;; FIXME: walks both hash tables because their key comparators
;;; might be different

(define (hash-table=? value-comparator ht1 ht2)
  (let ((val=? (comparator-equality-predicate value-comparator))
        (n1 (hash-table-size ht1))
        (n2 (hash-table-size ht2)))
    (and (= n1 n2)
         (hash-table-every (lambda (key val1)
                             (and (hashtable-contains? ht2 key)
                                  (val=? val1
                                         (hashtable-ref ht2 key 'ignored))))
                           ht1)
         (hash-table-every (lambda (key val2)
                             (and (hashtable-contains? ht1 key)
                                  (val=? val2
                                         (hashtable-ref ht1 key 'ignored))))
                           ht2))))

(define (hash-table-mutable? ht)
  (hashtable-mutable? ht))

;;; Accessors.

(define (hash-table-ref ht key . rest)
  (let ((failure (if (null? rest) #f (car rest)))
        (success (if (or (null? rest) (null? (cdr rest))) #f (cadr rest)))
        (val (hashtable-ref ht key %not-found)))
    (cond ((eq? val %not-found)
           (if (and failure (procedure? failure))
               (failure)
               (error %not-found-message ht key %not-found-irritant)))
          (success
           (success val))
          (else
           val))))

(define (hash-table-ref/default ht key default)
  (hashtable-ref ht key default))

;;; Mutators.

(define (hash-table-set! ht . rest)
  (if (= 2 (length rest))
      (hashtable-set! ht (car rest) (cadr rest))
      (let ((revrest (reverse rest)))
        (let loop ((revrest revrest))
          (cond ((and (not (null? revrest))
                      (not (null? (cdr revrest))))
                 (hashtable-set! ht (cadr revrest) (car revrest))
                 (loop (cddr revrest)))
                ((not (null? revrest))
                 (error "hash-table-set!: wrong number of arguments"
                        (cons ht rest))))))))

;;; hash-table-set-entries! goes left-to-right instead of right-to-left
;;; like hash-table-set!.

(define (hash-table-set-entries! ht keys vals)
  (define (htset! key val)
    (hashtable-set! ht key val))
  (for-each htset! keys vals))

(define (hash-table-delete! ht . keys)
  (hash-table-delete-keys! ht keys))

(define (hash-table-delete-keys! ht keys)
  (for-each (lambda (key)
              (hashtable-delete! ht key))
            keys))

(define (hash-table-extend! ht key . rest)
  (let* ((not-found? (not (hashtable-contains? ht key)))
         (result (apply hash-table-ref ht key rest)))
    (if not-found?
        (hash-table-set! ht key result))
    result))

(define (hash-table-extend!/default ht key default)
  (let* ((not-found? (not (hashtable-contains? ht key)))
         (result (hashtable-ref ht key default)))
    (if not-found?
        (hash-table-set! ht key result))
    result))

(define (hash-table-replace! ht key . rest)
  (let* ((found? (hashtable-contains? ht key))
         (result (apply hash-table-ref ht key rest)))
    (if found?
        (hash-table-set! ht key result))
    result))

(define (hash-table-replace!/default ht key . rest)
  (let* ((found? (hashtable-contains? ht key))
         (result (apply hash-table-ref ht key rest)))
    (if found?
        (hash-table-set! ht key result))
    result))

(define (hash-table-update! ht key updater . rest)
  (hash-table-set! ht
                   key
                   (updater (apply hash-table-ref ht key rest))))

(define (hash-table-update!/default ht key updater default)
  (hash-table-set! ht key (updater (hashtable-ref ht key default))))

(define (hash-table-push! ht key val failure)
  (let ((x (hashtable-ref ht key %not-found)))
    (if (eq? x %not-found)
        (hash-table-set! ht key (failure))
        (hash-table-set! ht key (cons val x)))))

(define (hash-table-pop! ht key failure)
  (let ((x (hashtable-ref ht key %not-found)))
    (cond ((eq? x %not-found)
           (failure))
          ((not (pair? x))
           (error "hash-table-pop!: current value not a pair" ht key x))
          (else
           (hashtable-set! ht key (cdr x))
           (car x)))))

(define (hash-table-search! ht key failure success)
  (let ((x (hashtable-ref ht key %not-found)))
    (if (eq? x %not-found)
        (failure (lambda (val) (hash-table-set! ht key val)))
        (success x
                 (lambda (val) (hash-table-set! ht key val))
                 (lambda () (hashtable-delete! ht key))))))

(define (hash-table-clear! ht)
  (hashtable-clear! ht))

;;; The whole hash table.

(define (hash-table-size ht)
  (hashtable-size ht))

(define (hash-table-keys ht)
  (vector->list (hashtable-keys ht)))

(define (hash-table-values ht)
  (call-with-values
   (lambda () (hashtable-entries ht))
   (lambda (keys vals)
     (vector->list vals))))

(define (hash-table-entries ht)
  (call-with-values
   (lambda () (hashtable-entries ht))
   (lambda (keys vals)
     (values (vector->list keys)
             (vector->list vals)))))

(define (hash-table-find ht proc failure)
  (call-with-values
   (lambda () (hash-table-entries ht))
   (lambda (keys vals)
     (let loop ((keys keys)
                (vals vals))
       (if (null? keys)
           (failure)
           (let* ((key (car keys))
                  (val (car vals))
                  (x   (proc key val)))
             (or x
                 (loop (cdr keys)
                       (cdr vals)))))))))

(define (hash-table-count ht pred)
  (call-with-values
   (lambda () (hash-table-entries ht))
   (lambda (keys vals)
     (let loop ((keys keys)
                (vals vals)
                (n 0))
       (if (null? keys)
           n
           (let* ((key (car keys))
                  (val (car vals))
                  (x   (pred key val)))
             (loop (cdr keys)
                   (cdr vals)
                   (if x (+ n 1) n))))))))

(define (hash-table-any proc ht)
  (call-with-values
   (lambda () (hash-table-entries ht))
   (lambda (keys vals)
     (let loop ((keys keys)
                (vals vals))
       (if (null? keys)
           #f
           (let* ((key (car keys))
                  (val (car vals))
                  (x   (proc key val)))
             (or x
                 (loop (cdr keys)
                       (cdr vals)))))))))

(define (hash-table-every proc ht)
  (call-with-values
   (lambda () (hash-table-entries ht))
   (lambda (keys vals)
     (let loop ((keys keys)
                (vals vals))
       (if (null? keys)
           #t
           (let* ((key (car keys))
                  (val (car vals))
                  (x   (proc key val)))
             (and x
                  (loop (cdr keys)
                        (cdr vals)))))))))

;;; Mapping and folding.

(define (hash-table-map proc comparator merger ht)
  (let ((result (make-hash-table comparator)))
    (hash-table-for-each
     (lambda (key val)
       (call-with-values
        (lambda () (proc key val))
        (lambda (key1 val1)
          (let ((key1 (if (hashtable-contains? result key1)
                          (merger key val key1 val1)
                          key1)))
            (hashtable-set! result key1 val1)))))
     ht)
    result))

(define (hash-table-map-values proc comparator ht)
  (let ((result (make-hash-table comparator)))
    (hash-table-for-each
     (lambda (key val)
       (hashtable-set! result key (proc val)))
     ht)
    result))

;;; With this particular implementation, the proc can safely mutate ht.
;;; That property is not guaranteed by the specification, but can be
;;; relied upon by procedures defined in this file.

(define (hash-table-for-each proc ht)
  (call-with-values
   (lambda () (hashtable-entries ht))
   (lambda (keys vals)
     (vector-for-each proc keys vals))))

(define (hash-table-map! proc ht)
  (hash-table-for-each (lambda (key val)
                         (hashtable-set! ht key (proc key val)))
                       ht))

(define (hash-table-collect proc ht)
  (call-with-values
   (lambda () (hash-table-entries ht))
   (lambda (keys vals)
     (map proc keys vals))))

(define (hash-table-fold proc init ht)
  (call-with-values
   (lambda () (hash-table-entries ht))
   (lambda (keys vals)
     (let loop ((keys keys)
                (vals vals)
                (x    init))
       (if (null? keys)
           x
           (loop (cdr keys)
                 (cdr vals)
                 (proc (car keys) (car vals) x)))))))

(define (hash-table-filter! proc ht)
  (hash-table-for-each (lambda (key val)
                         (if (not (proc key val))
                             (hashtable-delete! ht key)))
                       ht)
  ht)

(define (hash-table-remove! proc ht)
  (hash-table-for-each (lambda (key val)
                         (if (proc key val)
                             (hashtable-delete! ht key)))
                       ht)
  ht)

;;; Copying and conversion.

(define (hash-table-copy ht . rest)
  (apply hashtable-copy ht rest))

(define (hash-table->alist ht)
  (hash-table-collect cons ht))

;;; Hash tables as functions.

(define (hash-table-accessor ht . rest)
  (cond ((null? rest)
         (lambda (key) (hash-table-ref ht key)))
        ((null? (cdr rest))
         (let ((failure (car rest)))
           (lambda (key) (hash-table-ref ht key failure))))
        (else
         (let ((failure (car rest))
               (success (cadr rest)))
           (lambda (key) (hash-table-ref ht key failure success))))))

(define (hash-table-accessor/default ht default)
  (lambda (key) (hash-table-ref/default ht key default)))

;;; Hash tables as sets.

(define (hash-table-union! ht1 ht2)
  (hash-table-for-each
   (lambda (key2 val2)
     (if (not (hashtable-contains? ht1 key2))
         (hashtable-set! ht1 key2 val2)))
   ht2)
  ht1)

(define (hash-table-intersection! ht1 ht2)
  (hash-table-for-each
   (lambda (key1 val1)
     (if (not (hashtable-contains? ht2 key1))
         (hashtable-delete! ht1 key1)))
   ht1)
  ht1)

(define (hash-table-difference! ht1 ht2)
  (hash-table-for-each
   (lambda (key1 val1)
     (if (hashtable-contains? ht2 key1)
         (hashtable-delete! ht1 key1)))
   ht1)
  ht1)

(define (hash-table-xor! ht1 ht2)
  (hash-table-for-each
   (lambda (key2 val2)
     (if (hashtable-contains? ht1 key2)
         (hashtable-delete! ht1 key2)
         (hashtable-set! ht1 key2 val2)))
   ht2)
  ht1)

; eof
