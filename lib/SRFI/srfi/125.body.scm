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

;;; Ten of the SRFI 125 procedures are deprecated, and another
;;; two allow alternative arguments that are deprecated.

(define (issue-deprecated-warnings?) #t)

(define (issue-warning-deprecated name-of-deprecated-misfeature)
  (if (not (memq name-of-deprecated-misfeature already-warned))
      (begin
       (set! already-warned
             (cons name-of-deprecated-misfeature already-warned))
       (if (issue-deprecated-warnings?)
           (let ((out (current-error-port)))
             (display "WARNING: " out)
             (display name-of-deprecated-misfeature out)
             (newline out)
             (display "    is deprecated by SRFI 125.  See" out)
             (newline out)
             (display "    " out)
             (display url:deprecated out)
             (newline out))))))

(define url:deprecated
  "http://srfi.schemers.org/srfi-125/srfi-125.html")

; List of deprecated features for which a warning has already
; been issued.

(define already-warned '())

;;; Comparators contain a type test predicate, which implementations
;;; of the hash-table-set! procedure can use to reject invalid keys.
;;; That's hard to do without sacrificing interoperability with R6RS
;;; and/or SRFI 69 and/or SRFI 126 hash tables.
;;;
;;; Full interoperability means the hash tables implemented here are
;;; interchangeable with the SRFI 126 hashtables used to implement them.
;;; SRFI 69 and R6RS and SRFI 126 hashtables don't contain comparators,
;;; so any association between a hash table and its comparator would have
;;; to be maintained outside the representation of hash tables themselves,
;;; which is problematic unless weak pointers are available.
;;;
;;; Not all of the hash tables implemented here will have comparators
;;; associated with them anyway, because an equivalence procedure
;;; and hash function can be used to create a hash table instead of
;;; a comparator (although that usage is deprecated by SRFI 125).
;;;
;;; One way to preserve interoperability while enforcing a comparator's
;;; type test is to incorporate that test into a hash table's hash
;;; function.  The advantage of doing that should be weighed against
;;; these disadvantages:
;;;
;;;     If the type test is slow, then hashing would also be slower.
;;;
;;;     The R6RS, SRFI 69, and SRFI 126 APIs allow extraction of
;;;     a hash function from some hash tables.
;;;     Some programmers might expect that hash function to be the
;;;     hash function encapsulated by the comparator (in the sense
;;;     of eq?, perhaps) even though this API makes no such guarantee
;;;     (and extraction of that hash function from an existing hash
;;;     table can only be done by calling a deprecated procedure).

;;; If %enforce-comparator-type-tests is true, then make-hash-table,
;;; when passed a comparator, will use a hash function that enforces
;;; the comparator's type test.

(define %enforce-comparator-type-tests #t)

;;; Given a comparator, return its hash function, possibly augmented
;;; by the comparator's type test.

(define (%comparator-hash-function comparator)
  (let ((okay? (comparator-type-test-predicate comparator))
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

;;; A unique (in the sense of eq?) value that escapes only as an irritant
;;; when a hash-table key is not found.

(define %not-found-irritant (list 'not-found))

;;; The error message used when a hash-table key is not found.

(define %not-found-message "hash-table key not found")

;;; FIXME: thread-safe, weak-keys, ephemeral-keys, weak-values,
;;; and ephemeral-values are not supported by this portable
;;; reference implementation.

(define (%check-optional-arguments procname args)
  (if (or (memq 'thread-safe args)
          (memq 'weak-keys args)
          (memq 'weak-values args)
          (memq 'ephemeral-keys args)
          (memq 'ephemeral-values args))
      (error (string-append (symbol->string procname)
                            ": unsupported optional argument(s)")
             args)))

;;; This was exported by an earlier draft of SRFI 125,
;;; and is still used by hash-table=?

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exported procedures
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constructors.

;;; The first argument can be a comparator or an equality predicate.
;;;
;;; If the first argument is a comparator, any remaining arguments
;;; are implementation-dependent, but a non-negative exact integer
;;; should be interpreted as an initial capacity and the symbols
;;; thread-safe, weak-keys, ephemeral-keys, weak-values, and
;;; emphemeral-values should be interpreted specially.  (These
;;; special symbols are distinct from the analogous special symbols
;;; in SRFI 126.)
;;;
;;; If the first argument is not a comparator, then it had better
;;; be an equality predicate (which is deprecated by SRFI 125).
;;; If a second argument is present and is a procedure, then it's
;;; a hash function (which is allowed only for the deprecated case
;;; in which the first argument is an equality predicate).  If a
;;; second argument is not a procedure, then it's some kind of
;;; implementation-dependent optional argument, as are all arguments
;;; beyond the second.
;;;
;;; SRFI 128 defines make-eq-comparator, make-eqv-comparator, and
;;; make-equal-comparator procedures whose hash function is the
;;; default-hash procedure of SRFI 128, which is inappropriate
;;; for use with eq? and eqv? unless the object being hashed is
;;; never mutated.  Neither SRFI 125 nor 128 provide any way to
;;; define a comparator whose hash function is truly compatible
;;; with the use of eq? or eqv? as an equality predicate.
;;;
;;; That would make SRFI 125 almost as bad as SRFI 69 if not for
;;; the following paragraph of SRFI 125:
;;;
;;;     Implementations are permitted to ignore user-specified
;;;     hash functions in certain circumstances. Specifically,
;;;     if the equality predicate, whether passed as part of a
;;;     comparator or explicitly, is more fine-grained (in the
;;;     sense of R7RS-small section 6.1) than equal?, the
;;;     implementation is free — indeed, is encouraged — to
;;;     ignore the user-specified hash function and use something
;;;     implementation-dependent. This allows the use of addresses
;;;     as hashes, in which case the keys must be rehashed if
;;;     they are moved by the garbage collector. Such a hash
;;;     function is unsafe to use outside the context of
;;;     implementation-provided hash tables. It can of course be
;;;     exposed by an implementation as an extension, with
;;;     suitable warnings against inappropriate uses.
;;;
;;; That gives implementations permission to do something more
;;; useful, but when should implementations take advantage of
;;; that permission?  This implementation uses the superior
;;; solution provided by SRFI 126 whenever:
;;;
;;;     A comparator is passed as first argument and its equality
;;;     predicate is eq? or eqv?.
;;;
;;;     The eq? or eqv? procedure is passed as first argument
;;;     (which is a deprecated usage).

(define (make-hash-table comparator/equiv . rest)
  (if (comparator? comparator/equiv)
      (let ((equiv (comparator-equality-predicate comparator/equiv))
            (hash-function (%comparator-hash-function comparator/equiv)))
        (%make-hash-table equiv hash-function rest))
      (let* ((equiv comparator/equiv)
             (hash-function (if (and (not (null? rest))
                                     (procedure? (car rest)))
                                (car rest)
                                #f))
             (rest (if hash-function (cdr rest) rest)))
        (issue-warning-deprecated 'srfi-69-style:make-hash-table)
        (%make-hash-table equiv hash-function rest))))

(define (%make-hash-table equiv hash-function opts)
  (%check-optional-arguments 'make-hash-table opts)
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
                equiv))))

;;; FIXME: assumes hash-table-set! goes right to left.

(define (hash-table comparator . rest)
  (let ((ht (apply make-hash-table comparator rest)))
    (apply hash-table-set!
           ht
           rest)
    ht))

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
  (if (and (not (null? rest))
           (procedure? (car rest)))
      (issue-warning-deprecated 'srfi-69-style:alist->hash-table))
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

(define (hash-table-delete! ht . keys)
  (for-each (lambda (key)
              (hashtable-delete! ht key))
            keys))

(define (hash-table-intern! ht key failure)
  (if (hashtable-contains? ht key)
      (hash-table-ref ht key)
      (let ((val (failure)))
        (hash-table-set! ht key val)
        val)))

(define (hash-table-update! ht key updater . rest)
  (hash-table-set! ht
                   key
                   (updater (apply hash-table-ref ht key rest))))

(define (hash-table-update!/default ht key updater default)
  (hash-table-set! ht key (updater (hashtable-ref ht key default))))

(define (hash-table-pop! ht)
  (call/cc
    (lambda (return)
      (hash-table-for-each
        (lambda (key value)
          (hash-table-delete! ht key)
          (return key value))
        ht))))

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

(define (hash-table-find proc ht failure)
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

(define (hash-table-count pred ht)
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

;;; Mapping and folding.

(define (hash-table-map proc comparator ht)
  (let ((result (make-hash-table comparator)))
    (hash-table-for-each
     (lambda (key val)
       (hash-table-set! result key (proc val)))
     ht)
    result))

(define (hash-table-map->list proc ht)
  (call-with-values
   (lambda () (hash-table-entries ht))
   (lambda (keys vals)
     (map proc keys vals))))

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

(define (hash-table-fold proc init ht)
  (if (hashtable? proc)
      (deprecated:hash-table-fold proc init ht)
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
                     (proc (car keys) (car vals) x))))))))

(define (hash-table-prune! proc ht)
  (hash-table-for-each (lambda (key val)
                         (if (proc key val)
                             (hashtable-delete! ht key)))
                       ht))

;;; Copying and conversion.

(define (hash-table-copy ht . rest)
  (apply hashtable-copy ht rest))

(define (hash-table-empty-copy ht)
  (let* ((ht2 (hashtable-copy ht #t))
         (ignored (hashtable-clear! ht2)))
    (if (hashtable-mutable? ht)
        ht2
        (hashtable-copy ht2))))

(define (hash-table->alist ht)
  (call-with-values
   (lambda () (hash-table-entries ht))
   (lambda (keys vals)
     (map cons keys vals))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The following procedures are deprecated by SRFI 125, but must
;;; be exported nonetheless.
;;;
;;; Programs that import the (srfi 125) library must rename the
;;; deprecated string-hash and string-ci-hash procedures to avoid
;;; conflict with the string-hash and string-ci-hash procedures
;;; exported by SRFI 126 and SRFI 128.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deprecated:hash obj . rest)
  (issue-warning-deprecated 'hash)
  (default-hash obj))

(define (deprecated:string-hash obj . rest)
  (issue-warning-deprecated 'srfi-125:string-hash)
  (string-hash obj))

(define (deprecated:string-ci-hash obj . rest)
  (issue-warning-deprecated 'srfi-125:string-ci-hash)
  (string-ci-hash obj))

(define (deprecated:hash-by-identity obj . rest)
  (issue-warning-deprecated 'hash-by-identity)
  (deprecated:hash obj))

(define (deprecated:hash-table-equivalence-function ht)
  (issue-warning-deprecated 'hash-table-equivalence-function)
  (hashtable-equivalence-function ht))

(define (deprecated:hash-table-hash-function ht)
  (issue-warning-deprecated 'hash-table-hash-function)
  (hashtable-hash-function ht))

(define (deprecated:hash-table-exists? ht key)
  (issue-warning-deprecated 'hash-table-exists?)
  (hash-table-contains? ht key))

(define (deprecated:hash-table-walk ht proc)
  (issue-warning-deprecated 'hash-table-walk)
  (hash-table-for-each proc ht))

(define (deprecated:hash-table-fold ht proc seed)
  (issue-warning-deprecated 'srfi-69-style:hash-table-fold)
  (hash-table-fold proc seed ht))

(define (deprecated:hash-table-merge! ht1 ht2)
  (issue-warning-deprecated 'hash-table-merge!)
  (hash-table-union! ht1 ht2))

; eof
