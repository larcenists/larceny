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

;;; This implementation of bimaps works only with the reference
;;; implementation of hash tables on top of (r6rs hashtables).

;;; A bimap is just a pair of hash tables with additional invariants.
;;;
;;; Let the hash tables be ht1 and ht2, with equivalence procedures
;;; equiv1 and equiv2 respectively.
;;;
;;; Invariants:
;;;
;;;     Every key in ht1 is accepted as an argument by equiv1.
;;;
;;;     Every key in ht2 is accepted as an argument by equiv2.
;;;
;;;     Every value in ht1 is accepted as an argument by equiv2.
;;;
;;;     Every value in ht2 is accepted as an argument by equiv1.
;;;
;;;     If x is a key of ht1, then its associated value is a key of ht2
;;;     (in the sense of equiv1).
;;;
;;;     If y is a key of ht2, then its associated value is a key of ht1
;;;     (in the sense of equiv2).
;;;
;;;     If x and y are distinct keys of ht1 (meaning they are listed
;;;     separately by hash-table-keys), then their associated values
;;;     are not equivalent according to equiv2.
;;;
;;;     If x and y are distinct keys of ht2 (meaning they are listed
;;;     separately by hash-table-keys), then their associated values
;;;     are not equivalent according to equiv1.
;;;
;;; These invariants are checked when a bimap is constructed, and
;;; preserved by operations on bimaps.  The basic operations that
;;; preserve these invariants are bimap-set! and bimap-delete!.
;;; All other mutators (apart from bimap-clear!) are defined using
;;; bimap-set! and bimap-delete!.
;;;
;;; FIXME:  As noted below, mutating ht1 can break these invariants,
;;; but it is an error to mutate ht1 after the bimap is created.

;;; Private to this file.

;;; A unique (in the sense of eq?) value that will never be found
;;; within a hash-table.

(define %not-found (list '%not-found))

(define (%check-optional-arguments procname args)
  (if (or (memq 'thread-safe args)
          (memq 'weak-keys args)
          (memq 'weak-values args))
      (error (string-append (symbol->string procname)
                            ": unsupported optional argument(s)")
             args)))

;;; The :bimap type and raw-make-bimap procedure are private to this file,
;;; but bimap?, bimap-forward-hash-table, and bimap-reverse-hash-table
;;; are exported, as are all other procedures defined below.

(define-record-type :bimap
  (raw-make-bimap ht1 ht2)
  bimap?
  (ht1 bimap-forward-hash-table)
  (ht2 bimap-reverse-hash-table))

;;; FIXME:  Why is ht1 required to share structure with the bimap?
;;; It would be cleaner to copy ht1 when the bimap is created.

(define (make-bimap ht1 comparator . rest)
  (%check-optional-arguments 'make-bimap rest)
  (let ((ht2 (make-hash-table comparator)))
    (hash-table-for-each (lambda (key val)
                           (if (hash-table-contains? ht2 val)
                               (error "make-bimap: duplicate value"
                                      val
                                      (hash-table-ref/default ht2 val #f)
                                      key)
                               (hash-table-set! ht2 val key)))
                         ht1)
    (raw-make-bimap ht1 ht2)))

(define (bimap-contains? bimap key)
  (hash-table-contains? (bimap-forward-hash-table bimap) key))

(define (bimap-contains-value? bimap value)
  (hash-table-contains? (bimap-reverse-hash-table bimap) value))

;;; "Returns #t if bimap1 and bimap2 contain the same key-value
;;; associations, and #f otherwise."
;;;
;;; In this implementation, two key-value associations k1/v1 and k2/v2
;;; are the same if and only if v1 and v2 reverse-map (in both tables)
;;; to keys that are the same in the sense of the equivalence procedures
;;; of both tables.
;;;
;;; This implementation uses eq? to eliminate some procedure calls
;;; in special cases, which assumes equivalence procedures have no
;;; observable state and do not distinguish objects eq? cannot.

(define (bimap=? bimap1 bimap2)

  (define (half-check ht11 ht12 ht21 ht22)
    (let ((equiv1 (hashtable-equivalence-function ht11))
          (equiv2 (hashtable-equivalence-function ht21)))
      (hash-table-every
       (lambda (key1 val1)
         (and (hash-table-contains? ht21 key1)
              (let ((val2 (hash-table-ref ht21 key1)))
                (and (hash-table-contains? ht12 val1)
                     (hash-table-contains? ht22 val1)
                     (or (eq? val1 val2)
                         (hash-table-contains? ht12 val2)
                         (hash-table-contains? ht22 val2))
                     (let* ((k1 (hash-table-ref ht12 val1))
                            (k2 (hash-table-ref ht22 val1))
                            (k3 (if (eq? val1 val2)
                                    k1
                                    (hash-table-ref ht12 val2)))
                            (k4 (if (eq? val1 val2)
                                    k2
                                    (hash-table-ref ht22 val2))))
                       (and (equiv1 k1 k2)
                            (or (eq? val1 val2)
                                (and (equiv1 k2 k3)
                                     (equiv1 k3 k4)))
                            (or (eq? equiv1 equiv2)
                                (and (equiv2 k1 k2)
                                     (equiv2 k2 k3)
                                     (equiv2 k3 k4)))))))))
       ht11)))

  (let ((ht11 (bimap-forward-hash-table bimap1))
        (ht12 (bimap-reverse-hash-table bimap1))
        (ht21 (bimap-forward-hash-table bimap2))
        (ht22 (bimap-reverse-hash-table bimap2)))

    (and (half-check ht11 ht12 ht21 ht22)
         (half-check ht21 ht22 ht11 ht12))))         

(define (bimap-ref bimap key . rest)
  (apply hash-table-ref (bimap-forward-hash-table bimap) key rest))

(define (bimap-value-ref bimap value . rest)
  (apply hash-table-ref (bimap-reverse-hash-table bimap) value rest))

(define (bimap-ref/default bimap key default)
  (hash-table-ref/default (bimap-forward-hash-table bimap) key default))

(define (bimap-value-ref/default bimap value default)
  (hash-table-ref/default (bimap-reverse-hash-table bimap) value default))

(define (bimap-copy bimap . rest)
  (raw-make-bimap
   (apply hashtable-copy (bimap-forward-hash-table bimap) rest)
   (apply hashtable-copy (bimap-reverse-hash-table bimap) rest)))

;;; Let ht1 and ht2 be the forward and reverse hash tables of bimap.
;;; To preserve bimap invariants, (bimap-set! bimap key val) must delete
;;; 
;;;     any entry for key within ht1
;;;         and also the entry for key's value within ht2
;;;     any entry for val within ht2
;;;         and also the entry for val's value within ht1
;;;
;;; before it adds new entries to ht1 and ht2.
;;;
;;; FIXME: This implementation goes right to left, which may be unnecessary.

(define (bimap-set! bimap . rest)
  (if (= 2 (length rest))
      (let* ((key (car rest))
             (val (cadr rest))
             (ht1 (bimap-forward-hash-table bimap))
             (ht2 (bimap-reverse-hash-table bimap))
             (v (hashtable-ref ht1 key %not-found))
             (k (hashtable-ref ht2 val %not-found)))
        (cond ((and (eq? k %not-found) (eq? v %not-found))
               (hashtable-set! ht1 key val)
               (hashtable-set! ht2 val key))
              ((eq? k %not-found)
               (let ((oldk (hashtable-ref ht2 v %not-found)))
                 (hashtable-delete! ht2 v)
                 (unless (eq? oldk %not-found)
                         (hashtable-delete! ht1 oldk))
                 (bimap-set! bimap key val)))
              ((eq? v %not-found)
               (let ((oldv (hashtable-ref ht1 k %not-found)))
                 (hashtable-delete! ht1 k)
                 (unless (eq? oldv %not-found)
                         (hashtable-delete! ht2 oldv))
                 (bimap-set! bimap key val)))
              (else
               (let ((oldv (hashtable-ref ht1 k %not-found))
                     (oldk (hashtable-ref ht2 v %not-found)))
                 (hashtable-delete! ht1 k)
                 (hashtable-delete! ht2 v)
                 (hashtable-delete! ht1 oldk)
                 (hashtable-delete! ht2 oldv)
                 (unless (eq? key oldk)
                         (hashtable-delete! ht1 key))
                 (unless (eq? val oldv)
                         (hashtable-delete! ht2 val))
                 (bimap-set! bimap key val)))))
      (let ((revrest (reverse rest)))
        (let loop ((revrest revrest))
          (cond ((and (not (null? revrest))
                      (not (null? (cdr revrest))))
                 (bimap-set! bimap (cadr revrest) (car revrest))
                 (loop (cddr revrest)))
                ((not (null? revrest))
                 (error "bimap-set!: wrong number of arguments"
                        (cons bimap rest))))))))

;;; bimap-set-entries! goes left-to-right instead of right-to-left
;;; like bimap-set!.

(define (bimap-set-entries! bimap keys vals)
  (define (bmset! key val)
    (bimap-set! bimap key val))
  (for-each bmset! keys vals))
  
(define (bimap-delete! bimap . keys)
  (bimap-delete-keys! bimap keys))

;;; Let ht1 and ht2 be the forward and reverse hash tables of bimap.
;;; To preserve bimap invariants, deleting a key from bimap involves
;;; deleting that key's value from ht2 as well as deleting that key
;;; from ht1.

(define (bimap-delete-keys! bimap keys-list)
  (let ((ht1 (bimap-forward-hash-table bimap))
        (ht2 (bimap-reverse-hash-table bimap)))
    (for-each (lambda (key)
                (let ((val (hashtable-ref ht1 key %not-found)))
                  (if (not (eq? val %not-found))
                      (let ((oldk (hashtable-ref ht2 val %not-found)))
                        (unless (eq? key oldk)
                                (hashtable-delete! ht1 oldk))
                        (hashtable-delete! ht1 key)
                        (hashtable-delete! ht2 val)))))
              keys-list)))

(define (bimap-extend! bimap key . rest)
  (let* ((not-found? (not (bimap-contains? bimap key)))
         (result (apply bimap-ref bimap key rest)))
    (if not-found?
        (bimap-set! bimap key result))
    result))

(define (bimap-extend/default! bimap key default)
  (let* ((not-found? (not (bimap-contains? bimap key)))
         (result (bimap-ref/default bimap key default)))
    (if not-found?
        (bimap-set! bimap key result))
    result))

(define (bimap-replace! bimap key . rest)
  (let* ((found? (bimap-contains? bimap key))
         (result (apply bimap-ref bimap key rest)))
    (if found?
        (bimap-set! bimap key result))
    result))

(define (bimap-replace/default! bimap key default)
  (let* ((found? (bimap-contains? bimap key))
         (result (bimap-ref/default bimap key default)))
    (if found?
        (bimap-set! bimap key result))
    result))

(define (bimap-update! bimap key updater . rest)
  (bimap-set! bimap
              key
              (updater (apply bimap-ref bimap key rest))))

(define (bimap-update/default! bimap key updater default)
  (bimap-set! bimap key (updater (bimap-ref/default bimap key default))))

(define (bimap-clear! bimap)
  (hashtable-clear! (bimap-forward-hash-table bimap))
  (hashtable-clear! (bimap-reverse-hash-table bimap)))

(define (bimap-filter! proc bimap)
  (hash-table-for-each (lambda (key val)
                         (if (not (proc key val))
                             (bimap-delete! bimap key)))
                       (bimap-forward-hash-table bimap))
  bimap)

(define (bimap-remove! proc bimap)
  (hash-table-for-each (lambda (key val)
                         (if (proc key val)
                             (bimap-delete! bimap key)))
                       (bimap-forward-hash-table bimap))
  bimap)

