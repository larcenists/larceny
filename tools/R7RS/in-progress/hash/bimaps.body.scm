(define-record-type :bimap
  (raw-make-bimap ht1 ht2)
  bimap?
  (ht1 bimap-forward-hash-table)
  (ht2 bimap-reverse-hash-table))

;;; FIXME:  Why is ht1 required to share structure with the bimap?
;;; It would be cleaner to copy ht1 when the bimap is created, and
;;; it would make several other operations faster because they
;;; wouldn't have to guard against the possibility that ht1 had
;;; been mutated despite the contract.

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
;;; FIXME: what does "same" mean?
;;;
;;; In this implementation, two key-value associations k1/v1 and k2/v2
;;; are the same if and only if v1 and v2 reverse-map (in both tables)
;;; to keys that are the same in the sense of eqv?.

(define (bimap=? bimap1 bimap2)

  (define (half-check ht11 ht12 ht21 ht22)
    (hash-table-every (lambda (key1 val1)
                        (and (hash-table-contains? ht21 key1)
                             (let ((val2 (hash-table-ref ht21 key1)))
                               (hash-table-contains? ht12 val1)
                               (hash-table-contains? ht22 val1)
                               (hash-table-contains? ht12 val2)
                               (hash-table-contains? ht22 val2)
                               (let ((k1 (hash-table-ref ht12 val1))
                                     (k2 (hash-table-ref ht22 val1))
                                     (k3 (hash-table-ref ht12 val2))
                                     (k4 (hash-table-ref ht22 val2)))
                                 (and (eqv? k1 k2)
                                      (eqv? k2 k3)
                                      (eqv? k3 k4))))))
                      ht11))

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
              (else
               (let ((oldv (hashtable-ref ht1 k %not-found))
                     (oldk (hashtable-ref ht2 v %not-found)))
                 (hashtable-delete! ht1 k)
                 (hashtable-delete! ht2 v)
                 (hashtable-delete! ht1 oldk)
                 (hashtable-delete! ht2 oldv)
                 (hashtable-delete! ht1 key)
                 (hashtable-delete! ht2 val)
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

(define (bimap-set-entries! bimap keys-list values-list)
  (define (bmset! key val)
    (bimap-set! bimap key val))
  (for-each bmset! keys vals))
  
(define (bimap-delete! bimap . keys)
  (bimap-delete-keys! bimap keys))

(define (bimap-delete-keys! bimap keys-list)
  (let ((ht1 (bimap-forward-hash-table bimap))
        (ht2 (bimap-reverse-hash-table bimap)))
    (for-each (lambda (key)
                (let ((val (hashtable-ref ht1 key %not-found)))
                  (if (not (eq? val %not-found))
                      (let ((oldk (hashtable-ref ht2 val %not-found)))
                        (hashtable-delete! ht1 oldk)
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

(define (bimap-replace/default! bimap key . rest)
  (let* ((found? (bimap-contains? bimap key))
         (result (apply bimap-ref bimap key rest)))
    (if found?
        (bimap-set! bimap key result))
    result))

(define (bimap-update! bimap key updater . rest)
  (bimap-set! bimap
              key
              (updater (apply bimap-ref ht key rest))))

(define (bimap-update/default! bimap key updater default)
  (bimap-set! bimap key (updater (bimap-ref/default bimap key default))))

(define (bimap-clear! bimap)
  (hashtable-clear! (bimap-forward-hash-table bimap))
  (hashtable-clear! (bimap-reverse-hash-table bimap)))

(define (bimap-filter! proc bimap)
  (hash-table-for-each (lambda (key val)
                         (if (not (proc key val))
                             (bimap-delete! ht key)))
                       (bimap-forward-hash-table bimap)))
  bimap)

(define (bimap-remove! comparator proc bimap)
  (hash-table-for-each (lambda (key val)
                         (if (proc key val)
                             (bimap-delete! ht key)))
                       (bimap-forward-hash-table bimap)))
  bimap)

;;; FIXME: This procedure has no hash-table analogue, so its
;;; specification by reference to that analogue is vacuous.

(define (bimap-partition! bimap)
  'FIXME)

