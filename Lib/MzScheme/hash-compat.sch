
(define hashtable-tag-object
  (vector-ref (make-hashtable) 0))

(define (make-hash-table . flags)
  (make-hashtable))

(define (hash-table-get h k . args)
  (if (hashtable-contains? h k)
      (hashtable-get h k)
      (if (null? args)
          (error "hash-table-get: no entry for key: " k)
          ((car args)))))

(define (hash-table-put! h k v)
  (hashtable-put! h k v))

(define (hash-table-remove! h k)
  (hashtable-remove! h k))
(define (hash-table? v)
  (and (vector? v)
       (= 5 (vector-length v))
       (eq? hashtable-tag-object
            (vector-ref v 0))))

(define (hash-table-map h f)
  (hashtable-map h f))
(define (hash-table-for-each h f)
  (hashtable-for-each h f))
