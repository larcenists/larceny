; Untested.

; The table contains:
;   object is the key
;   id as the value

(define *last-gc-counter* -1)
(define *object-id-table* (make-hashtable object-representation)

(define (object-id object)
  (if (or (pair? object)
          (vector-like? object)
          (procedure? object)
          (bytevector-like? object))
      0                                 ; FIXME -- same as object-hash
      (begin
        (cond ((> (gc-counter) *last-gc-counter*)
               (rehash-table))
              ((time-to-collect-table)
               (collect-table)))
        (set! *last-gc-counter* gc-counter)
        (let ((probe (hashtable-get *object-id-table* 
                                    object)))
          (if probe
              probe
              (let ((id (next-id)))
                (hashtable-put! *object-id-table* 
                                object
                                id)
                id))))))


; This can't create a new table -- it must hack the existing table in
; place.

(define (rehash-table)
  (collect)                             ; Force collection to get
                                        ; maximal effect of the rehash.
  ...)


; Garbage collect the table: remove all associations for objects that
; are only referenced from the table.

; FIXME: this is now broken, must remember both key and value.

(define (collect-table)
  (let* ((n (hashtable-size *object-id-table*))
         (v (make-vector n)))
    (hashtable-for-each (let ((i 0))
                          (lambda (key value)
                            (vector-set! v (+ i 1) value)
                            (set! i (+ i 1))))
                        *object-id-table*)
    (hashtable-clear! *object-id-table*)
    (let* ((objs (sro -1 -1 1))         ; Objects referenced exactly once.
           (l    (vector-length objs)))
      (do ((j 0 (+ j 1)))
          ((= j n))
        (let* ((val (vector-ref v (+ v 1)))
               (obj (car val)))
          (let loop ((i 0))
            (cond ((= i l)
                   (hashtable-put! *object-id-table*
                                   (object-representation (car val))
                                   val))
                  ((eq? (vector-ref objs i) (car val)))
                  (else
                   (loop (+ i 1))))))))))

; (gc-counter) is builtin since 1.0a1 or thereabouts.

; (object-representation x) is actually quite slow and should be fixed.

(define (object-representation object)
  (syscall 36 object))
