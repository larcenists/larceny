; Deep copying with shared structure, for Larceny.
;
; Problem: some objects are used for their identity and should _not_ be
; copied, eg record type descriptors.  These objects could be said to be
; "atomic", perhaps, so it may be possible to have some "atomic" predicate
; that handles those cases.
;
; Does not copy procedures, though they are mutable.  It seems needless
; to copy them and is probably not what we want (especially for applyhooks).

(define (deep-copy x)

  (define ht
    (make-oldstyle-hashtable equal-hash assq))

  (define (lookup x)
    (hashtable-get ht x))

  (define (insert x new)
    (hashtable-put! ht x new))

  (define (copy x)
    (or (lookup x)
        (cond ((pair? x) 
               (let ((p (cons '() '())))
                 (insert x p)
                 (set-car! p (copy (car x)))
                 (set-cdr! p (copy (cdr x)))
                 p))
              ((or (vector? x) (structure? x))
               (let* ((l (vector-like-length x))
                      (v (make-vector l)))
                 (insert x v)
                 (do ((i 0 (+ i 1)))
                     ((= i l) 
                      (typetag-set! v (typetag-ref x))
                      v)
                   (vector-like-set! v i (copy (vector-like-ref x i))))))
              ((or (bytevector? x) (string? x))
               (let ((v (bytevector-like-copy x)))
                 (insert x v)
                 v))
              (else x))))

  (copy x))

; eof
