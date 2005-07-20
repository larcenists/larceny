; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Record Package.
;
; This is a record package as proposed by RRRS authors but never 
; made part of the report.  This implementation is based on a 
; proposal posted to rrrs-authors on 1 Sep 89 by Pavel Curtis, 
; reposted to comp.lang.scheme by Norman Adams on 5 Feb, 1992.
;
; This implementation extends the proposal in the following ways:
;   * Single inheritance (type extensions)
;   * Record-type-descriptors are records
;   * There are installable record printers
($$trace "record")

(define *record-type-type*)
(define make-record-type)
(define record-type-descriptor?)
(define record-type-field-names)
(define record-type-name)
(define record-type-extends?)
(define record-type-parent)

(define record?)
(define record-constructor)
(define record-predicate)
(define record-accessor)
(define record-updater)
(define record-type-descriptor)

;; Added these to support MzScheme structure interface
(define record-indexer)
(define record-mutator)

(let ((interface

       ; Records of length n are represented as structures where element 0 
       ; contains the record type descriptor, and the other elements hold
       ; the field values.
       ;
       ; A record type descriptor is a record.  Every record type descriptor
       ; is of type *record-type-type*.

       (let ((record-overhead 1))       ; Number of overhead slots.

         ; Records
         
         ; RECORD-WITH-TYPE? assumes rtd is in fact an rtd.

         (define (record-with-type? obj rtd)
           (and (structure? obj)
                (> (vector-like-length obj) 0)
                (let ((slot0 (vector-like-ref obj 0)))
                  (or (eq? rtd slot0)
                      (rtd-extends? slot0 rtd)))))

         (define (record? obj)
           (and (structure? obj)
                (> (vector-like-length obj) 0)
                (record-type-descriptor? (vector-like-ref obj 0))))

         (define (record-constructor rtd fields)
           (assert-rtd rtd)
           (let* ((indices (map (lambda (name)
                                  (rtd-field-offset rtd name))
                                (if (not fields)
                                    (rtd-field-names rtd)
                                    fields)))
                  (n       (length indices))
                  (size    (rtd-record-size rtd))
                  (make-the-record
                   (lambda ()
                     (let ((r (make-structure size)))
                       (vector-like-set! r 0 rtd)
                       r))))
             (case n
               ((1) (let ((a-offset (car indices)))
                      (lambda (a)
                        (let ((r (make-the-record)))
                          (vector-like-set! r a-offset a)
                          r))))
               ((2) (let ((a-offset (car indices))
                          (b-offset (cadr indices)))
                      (lambda (a b)
                        (let ((r (make-the-record)))
                          (vector-like-set! r a-offset a)
                          (vector-like-set! r b-offset b)
                          r))))
               ((3) (let ((a-offset (car indices))
                          (b-offset (cadr indices))
                          (c-offset (caddr indices)))
                      (lambda (a b c)
                        (let ((r (make-the-record)))
                          (vector-like-set! r a-offset a)
                          (vector-like-set! r b-offset b)
                          (vector-like-set! r c-offset c)
                          r))))
               (else
                (lambda values
                  (let ((r (make-the-record)))
                    (do ((indices indices (cdr indices))
                         (values  values  (cdr values)))
                        ((null? indices)
                         (if (not (null? values))
                             (error "Too many arguments to constructor for "
                                    rtd))
                         r)
                      (vector-like-set! r (car indices) (car values)))))))))

         (define (record-predicate rtd)
           (assert-rtd rtd)
           (lambda (obj)
             (record-with-type? obj rtd)))

         (define (record-accessor rtd field-name)
           (assert-rtd rtd)
           (let ((i (rtd-field-offset rtd field-name)))
             (lambda (obj)
               (assert-record-of-type obj rtd)
               (vector-like-ref obj i))))

         (define (record-updater rtd field-name)
           (assert-rtd rtd)
           (let ((i (rtd-field-offset rtd field-name)))
             (lambda (obj val)
               (assert-record-of-type obj rtd)
               (vector-like-set! obj i val))))

         (define (record-type-descriptor rec)
           (assert-record rec)
           (vector-like-ref rec 0))

         (define (record-indexer rtd)
           (assert-rtd rtd)
           (let ((num-fields (length (record-type-field-names rtd))))
             (lambda (obj index)
               (cond ((zero? num-fields)
                      (error "No slots in " (record-type-name rtd)))
                     ((<= 0 index (- num-fields 1))
                      (vector-like-ref obj (+ index
                                              record-overhead)))
                     (else (error "slot index out of bounds"))))))

         (define (record-mutator rtd)
           (assert-rtd rtd)
           (let ((num-fields (length (record-type-field-names rtd))))
             (lambda (obj index new-val)
               (assert-record-of-type obj rtd)
               (vector-like-set! obj
                                 (+ index record-overhead)
                                 new-val))))

         ; Record types
  
         ; (define *rtd-type* 
         ;   (make-record-type "record-type-descriptor-type"
         ;     '(name                symbol
         ;       slot-offsets        assoc list of (field-name . offset)
         ;       printer             procedure takes obj, output-port
         ;       record-size         fixnum
         ;       hierarchy-vector    vector of rtds
         ;       hierarchy-depth     fixnum
         ;       )))
         
         ; Magic definiton of *rtd-type*, predicate, and accessors because
         ; of circularity problems.

         (define *rtd-type*
           (let* ((slot-offsets
                   `((name . ,(+ record-overhead 0))
                     (slot-offsets . ,(+ record-overhead 1))
                     (printer . ,(+ record-overhead 2))
                     (record-size . ,(+ record-overhead 3))
                     (hierarchy-vector . ,(+ record-overhead 4))
                     (hierarchy-depth . ,(+ record-overhead 5))))
                  (x 
                   (make-structure (+ record-overhead (length slot-offsets))))
                  (name 'record-type))
             (vector-like-set! x 0 x)
             (vector-like-set! x 1 name)
             (vector-like-set! x 2 slot-offsets)
             (vector-like-set! x 3 #f)
             (vector-like-set! x 4 (vector-like-length x))
             (vector-like-set! x 5 (vector x))
             (vector-like-set! x 6 0)
             x))

         ; Hairy because record type descriptors are themselves record
         ; types that can be extended.  The logic here is: if the object
         ; looks like a record type descriptor, and its slot 0 looks
         ; like a record type descriptor, and slot 0 is a subtype of
         ; *rtd-type*, then the object is a record type descriptor.

         (define (record-type-descriptor? obj)
           (and (structure? obj)
                (> (vector-like-length obj) 0)
                (let ((slot0 (vector-like-ref obj 0)))
                  (and (structure? obj)
                       (and (>= (vector-like-length slot0) 
                                (vector-like-length *rtd-type*))
                            (vector? (rtd-hierarchy-vector slot0))
                            (rtd-extends? slot0 *rtd-type*))))))

         (define (rtd-name rtd)
           (vector-like-ref rtd (+ record-overhead 0)))

         (define (rtd-slot-offsets rtd)
           (vector-like-ref rtd (+ record-overhead 1)))

         (define (rtd-record-size rtd)
           (vector-like-ref rtd (+ record-overhead 3)))

         (define (rtd-hierarchy-vector rtd)
           (vector-like-ref rtd (+ record-overhead 4)))

         (define (rtd-hierarchy-depth rtd)
           (vector-like-ref rtd (+ record-overhead 5)))

         (define (rtd-field-offset rtd name)
           (cdr (assq name (rtd-slot-offsets rtd))))
  
         (define (rtd-field-names rtd)
           (map car (rtd-slot-offsets rtd)))

         ; End magic

         ; r1 extends r2 
         ;    iff 
         ; r1.type-hierarchy-vector[ r2.type-hierarchy.depth ] = r2

         (define (rtd-extends? r1 r2)
           (let ((r1-vector (rtd-hierarchy-vector r1))
                 (r2-depth  (rtd-hierarchy-depth r2)))
             (and (< r2-depth (vector-length r1-vector))
                  (eq? (vector-ref r1-vector r2-depth) r2))))

         (define (make-record-type type-name field-names parent)
           (or (not parent) (assert-rtd parent))
           (let* ((field-names
                   (if parent
                       (append (rtd-field-names parent)
                               field-names)
                       field-names))
                  (hierarchy-depth
                   (if parent
                       (+ 1 (rtd-hierarchy-depth parent))
                       0))
                  (hierarchy-vector
                   (let ((v (make-vector (+ hierarchy-depth 1))))
                     (if parent
                         (let ((pv (rtd-hierarchy-vector parent)))
                           (do ((i 0 (+ i 1)))
                               ((= i hierarchy-depth))
                             (vector-set! v i (vector-ref pv i)))))
                     v)))
             (let ((rtd ((record-constructor *rtd-type* #f)
                         type-name
                         (compute-slot-offsets field-names)
                         #f
                         (+ (length field-names) record-overhead)
                         hierarchy-vector
                         hierarchy-depth)))
               (vector-set! hierarchy-vector hierarchy-depth rtd)
               rtd)))

         (define (record-type-field-names rtd)
           (assert-rtd rtd)
           (rtd-field-names rtd))

         (define (record-type-extends? rtd1 rtd2)
           (assert-rtd rtd1)
           (assert-rtd rtd2)
           (rtd-extends? rtd1 rtd2))

         (define (record-type-name rtd)
           (assert-rtd rtd)
           (rtd-name rtd))

         (define (record-type-parent rtd)
           (assert-rtd rtd)
           (if (> (rtd-hierarchy-depth rtd) 0)
               (vector-ref (rtd-hierarchy-vector rtd)
                           (- (rtd-hierarchy-depth rtd) 1))
               #f))

         ; Helper functions.

         (define (compute-slot-offsets fields)
           (do ((j record-overhead (+ j 1))
                (r '() (cons (cons (car f) j) r))
                (f fields (cdr f)))
               ((null? f) (reverse r))))

         (define (assert-rtd obj)
           (if (not (record-type-descriptor? obj))
               (error "Not a record type descriptor: " obj)))

         (define (assert-record obj)
           (if (not (record? obj))
               (error "Not a record: " obj)))

         (define (assert-record-of-type obj rtd)
           (if (not (record-with-type? obj rtd))
               (error "Object is not record of type: " (rtd-name rtd) 
                      ": " obj)))

         (list 
          (lambda (name field-names . rest) 
            (make-record-type name field-names (if (null? rest) 
                                                   #f 
                                                   (car rest))))
          (lambda (x) (record-type-descriptor? x))
          (lambda (rtd) (record-type-field-names rtd))
          (lambda (rtd) (record-type-name rtd))
          (lambda (rtd1 rtd2) (record-type-extends? rtd1 rtd2))
          (lambda (rtd) (record-type-parent rtd))
          (lambda (x) (record? x))
          (lambda (rtd . rest) 
            (record-constructor rtd (if (null? rest)
                                        #f
                                        (car rest))))
          (lambda (rtd) (record-predicate rtd))
          (lambda (rtd field-name) (record-accessor rtd field-name))
          (lambda (rtd field-name) (record-updater rtd field-name))
          (lambda (x) (record-type-descriptor x))
          (lambda (rtd) (record-indexer rtd))
          (lambda (rtd) (record-mutator rtd))
          *rtd-type*))))

  (set! make-record-type (list-ref interface 0))
  (set! record-type-descriptor? (list-ref interface 1))
  (set! record-type-field-names (list-ref interface 2))
  (set! record-type-name (list-ref interface 3))
  (set! record-type-extends? (list-ref interface 4))
  (set! record-type-parent (list-ref interface 5))
  (set! record? (list-ref interface 6))
  (set! record-constructor (list-ref interface 7))
  (set! record-predicate (list-ref interface 8))
  (set! record-accessor (list-ref interface 9))
  (set! record-updater (list-ref interface 10))
  (set! record-type-descriptor (list-ref interface 11))
  (set! record-indexer (list-ref interface 12))
  (set! record-mutator (list-ref interface 13))
  (set! *record-type-type* (list-ref interface 14))

  ;; Install printers.

  ;; Record-type descriptor printer.
  ((record-updater *record-type-type* 'printer)
   *record-type-type*
   (lambda (obj port)
     (display "#<record-type " port)
     (display (record-type-name obj) port)
     (display ">" port)))

  ; Record printer: if the record's rtd has a printer, call it.  Otherwise
  ; print #<record name>.

  (let ((previous-printer (structure-printer))
        (get-printer (record-accessor *record-type-type* 'printer)))
    (structure-printer
     (lambda (obj port quote?)
       (cond ((record? obj)
              (let ((p (get-printer (record-type-descriptor obj))))
                (if p
                    (p obj port)
                    (begin (display "#<record " port)
                           (display (record-type-name
                                     (record-type-descriptor obj))
                                    port)
                           (display ">" port)))))
             (else
              (previous-printer obj port quote?))))))

  'records)

; eof
