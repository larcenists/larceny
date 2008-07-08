; Copyright 1998 Lars T Hansen.
;
; $Id: record.sch 5090 2007-11-06 02:49:12Z will $
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
;
; That implementation has been modified and extended to conform
; to the procedural record layer (r6rs records procedural) that
; is described in the 5.92 draft R6RS.
;
; Finally, the ERR5RS API is layered on top of the R6RS API.
;
; There are now two kinds of records, old-style and R6RS.
; The R6RS procedures have been extended to accept old-style
; records; this does not affect R6RS conformance because it
; will be impossible to create old-style records in
; R6RS-conforming mode.  Note that record-type-field-names
; returns a list when given an old-style record, but returns
; a vector when given an R6RS record.
;
; FIXME:  The implementation is now incredibly crufty because
; Will added the R6RS extensions before he really understood
; the R6RS spec (which was in even worse shape at that time
; than it is now).  The whole thing should be rewritten to
; implement ERR5RS records natively, and Larceny's traditional
; records and R6RS records should both be layered on top of
; the ERR5RS API.
;
; FIXME:  This file belongs in src/Lib/Common.
;
; FIXME:
;     make-record-constructor-descriptor should check its arguments
;         and should create sealed and immutable records
;     record-constructor may not be quite right
;         (because the R5.97RS is still quite confusing)
;     the rtd-rtd should probably be sealed and opaque
;
; Larceny now supports (I believe) the following invariant:
;     all structures are records
;     slot 0 contains an extended inheritance hierarchy, a vector
;     that vector always contains at least record:hierarchy:min elements
;     element 0 of that vector holds the rtd
;     element 1 holds the base rtd (which has no parent)
;     element 2 holds an immediate child of the base rtd
;     et cetera
;
; FIXME: This invariant is not enforced by make-structure,
; so programmers who use make-structure can break it.
;
; Advantages of that invariant:
;     once you know r is a structure,
;     you know slot 0 is a vector with at least record:hierarchy:min elements
;     so you can perform the type check by fetching and comparing
;     provided the inheritance depth of the expected record type
;     is no greater than record:hierarchy:min - 1

($$trace "record")

(define record:hierarchy:min 7)          ; record hierarchy vec's min length
(define record:bummed? #t)               ; #f means slow, #t means fast

(define make-record-type)
(define record-type-descriptor?)
(define record-type-field-names)         ; overloaded for R6RS
(define record-type-name)                ; overloaded for R6RS
(define record-type-extends?)
(define record-type-parent)

(define record?)
(define record-constructor)              ; overloaded for R6RS
(define record-predicate)
(define record-accessor)                 ; overloaded for R6RS
(define record-updater)
(define record-type-descriptor)

;; Added these to support MzScheme structure interface
(define record-indexer)
(define record-mutator)

; New for R6RS.

(define make-record-type-descriptor)
(define record-type-uid)
(define record-type-generative?)
(define record-type-sealed?)
(define record-type-opaque?)
(define record-field-mutable?)

(define (record-rtd rec)
  (if (record? rec)
      (record-type-descriptor rec)
      (assertion-violation 'record-rtd "illegal argument" rec)))

(define make-record-constructor-descriptor)

; The performance of records is mostly determined by these procedures.
; FIXME: all of them could be made faster

; Given the hierarchy vector that goes in element 0,
; the number of elements of the record to be created,
; the number of arguments to be passed to the creator,
; and a list of indices within the record structure for those arguments,
; returns a procedure that accepts the arguments and creates the record.

(define (make-bummed-record-constructor rtd hierarchy-vector size n indices)
  (cond ((and (= size 1) (= n 0) (equal? indices '()))
         (lambda ()
           (let ((r (make-structure 1)))
             (vector-like-set! r 0 hierarchy-vector)
             r)))
        ((and (= size (+ n 1)) (= n 1) (equal? indices '(1)))
         (lambda (a)
           (let ((r (make-structure 2)))
             (vector-like-set! r 0 hierarchy-vector)
             (vector-like-set! r 1 a)
             r)))
        ((and (= size (+ n 1)) (= n 2) (equal? indices '(1 2)))
         (lambda (a b)
           (let ((r (make-structure 3)))
             (vector-like-set! r 0 hierarchy-vector)
             (vector-like-set! r 1 a)
             (vector-like-set! r 2 b)
             r)))
        ((and (= size (+ n 1)) (= n 3) (equal? indices '(1 2 3)))
         (lambda (a b c)
           (let ((r (make-structure 4)))
             (vector-like-set! r 0 hierarchy-vector)
             (vector-like-set! r 1 a)
             (vector-like-set! r 2 b)
             (vector-like-set! r 3 c)
             r)))
        ((and (= size (+ n 1)) (= n 4) (equal? indices '(1 2 3 4)))
         (lambda (a b c d)
           (let ((r (make-structure 5)))
             (vector-like-set! r 0 hierarchy-vector)
             (vector-like-set! r 1 a)
             (vector-like-set! r 2 b)
             (vector-like-set! r 3 c)
             (vector-like-set! r 4 d)
             r)))
        ((and (= size (+ n 1)) (= n 5) (equal? indices '(1 2 3 4 5)))
         (lambda (a b c d e)
           (let ((r (make-structure 6)))
             (vector-like-set! r 0 hierarchy-vector)
             (vector-like-set! r 1 a)
             (vector-like-set! r 2 b)
             (vector-like-set! r 3 c)
             (vector-like-set! r 4 d)
             (vector-like-set! r 5 e)
             r)))
        ((and (= size (+ n 1)) (= n 6) (equal? indices '(1 2 3 4 5 6)))
         (lambda (a b c d e f)
           (let ((r (make-structure 7)))
             (vector-like-set! r 0 hierarchy-vector)
             (vector-like-set! r 1 a)
             (vector-like-set! r 2 b)
             (vector-like-set! r 3 c)
             (vector-like-set! r 4 d)
             (vector-like-set! r 5 e)
             (vector-like-set! r 6 f)
             r)))
        ((= n (length indices))

         ; FIXME: poor error message for wrong number of args

         (lambda vals0
           (let ((r (make-structure size)))
             (vector-like-set! r 0 hierarchy-vector)
             (do ((indices indices (cdr indices))
                  (vals    vals0   (cdr vals)))
                 ((or (null? indices) (null? vals))
                  (if (not (and (null? indices) (null? vals)))
                      (error #f
                             "wrong number of arguments to record constructor"
                             rtd vals0))
                  r)
               (vector-like-set! r (car indices) (car vals))))))
        (else
         (assertion-violation 'record-constructor
                              "internal error" rtd n indices))))

(define (make-bummed-record-predicate rtd depth)
  (lambda (obj)
    (and (structure? obj)
         (eq? (vector-ref (vector-like-ref obj 0) depth)
              rtd))))

(define (make-bummed-record-accessor rtd depth i)
  (lambda (obj)
    (if (and (structure? obj)
             (eq? (vector-ref (vector-like-ref obj 0) depth)
                  rtd))
        (vector-like-ref obj i)
        (assertion-violation 'anonymous-record-accessor
                             "illegal argument" rtd obj))))

(define (make-bummed-record-mutator rtd depth i)
  (lambda (obj x)
    (if (and (structure? obj)
             (eq? (vector-ref (vector-like-ref obj 0) depth)
                  rtd))
        (vector-like-set! obj i x)
        (assertion-violation 'anonymous-record-mutator
                             "illegal argument" rtd obj))))

;

(let ((interface

       ; Records of length n are represented as structures where element 0 
       ; contains the record type descriptor, and the other elements hold
       ; the field values.
       ;
       ; A record type descriptor is a record.  Every record type descriptor 
       ; is of type *rtd-type*, which is internal to this module.

       (let ((record-overhead 1))       ; Number of overhead slots.

         ; Records
         
         ; RECORD-WITH-TYPE? assumes rtd is in fact an rtd.
         ; FIXME: reliance on the invariant would make this much faster,
         ; but we should check the invariant until this is debugged.

         (define (record-with-type? obj rtd)
           (and (structure? obj)
                (> (vector-like-length obj) 0)
                (let ((slot0 (vector-like-ref obj 0)))
                  (and (vector? slot0)
                       (>= (vector-length slot0) record:hierarchy:min)
                       (let ((obj-rtd (vector-ref slot0 0)))
                         (or (eq? rtd obj-rtd)
                             (rtd-extends? obj-rtd rtd)))))))

         ; FIXME: reliance on the invariant would make this much faster,
         ; but we should check the invariant until this is debugged.

         (define (record? obj)
           (and (structure? obj)
                (> (vector-like-length obj) 0)
                (let ((slot0 (vector-like-ref obj 0)))
                  (and (vector? slot0)
                       (>= (vector-length slot0) record:hierarchy:min)
                       (record-type-descriptor? (vector-ref slot0 0))))))

         (define (record-constructor rtd fields)
           (assert-rtd rtd)
           (let* ((indices (map (lambda (name)
                                  (rtd-field-offset rtd name))
                                (if (not fields)
                                    (rtd-field-names rtd)
                                    fields)))
                  (n       (length indices))
                  (size    (rtd-record-size rtd))
                  (hvec    (rtd-hierarchy-vector rtd)))
             (make-bummed-record-constructor
              rtd hvec size n indices)))

         ; Returns a thunk that creates an instance of rtd
         ; with all of its fields initialized to unspecified values.

         (define (record-constructor-raw rtd)
           (assert-rtd rtd)
           (let ((size (rtd-record-size rtd)))
             (lambda ()
               (let ((r (make-structure size)))
                 (vector-like-set! r 0 (rtd-hierarchy-vector rtd))
                 r))))

         (define (record-predicate rtd)
           (assert-rtd rtd)
           (let* ((rtd-depth (rtd-hierarchy-depth rtd))
                  (depth (+ rtd-depth 1)))
             (if (and record:bummed?
                      (< depth record:hierarchy:min))
                 (make-bummed-record-predicate rtd depth)
                 (lambda (obj)
                   (record-with-type? obj rtd)))))

         (define (record-accessor rtd field-name)
           (assert-rtd rtd)
           (let* ((rtd-depth (rtd-hierarchy-depth rtd))
                  (depth (+ rtd-depth 1))
                  (i (rtd-field-offset rtd field-name)))
             (if (and record:bummed?
                      (< depth record:hierarchy:min))
                 (make-bummed-record-accessor rtd depth i)
                 (lambda (obj)
                   (assert-record-of-type obj rtd)
                   (vector-like-ref obj i)))))

         (define (record-updater rtd field-name)
           (assert-rtd rtd)
           (let* ((rtd-depth (rtd-hierarchy-depth rtd))
                  (depth (+ rtd-depth 1))
                  (i (rtd-field-offset rtd field-name)))
             (if (and record:bummed?
                      (< depth record:hierarchy:min))
                 (make-bummed-record-mutator rtd depth i)
                 (lambda (obj val)
                   (assert-record-of-type obj rtd)
                   (vector-like-set! obj i val)))))

         (define (record-type-descriptor rec)
           (assert-record rec)
           (vector-ref (vector-like-ref rec 0) 0))

         (define (record-indexer rtd)
           (assert-rtd rtd)
           (let ((num-fields (length (record-type-field-names rtd))))
             (lambda (obj index)
               (cond ((zero? num-fields)
                      (error "No slots in " (record-type-name rtd)))
                     ((<= 0 index (- num-fields 1))
                      (vector-like-ref obj (+ index
                                              record-overhead)))
                     (else (error "slot index must be in [0, "
                                  (- num-fields 1) "]") )))))

         (define (old-style-record-mutator rtd)
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
         ;     '(name                string
         ;       slot-offsets        assoc list of (field-name . offset)
         ;       printer             procedure takes obj, output-port
         ;       record-size         fixnum
         ;       hierarchy-vector    vector of rtds
         ;       hierarchy-depth     fixnum
         ;
         ;       new for R6RS
         ;
         ;       r6rs?               boolean
         ;       uid                 symbol or #f
         ;       sealed?             boolean
         ;       opaque?             boolean
         ;       field-names         vector of symbols (excluding parent's)
         ;       mutabilities        vector of booleans (for field-names)
         ;       favored-cd          record constructor descriptor
         ;       )))
         
         ; Magic definition of *rtd-type*, predicate, and accessors because
         ; of circularity problems.

         (define *rtd-type*
           (let* ((slot-offsets
                   `((name . ,(+ record-overhead 0))
                     (slot-offsets . ,(+ record-overhead 1))
                     (printer . ,(+ record-overhead 2))
                     (record-size . ,(+ record-overhead 3))
                     (hierarchy-vector . ,(+ record-overhead 4))
                     (hierarchy-depth . ,(+ record-overhead 5))
                     ;
                     (r6rs? . ,(+ record-overhead 6))
                     (uid . ,(+ record-overhead 7))
                     (sealed? . ,(+ record-overhead 8))
                     (opaque? . ,(+ record-overhead 9))
                     (field-names . ,(+ record-overhead 10))
                     (mutabilities . ,(+ record-overhead 11))
                     (favored-cd . ,(+ record-overhead 12))))

                  (x 
                   (make-structure (+ record-overhead (length slot-offsets))))
                  (x-hierarchy
                   (let ((v (make-vector record:hierarchy:min #f)))
                     (vector-set! v 0 x)
                     (vector-set! v 1 x)
                     v))
                  (name
                   "record-type-descriptor-type"))

             (vector-like-set! x 0 x-hierarchy)
             (vector-like-set! x 1 name)
             (vector-like-set! x 2 slot-offsets)
             (vector-like-set! x 3 #f)
             (vector-like-set! x 4 (vector-like-length x))
             (vector-like-set! x 5 x-hierarchy)
             (vector-like-set! x 6 0)
             ;
             (vector-like-set! x 7 #f)
             (vector-like-set! x 8 #f)
             (vector-like-set! x 9 #f)
             (vector-like-set! x 10 #f)
             (vector-like-set! x 11 #f)
             (vector-like-set! x 12 #f)
             (vector-like-set! x 13 #f)
             x))

         ; Hairy because record type descriptors are themselves record
         ; types that can be extended.  The logic here is: if the object
         ; looks like a record type descriptor, and its slot 0 looks
         ; like the hierarchy vector of a record type descriptor,
         ; and element 0 of that hierarchy vector is a subtype of
         ; *rtd-type*, then the object is a record type descriptor.

         ; FIXME: reliance on the invariant would make this much faster,
         ; but we should check the invariant until this is debugged.

         (define (record-type-descriptor? obj)
           (and (structure? obj)
                (> (vector-like-length obj) 0)
                (let ((slot0 (vector-like-ref obj 0)))
                  (and (vector? slot0)
                       (>= (vector-length slot0) record:hierarchy:min)
                       (let ((rtd (vector-ref slot0 0)))
                         (and (structure? rtd)
                              (>= (vector-like-length rtd) 
                                  (vector-like-length *rtd-type*))
                              (vector? (rtd-hierarchy-vector rtd))
                              (rtd-extends? rtd *rtd-type*)))))))

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

         ; Magic for R6RS extensions

         (define (rtd-r6rs? rtd)
           (vector-like-ref rtd (+ record-overhead 6)))

         (define (rtd-uid rtd)
           (vector-like-ref rtd (+ record-overhead 7)))

         (define (rtd-sealed? rtd)
           (vector-like-ref rtd (+ record-overhead 8)))

         (define (rtd-opaque? rtd)
           (vector-like-ref rtd (+ record-overhead 9)))

         (define (rtd-field-names-r6rs rtd)
           (vector-like-ref rtd (+ record-overhead 10)))

         (define (rtd-mutabilities rtd)
           (vector-like-ref rtd (+ record-overhead 11)))

         (define (rtd-favored-cd rtd)
           (vector-like-ref rtd (+ record-overhead 12)))

         ; End magic

         ; r1 extends r2 
         ;    iff 
         ; r1.type-hierarchy-vector[ r2.type-hierarchy.depth + 1 ] = r2

         (define (rtd-extends? r1 r2)
           (let ((r1-vector (rtd-hierarchy-vector r1))
                 (r2-depth  (rtd-hierarchy-depth r2)))
             (and (< r2-depth (- (vector-length r1-vector) 1))
                  (eq? (vector-ref r1-vector (+ r2-depth 1)) r2))))

         (define (make-record-type type-name field-names parent)
           (if parent (and (assert-rtd parent)
                           (assert (not (rtd-sealed? parent)))))
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
                   (let ((v (make-vector (max record:hierarchy:min
                                              (+ hierarchy-depth 2))
                                         #f)))
                     (if parent
                         (let ((pv (rtd-hierarchy-vector parent)))
                           (do ((i 1 (+ i 1)))
                               ((> i hierarchy-depth))
                             (vector-set! v i (vector-ref pv i)))))
                     v)))
             (let ((rtd ((record-constructor *rtd-type* #f)
                         type-name
                         (compute-slot-offsets field-names)
                         #f
                         (+ (length field-names) record-overhead)
                         hierarchy-vector
                         hierarchy-depth
                         #f #f #f #f #f #f #f)))
               (vector-set! hierarchy-vector 0 rtd)
               (vector-set! hierarchy-vector (+ hierarchy-depth 1) rtd)
               rtd)))

         (define (record-type-field-names rtd)
           (assert-rtd rtd)
           (if (rtd-r6rs? rtd)
               (list->vector (rtd-field-names-r6rs rtd))
               (rtd-field-names rtd)))

         (define (record-type-extends? rtd1 rtd2)
           (assert-rtd rtd1)
           (assert-rtd rtd2)
           (rtd-extends? rtd1 rtd2))

         (define (record-type-name rtd)
           (assert-rtd rtd)
           (if (rtd-r6rs? rtd)
               (string->symbol (rtd-name rtd))
               (rtd-name rtd)))

         (define (record-type-parent rtd)
           (assert-rtd rtd)
           (if (> (rtd-hierarchy-depth rtd) 0)
               (vector-ref (rtd-hierarchy-vector rtd)
                           (rtd-hierarchy-depth rtd))
               #f))

         ; R6RS extensions

         (define (record-type-uid rtd)
           (assert-rtd rtd)
           (rtd-uid rtd))

         (define (record-type-generative? rtd)
           (assert-rtd rtd)
           (not (rtd-uid rtd)))

         (define (record-type-sealed? rtd)
           (assert-rtd rtd)
           (rtd-sealed? rtd))

         (define (record-type-opaque? rtd)
           (assert-rtd rtd)
           (rtd-opaque? rtd))

         (define (record-field-mutable? rtd k)
           (assert-rtd rtd)
           (if (rtd-r6rs? rtd)
               (let ((mutabilities (rtd-mutabilities rtd)))
                 (assert-index k)
                 (if (< k (length mutabilities))
                     (list-ref mutabilities k)
                     (error "Field index out of range: " rtd k)))
               (let ((field-names (rtd-field-names rtd)))
                 (if (and (symbol? k) (memq k field-names))
                     #t
                     (error "Illegal record field name: " k)))))

         (define (make-record-type-descriptor
                  name parent uid sealed? opaque? fields0)
           (if (and (symbol? name)
                    (or (eq? parent #f)
                        (and (record-type-descriptor? parent)
                             (rtd-r6rs? parent)
                             (if (rtd-sealed? parent)
                                 (error "Parent type is sealed: " parent)
                                 #t)))
                    (or (symbol? uid) (eq? uid #f))
                    (boolean? sealed?)
                    (boolean? opaque?)
                    (vector? fields0))
               (do ((fields (reverse (vector->list fields0)) (cdr fields))
                    (names '() (cons (cadar fields) names))
                    (mutabilities '()
                                  (cons (eq? (caar fields) 'mutable)
                                        mutabilities)))
                   ((null? fields)
         
                    (let* ((field-names
                            (if parent
                                (append (rtd-field-names parent)
                                        names)
                                names))
                           (hierarchy-depth
                            (if parent
                                (+ 1 (rtd-hierarchy-depth parent))
                                0))
                           (hierarchy-vector
                            (let ((v (make-vector (max record:hierarchy:min
                                                       (+ hierarchy-depth 2))
                                                  #f)))
                              (if parent
                                  (let ((pv (rtd-hierarchy-vector parent)))
                                    (do ((i 1 (+ i 1)))
                                        ((> i hierarchy-depth))
                                      (vector-set! v i (vector-ref pv i)))))
                              v))
                           (rtd ((record-constructor *rtd-type* #f)
                                 (symbol->string name) ; FIXME
                                 (compute-slot-offsets field-names)
                                 #f
                                 (+ (length field-names) record-overhead)
                                 hierarchy-vector
                                 hierarchy-depth
                                 #t uid sealed?
                                 (or opaque?
                                     (and parent (rtd-opaque? parent)))
                                 names mutabilities #f)))

                      (vector-set! hierarchy-vector 0 rtd)
                      (vector-set! hierarchy-vector (+ hierarchy-depth 1) rtd)
                      (if uid
                          (lookup-nongenerative-rtd
                           rtd name parent uid sealed? opaque? fields0)
                          rtd)))

                 (if (let ((field (car fields)))
                       (or (not (list? field))
                           (not (= 2 (length field)))
                           (not (or (eq? (car field) 'immutable)
                                    (eq? (car field) 'mutable)))
                           (not (symbol? (cadr field)))))
                     (error "Bad field passed to make-record-type-descriptor: "
                            (car fields))))
               (error "Bad arguments to make-record-type-descriptor: "
                            (list name parent
                                  uid sealed? opaque? fields0))))

         (define (r6rs-record-constructor cd)
           (define wna
             "Wrong number of values supplied when constructing record: ")
           (if (r6rs-record-constructor-descriptor? cd)
               (let ((rtd (r6rs-record-constructor-descriptor-rtd cd))
                     (parent-cd
                      (r6rs-record-constructor-descriptor-parent-cd cd))
                     (protocol
                      (r6rs-record-constructor-descriptor-protocol cd)))
                 (if (and (record-type-descriptor? rtd)
                          (rtd-r6rs? rtd)
                          (or (eq? #f parent-cd)
                              (and (r6rs-record-constructor-descriptor?
                                    parent-cd)
                                   (< 0 (rtd-hierarchy-depth rtd))))
                          (or (eq? #f protocol)
                              (procedure? protocol)))
                     (r6rs-record-constructor-general-case rtd cd)
                     (error
                      'record-constructor
                      "illegal arguments"
                      rtd parent-cd protocol)))
               (error 'record-constructor
                      "illegal arguments to constructor-descriptor" cd)))

         ; Returns a procedure that constructs an instance of rtd
         ; and initializes an initial segment of the fields
         ; according to cd.

         (define (r6rs-record-constructor-general-case rtd cd)
           (define wna
             "Wrong number of values supplied when constructing record: ")
           (let* ((cd-rtd (r6rs-record-constructor-descriptor-rtd cd))
                  (parent-cd
                   (r6rs-record-constructor-descriptor-parent-cd cd))
                  (protocol
                   (or (r6rs-record-constructor-descriptor-protocol cd)
                       (default-protocol rtd parent-cd cd-rtd)))) ; FIXME
             (if (and (record-type-descriptor? cd-rtd)
                      (rtd-r6rs? cd-rtd)
                      (or (eq? #f parent-cd)
                          (and (r6rs-record-constructor-descriptor?
                                parent-cd)
                               (< 0 (rtd-hierarchy-depth cd-rtd))))
                      (or (eq? #f protocol)
                          (procedure? protocol)))
                 (cond ((and (eq? #f parent-cd) (eq? #f protocol))
                        ; FIXME: can't happen because of default protocol
                        (if (eq? rtd cd-rtd)
                            (record-constructor rtd #f)
                            (make-r6rs-constructor rtd cd-rtd)))
                       ((= 0 (rtd-hierarchy-depth cd-rtd))
                        ; parent-cd is #f
                        (protocol
                         (if (eq? rtd cd-rtd)
                             (record-constructor rtd #f)
                             (make-r6rs-constructor rtd cd-rtd))))
                       ((eq? #f parent-cd)
                        (let* ((parent (record-type-parent cd-rtd))
                               (maker (make-r6rs-constructor rtd parent))
                               (f (make-r6rs-initializer maker cd-rtd parent)))
                          (protocol f)))
                       (else
                        (let* ((parent (record-type-parent cd-rtd))
                               (maker (r6rs-record-constructor-general-case
                                       rtd parent-cd))
                               (f (make-r6rs-initializer maker cd-rtd parent)))
                          (protocol f))))
                 (error "Illegal argument to constructor-descriptor: " cd))))

         ; Returns a default protocol for cd-rtd.
         ; FIXME: the 5.97 draft specification contradicts itself
         ; regarding whether a certain procedure takes one argument
         ; for every field *including* parents or *excluding* parents.
         ; What follows is my best guess.
         ; FIXME: I have no idea.

         (define (default-protocol rtd parent-cd cd-rtd)
           (cond ((= 0 (rtd-hierarchy-depth cd-rtd))
                  (lambda (x) x))
                 ((and #f (not parent-cd))
                  ; FIXME: I have no idea.
                  (lambda (x) x))
                 (else
                  (let* ((parent (record-type-parent cd-rtd))
                         (parent-fields (rtd-field-names parent))
                         (cd-rtd-fields (rtd-field-names cd-rtd))
                         (nparent (length parent-fields))
                         (nthis (- (length cd-rtd-fields) nparent)))
                    (lambda (n)
                      (lambda args0
                        (define (loop i args parent-args-reversed)
                          (cond ((= i nparent)
                                 (let* ((pargs (reverse parent-args-reversed))
                                        (p (apply n pargs)))
                                   (apply p args)))
                                ((pair? args)
                                 (loop (+ i 1)
                                       (cdr args)
                                       (cons (car args) parent-args-reversed)))
                                (else
                                 (assertion-violation
                                  #f "too few arguments to record constructor"
                                  args0))))
                        (loop 0 args0 '())))))))

         ; Returns a procedure that
         ; takes one argument for each field of base-rtd,
         ; creates an instance of derived-rtd,
         ; initializes the instance's base-rtd fields to the arguments,
         ; and returns the instance.

         (define (make-r6rs-constructor derived-rtd base-rtd)
           (let* ((base-fields (rtd-field-names base-rtd))
                  (nargs (length base-fields))
                  (nlimit (+ record-overhead nargs))
                  (make-uninitialized-record
                   (record-constructor-raw derived-rtd)))
             (case nargs
              (else
               (lambda base-inits
                 (let ((r (make-uninitialized-record)))
                   (do ((inits base-inits (cdr inits))
                        (offset record-overhead (+ offset 1)))
                       ((= offset nlimit)
                        (if (null? inits)
                            r
                            (error:constructor:wna derived-rtd base-inits)))
                     (if (null? inits)
                         (error:constructor:wna derived-rtd base-inits)
                         (vector-like-set! r offset (car inits))))))))))

         ; Given a partial constructor f for parent-rtd,
         ; returns a partial constructor g for derived-rtd.
         ;
         ; The procedure g accepts any number of arguments
         ; and returns a procedure h that
         ; takes one argument for each field of derived-rtd
         ; that is not a field of parent-rtd,
         ; calls f on the arguments that were passed to g
         ; to create a partially initialized record r,
         ; initializes the appropriate fields of r
         ; to the arguments that were passed to h,
         ; and returns the instance r.

         (define (make-r6rs-initializer f derived-rtd parent-rtd)
           (let* ((parent-fields (rtd-field-names parent-rtd))
                  (derived-fields (rtd-field-names derived-rtd))
                  (nfields-parent (length parent-fields))
                  (nfields-derived (length derived-fields))
                  (nargs (- nfields-derived nfields-parent))
                  (nstart (+ record-overhead nfields-parent))
                  (nlimit (+ nstart nargs)))
             (case nargs
              (else
               (lambda args
                 (lambda derived-inits
                   (let ((r (apply f args)))
                     (do ((inits derived-inits (cdr inits))
                          (offset nstart (+ offset 1)))
                         ((= offset nlimit)
                          (if (null? inits)
                              r
                              (error:constructor:wna derived-rtd
                                                     derived-inits)))
                       (if (null? inits)
                           (error:constructor:wna derived-rtd derived-inits)
                           (vector-like-set! r offset (car inits)))))))))))

         (define (r6rs-record-accessor rtd k)
           (assert-rtd rtd)
           (assert-index k)
           (let* ((all-field-names (rtd-field-names rtd))
                  (field-names (rtd-field-names-r6rs rtd))
                  (n (rtd-record-size rtd))
                  (i (+ k (- n (length field-names))))
                  (rtd-depth (rtd-hierarchy-depth rtd))
                  (depth (+ rtd-depth 1)))
             (if (< i n)
                 (if (and record:bummed?
                          (< depth record:hierarchy:min))
                     (make-bummed-record-accessor rtd depth i)
                     (lambda (obj)
                       (assert-record-of-type obj rtd)
                       (vector-like-ref obj i)))
                 (error 'record-accessor
                        "record index out of range" rtd k))))

         (define (r6rs-record-mutator rtd k)
           (assert-rtd rtd)
           (assert-index k)
           (let* ((all-field-names (rtd-field-names rtd))
                  (field-names (rtd-field-names-r6rs rtd))
                  (n (rtd-record-size rtd))
                  (i (+ k (- n (length field-names))))
                  (rtd-depth (rtd-hierarchy-depth rtd))
                  (depth (+ rtd-depth 1)))
             (if (< i n)
                 (cond ((not (record-field-mutable? rtd k))
                        (assertion-violation
                         'r6rs-record-mutator
                         "record field is immutable" rtd k))
                       ((and record:bummed?
                             (< depth record:hierarchy:min))
                        (make-bummed-record-mutator rtd depth i))
                       (else
                        (lambda (obj val)
                          (assert-record-of-type obj rtd)
                          (vector-like-set! obj i val))))
                 (assertion-violaton
                 'r6rs-record-mutator
                 "record index out of range: " rtd k))))

         ; Helper functions.

         (define (compute-slot-offsets fields)
           (do ((j record-overhead (+ j 1))
                (r '() (cons (cons (car f) j) r))
                (f fields (cdr f)))
               ((null? f) (reverse r))))

         (define (assert-rtd obj)
           (if (not (record-type-descriptor? obj))
               (error "Not a record type descriptor: " obj)))

         (define (assert-r6rs-rtd obj)
           (if (or (not (record-type-descriptor? obj))
                   (not (rtd-r6rs? obj)))
               (error "Not an R6RS record type descriptor: " obj)))

         (define (assert-record obj)
           (if (not (record? obj))
               (error "Not a record: " obj)))

         (define (assert-record-of-type obj rtd)
           (if (not (record-with-type? obj rtd))
               (error "Object is not record of type: " (rtd-name rtd) 
                      ": " obj)))

         (define (assert-index obj)
           (if (or (not (fixnum? obj)) (negative? obj))
               (error "Not an index: " obj)))

         (define (error:constructor:wna rtd args)
           (assertion-violation
            #f
            "wrong number of arguments to record constructor"
            rtd args))

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
          (lambda (x)
            (and (record? x)
                 (not (rtd-opaque? (record-type-descriptor x)))))
          (lambda (rtd . rest)
            (if (record-type-descriptor? rtd)
                (record-constructor rtd (if (null? rest)
                                            #f
                                            (car rest)))
                (apply r6rs-record-constructor rtd rest)))
          (lambda (rtd) (record-predicate rtd))
          (lambda (rtd field-name)
            (if (symbol? field-name)
                (record-accessor rtd field-name)
                (r6rs-record-accessor rtd field-name)))
          (lambda (rtd . rest)
            (if (null? rest)
                (old-style-record-mutator rtd)
                (let ((field-name (car rest)))
                  (if (symbol? field-name)
                      (record-updater rtd field-name)
                      (r6rs-record-mutator rtd field-name)))))
          (lambda (x) (record-type-descriptor x))
          (lambda (rtd) (record-indexer rtd))

          ; R6RS extensions

          (lambda (rtd) (record-type-uid rtd))
          (lambda (rtd) (record-type-generative? rtd))
          (lambda (rtd) (record-type-sealed? rtd))
          (lambda (rtd) (record-type-opaque? rtd))
          (lambda (rtd k) (record-field-mutable? rtd k))
          (lambda (name parent uid sealed? opaque? fields)
            (make-record-type-descriptor
             name parent uid sealed? opaque? fields))
))))

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
  (set! record-mutator (list-ref interface 10))
  (set! record-type-descriptor (list-ref interface 11))
  (set! record-indexer (list-ref interface 12))
  ;
  (set! record-type-uid (list-ref interface 13))
  (set! record-type-generative? (list-ref interface 14))
  (set! record-type-sealed? (list-ref interface 15))
  (set! record-type-opaque? (list-ref interface 16))
  (set! record-field-mutable? (list-ref interface 17))
  (set! make-record-type-descriptor (list-ref interface 18))
  'records)

; Table of nongenerative record type descriptors.
;
; FIXME:  Should be a hashtable instead of an association list,
; but programs that create infinitely many nongenerative record
; types are probably broken anyway.
;
; FIXME:  The transaction should be atomic, as in a hashtable.

(define record:nongenerative-rtds '())

(define (lookup-nongenerative-rtd rtd name parent uid sealed? opaque? fields)
  (let ((probe (assq uid record:nongenerative-rtds)))
    (if probe
        (let ((rtd0    (list-ref probe 1))
              (name0   (list-ref probe 2))
              (parent0 (list-ref probe 3))
              (sealed0 (list-ref probe 4))
              (opaque0 (list-ref probe 5))
              (fields0 (list-ref probe 6)))
          (if (and ; FIXME: not comparing names may be an error in the R6RS
                   (eqv? parent0 parent)
                   (eq? sealed0 sealed?)
                   (eq? opaque0 opaque?)
                   (equal? fields0 fields))
              rtd0
              (assertion-violation 'make-record-type-descriptor
                                   "nongenerative record type"
                                   name parent uid sealed? opaque? fields)))
        (let* ((fields (vector-map list-copy fields))
               (entry (list uid rtd name parent sealed? opaque? fields)))
          (set! record:nongenerative-rtds
                (cons entry record:nongenerative-rtds))
          rtd))))

; R6RS constructor descriptors.  Ugh.

(define r6rs-constructor-descriptor-type
  (make-record-type "r6rs-constructor-descriptor-type"
                    '(rtd parent-cd protocol)))

(define make-record-constructor-descriptor
  (record-constructor r6rs-constructor-descriptor-type))

(define r6rs-record-constructor-descriptor?
  (record-predicate r6rs-constructor-descriptor-type))

(define r6rs-record-constructor-descriptor-rtd
  (record-accessor r6rs-constructor-descriptor-type 'rtd))

(define r6rs-record-constructor-descriptor-parent-cd
  (record-accessor r6rs-constructor-descriptor-type 'parent-cd))

(define r6rs-record-constructor-descriptor-protocol
  (record-accessor r6rs-constructor-descriptor-type 'protocol))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Install printers.
;
; A record printer is a procedure that takes two arguments:
;     a record
;     a textual output port
;
; When called, the record printer should print some arbitrary
; representation of the record to the output port.
;
; Most record printers only know how to print instances of
; some particular record type descriptor.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given an rtd, returns the current record printer for
; instances of that rtd, or #f if no record printer is
; currently installed for that rtd (or one of its parents).

(define rtd-printer
  (record-accessor (record-type-descriptor (make-record-type "" '()))
                   'printer))

; Given a record type descriptor and a record printer
; that knows how to print instances of that rtd, installs
; the record printer so it will be used when printing
; instances of that rtd.

(define rtd-printer-set!
  (record-mutator (record-type-descriptor (make-record-type "" '()))
                  'printer))

(let* ((rtd0 (make-record-type "" '()))
       (rtdtd (record-type-descriptor rtd0))
       (get-printer (rtd-printer rtd0)))

  ; Record-type descriptor printer.

  (rtd-printer-set!
   rtdtd
   (lambda (obj port)
     (display "#<record-type-descriptor " port)
     (display (record-type-name obj) port)
     (display ">" port)))

  ; Record printer: if the record's rtd has a printer, call it.  Otherwise
  ; print #<record name>.

  (let ((previous-printer (structure-printer))
        (get-printer (rtd-printer rtdtd)))
    (structure-printer
     (lambda (obj port quote?)
       (assert (structure? obj))
       (cond ((rtd-printer (vector-ref (vector-like-ref obj 0) 0))
              =>
              (lambda (printer)
                (printer obj port)))
             ((record? obj)
              (display "#<record " port)
              (display (record-type-name
                        (record-type-descriptor obj))
                       port)
              (display ">" port))
             (else
              (previous-printer obj port quote?))))))
  'records-printers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; FIXME:  This hack makes it unnecessary to edit library code.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax library
  (syntax-rules (export import)
   ((library name (export x ...) (import y ...) form ...)
    (begin form ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS Records.
;
; This is a quick-and-dirty reference implementation that favors
; simplicity over quality error messages and performance.  It is
; implemented using the R6RS procedural and inspection layers,
; with which it interoperates nicely.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This library breaks a circular interdependence between the
; procedural and inspection layers.

(library (err5rs-helpers records rtd?)
  (export rtd?)
  (import (rnrs base) (rnrs records procedural))

  (define rtd? record-type-descriptor?)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (err5rs records inspection)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(library (err5rs records inspection)

  (export record? record-rtd
          rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?)

  (import (rnrs base)
          (rnrs lists)
          (rnrs records inspection)
          (err5rs-helpers records rtd?))

  ; The record? predicate is already defined by (rnrs records inspection).
  
  ; The record-rtd procedure is already defined by (rnrs records inspection).
  
  (define rtd-name record-type-name)
  
  (define rtd-parent record-type-parent)
  
  (define rtd-field-names record-type-field-names)
  
  (define (rtd-all-field-names rtd)
    (define (loop rtd othernames)
      (let ((parent (rtd-parent rtd))
            (names (append (vector->list
                            (rtd-field-names rtd))
                           othernames)))
        (if parent
            (loop parent names)
            (list->vector names))))
    (loop rtd '()))
  
  (define (rtd-field-mutable? rtd0 fieldname)
    (define (loop rtd)
      (if (rtd? rtd)
          (let* ((names (vector->list (rtd-field-names rtd)))
                 (probe (memq fieldname names)))
            (if probe
                (record-field-mutable? rtd (- (length names) (length probe)))
                (loop (rtd-parent rtd))))
          (assertion-violation 'rtd-field-mutable?
                               "illegal argument" rtd0 fieldname)))
    (loop rtd0))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (err5rs records procedural)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (err5rs records procedural)

  (export make-rtd rtd? rtd-constructor
          rtd-predicate rtd-accessor rtd-mutator)

  (import (rnrs base)
          (rnrs lists)
          (rnrs records procedural)
          (err5rs records inspection))

  ; Note: the options are permitted by ERR5RS,
  ; but are not part of ERR5RS.

  (define (make-rtd name fieldspecs . rest)
    (let* ((parent (if (null? rest) #f (car rest)))
           (options (if (null? rest) '() (cdr rest)))
           (sealed? (and (memq 'sealed options) #t))
           (opaque? (and (memq 'opaque options) #t))
           (uid (let ((probe (memq 'uid options)))
                  (if (and probe (not (null? (cdr probe))))
                      (cadr probe)
                      #f))))
      (make-record-type-descriptor
       name
       parent
       uid
       sealed?
       opaque?
       (vector-map (lambda (fieldspec)
                     (if (symbol? fieldspec)
                         (list 'mutable fieldspec)
                         fieldspec))
                   fieldspecs))))
  
  (define rtd? record-type-descriptor?)
  
  (define (rtd-constructor rtd . rest)
  
    ; Computes permutation and allocates permutation buffer
    ; when the constructor is created, not when the constructor
    ; is called.  More error checking is recommended.
  
    (define (make-constructor fieldspecs allnames maker)
      (let* ((k (length fieldspecs))
             (n (length allnames))
             (buffer (make-vector n (unspecified)))
             (reverse-all-names (reverse allnames)))
  
        (define (position fieldname)
          (let ((names (memq fieldname reverse-all-names)))
            (assert names)
            (- (length names) 1)))
  
        (let ((indexes (map position fieldspecs)))
  
          ; The following can be made quite efficient by
          ; hand-coding it in some lower-level language,
          ; e.g. Larceny's mal.  Even case-lambda would
          ; be good enough in most systems.
  
          (lambda args
            (assert (= (length args) k))
            (for-each (lambda (arg posn)
                        (vector-set! buffer posn arg))
                      args indexes)
            (apply maker (vector->list buffer))))))
  
    (if (null? rest)
        (record-constructor
         (make-record-constructor-descriptor rtd #f #f))
        (begin (assert (null? (cdr rest)))
               (make-constructor
                (vector->list (car rest))
                (vector->list (rtd-all-field-names rtd))
                (record-constructor
                 (make-record-constructor-descriptor rtd #f #f))))))
  
  (define rtd-predicate record-predicate)
  
  (define (rtd-accessor rtd0 fieldname)
    (define (loop rtd)
      (if (rtd? rtd)
          (let* ((names (vector->list (rtd-field-names rtd)))
                 (probe (memq fieldname names)))
            (if probe
                (record-accessor rtd (- (length names) (length probe)))
                (loop (rtd-parent rtd))))
          (assertion-violation 'rtd-accessor
                               "illegal argument" rtd0 fieldname)))
    (loop rtd0))
  
  (define (rtd-mutator rtd0 fieldname)
    (define (loop rtd)
      (if (rtd? rtd)
          (let* ((names (vector->list (rtd-field-names rtd)))
                 (probe (memq fieldname names)))
            (if probe
                (record-mutator rtd (- (length names) (length probe)))
                (loop (rtd-parent rtd))))
          (assertion-violation 'rtd-mutator
                               "illegal argument" rtd0 fieldname)))
    (loop rtd0))

  )

; eof

