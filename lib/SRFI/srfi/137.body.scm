(define-record-type <instance>
  (make-instance ancestors payload)
  instance?
  (ancestors instance-ancestors)
  (payload instance-payload))

(define (%make-type proper-ancestors payload)
  (define (type-accessor . rest)
    (if (null? rest)
	payload
	ancestors))
  (define (constructor payload)
    (make-instance ancestors payload))
  (define (predicate object)
    (and (instance? object)
	 (memq constructor (instance-ancestors object))
	 #t))
  (define (accessor object)
    (unless (predicate object)
      (error "not an instance of the correct type" object))
    (instance-payload object))
  (define (make-subtype payload)
    (%make-type ancestors payload))
  (define ancestors (cons constructor proper-ancestors))
  (values type-accessor
	    constructor
	    predicate
	    accessor
	    make-subtype))

(define (make-type payload)
  (%make-type '() payload))
