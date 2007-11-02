; Copyright 2007 William D Clinger
;
; $Id$
;
; R6RS conditions.

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
; R6RS conditions.
;
; The R6RS allows condition types to be opaque, but this
; is probably an error in the R6RS.  The implementation
; below will not work with opaque condition types.
; Larceny deprecates opaque condition types.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (rnrs conditions (6))

  (export
   &condition condition simple-conditions condition?
   condition-predicate condition-accessor
   define-condition-type
   &message make-message-condition message-condition? condition-message
   &warning make-warning warning?
   &serious make-serious-condition serious-condition?
   &error make-error error?
   &violation make-violation violation?
   &assertion make-assertion-violation assertion-violation?
   &irritants make-irritants-condition irritants-condition? condition-irritants
   &who make-who-condition who-condition? condition-who
   &non-continuable make-non-continuable-violation non-continuable-violation?
   &implementation-restriction make-implementation-restriction-violation
   implementation-restriction-violation?
   &lexical make-lexical-violation lexical-violation?
   &syntax make-syntax-violation syntax-violation?
   syntax-violation-form syntax-violation-subform
   &undefined make-undefined-violation undefined-violation?)

  (import (rnrs base)
          (err5rs records procedural))

  (define &condition
   (make-rtd '&condition '#()))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; Private.
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define &compound-condition
    (make-rtd '&compound-condition
              '#((immutable components))   ; a list of simple conditions
              &condition
              'sealed))

  (define make-compound-condition (rtd-constructor &compound-condition))

  (define compound-condition? (rtd-predicate &compound-condition))

  (define compound-condition-components
    (rtd-accessor &compound-condition 'components))

  ; Returns #t iff rtd1 is a subtype of rtd2.

  (define (condition-subtype? rtd1 rtd2)
    (cond ((not (rtd? rtd1))
           #f)
          ((not (rtd? rtd2))
           #f)
          ((eqv? rtd1 rtd2)
           #t)
          (else
           (condition-subtype? (rtd-parent rtd1) rtd2))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; End private.
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (condition . args)
    (let ((components (apply append (map simple-conditions args))))
      (cond ((null? components)
             (make-compound-condition '()))
            ((null? (cdr components))
             (car components))
            (else
             (make-compound-condition components)))))

  (define (simple-conditions c)
    (if (compound-condition? c)
        (compound-condition-components c)
        (list c)))

  (define condition? (rtd-predicate &condition))

  (define (condition-predicate rtd)
    (if (and (condition-subtype? rtd &condition)
             (not (eqv? rtd &compound-condition)))
        (let ((instance-of-rtd? (rtd-predicate rtd)))
          (lambda (x)
            (or (and (record? x) (instance-of-rtd? x))
                (and (compound-condition? x)
                     (exists instance-of-rtd?
                             (compound-condition-components x))))))
        (assertion-violation 'condition-predicate "illegal argument" rtd)))

  (define (condition-accessor rtd proc)
    (define (complain)
      (assertion-violation 'condition-accessor "illegal argument" rtd proc))
    (if (and (condition-subtype? rtd &condition)
             (not (eqv? rtd &compound-condition))
             (procedure? proc))
        (let ((rtd-instance? (rtd-predicate rtd)))
          (lambda (x)
            (do ((components (simple-conditions x) (cdr components)))
                ((or (null? components)
                     (rtd-instance? (car components)))
                 (if (null? components)
                     (complain)
                     (proc (car components)))))))
        (complain)))

  ; Note: This relies on letrec* semantics for internal definitions.

  (define-syntax define-condition-type
    (syntax-rules ()
     ((define-condition-type <condition-type>
        <supertype> <constructor> <predicate> (<field> <accessor>) ...)
      (begin
       (define <condition-type>
         (make-rtd '<condition-type>
                   (vector (list 'immutable '<field>) ...)
                   <supertype>))
       (define <constructor> (rtd-constructor <condition-type>))
       (define <predicate> (condition-predicate <condition-type>))
       (define <accessor>
         (condition-accessor <condition-type>
                             (rtd-accessor <condition-type> '<field>)))
       ...))))

  (define-condition-type &message &condition
    make-message-condition message-condition?
    (message condition-message))
  
  (define-condition-type &warning &condition
    make-warning warning?)
  
  (define-condition-type &serious &condition
    make-serious-condition serious-condition?)
  
  (define-condition-type &error &serious
    make-error error?)
  
  (define-condition-type &violation &serious
    make-violation violation?)
  
  (define-condition-type &assertion &violation
    make-assertion-violation assertion-violation?)
  
  (define-condition-type &irritants &condition
    make-irritants-condition irritants-condition?
    (irritants condition-irritants))
  
  (define-condition-type &who &condition
    make-who-condition who-condition?
    (who condition-who))
  
  (define-condition-type &non-continuable &violation
    make-non-continuable-violation
    non-continuable-violation?)
  
  (define-condition-type &implementation-restriction &violation
    make-implementation-restriction-violation
    implementation-restriction-violation?)
  
  (define-condition-type &lexical &violation
    make-lexical-violation lexical-violation?)
  
  (define-condition-type &syntax &violation
    make-syntax-violation syntax-violation?
    (form syntax-violation-form)
    (subform syntax-violation-subform))
  
  (define-condition-type &undefined &violation
    make-undefined-violation undefined-violation?)
  
  )

