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

  (import
   (rnrs base)
   (err5rs records procedural)
   (primitives
    &condition condition simple-conditions condition?
    condition-predicate condition-accessor
    &message make-message-condition message-condition? condition-message
    &warning make-warning warning?
    &serious make-serious-condition serious-condition?
    &error make-error error?
    &violation make-violation violation?
    &assertion make-assertion-violation assertion-violation?
    &irritants make-irritants-condition irritants-condition?
    condition-irritants
    &who make-who-condition who-condition? condition-who
    &non-continuable make-non-continuable-violation non-continuable-violation?
    &implementation-restriction make-implementation-restriction-violation
    implementation-restriction-violation?
    &lexical make-lexical-violation lexical-violation?
    &syntax make-syntax-violation syntax-violation?
    syntax-violation-form syntax-violation-subform
    &undefined make-undefined-violation undefined-violation?))

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
       ...)))))

