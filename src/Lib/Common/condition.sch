; Copyright 2007 William D Clinger
;
; $Id$
;
; R6RS conditions.

($$trace "condition")

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
          (rnrs lists)
          (err5rs records procedural)
          (err5rs records inspection))

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

  ; define-condition-type is defined in Compiler/usual.sch
  ;
  ; Note: This relies on letrec* semantics for internal definitions.
  ;
 ;(define-syntax define-condition-type
 ;  (syntax-rules ()
 ;   ((define-condition-type <condition-type>
 ;      <supertype> <constructor> <predicate> (<field> <accessor>) ...)
 ;    (begin
 ;     (define <condition-type>
 ;       (make-rtd '<condition-type>
 ;                 (vector (list 'immutable '<field>) ...)
 ;                 <supertype>))
 ;     (define <constructor> (rtd-constructor <condition-type>))
 ;     (define <predicate> (condition-predicate <condition-type>))
 ;     (define <accessor>
 ;       (condition-accessor <condition-type>
 ;                           (rtd-accessor <condition-type> '<field>)))
 ;     ...))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; These conditions are exported by R6RS standard libraries other
; than (rnrs conditions).
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (larceny conditions other-r6rs)

  (export
   &i/o make-i/o-error i/o-error?
   &i/o-read make-i/o-read-error i/o-read-error?
   &i/o-write make-i/o-write-error i/o-write-error?
   &i/o-invalid-position make-i/o-invalid-position-error
   i/o-invalid-position-error? i/o-error-position
   &i/o-filename make-i/o-filename-error i/o-filename-error?
   i/o-error-filename
   &i/o-file-protection make-i/o-file-protection-error
   i/o-file-protection-error?
   &i/o-file-is-read-only make-i/o-file-is-read-only-error
   i/o-file-is-read-only-error?
   &i/o-file-already-exists make-i/o-file-already-exists-error
   i/o-file-already-exists-error?
   &i/o-file-does-not-exist make-i/o-file-does-not-exist-error
   i/o-file-does-not-exist-error?
   &i/o-port make-i/o-port-error i/o-port-error? i/o-error-port

   &i/o-decoding make-i/o-decoding-error i/o-decoding-error?
   &i/o-encoding make-i/o-encoding-error i/o-encoding-error?
   i/o-encoding-error-char

   &no-infinities make-no-infinities-violation no-infinities-violation?
   &no-nans make-no-nans-violation no-nans-violation?)

  (import (rnrs base) (rnrs conditions))

  ; These conditions are exported by both (rnrs io ports)
  ; and (rnrs io simple).

  (define-condition-type &i/o &error
    make-i/o-error i/o-error?)

  (define-condition-type &i/o-read &i/o
    make-i/o-read-error i/o-read-error?)

  (define-condition-type &i/o-write &i/o
    make-i/o-write-error i/o-write-error?)

  (define-condition-type &i/o-invalid-position &i/o
    make-i/o-invalid-position-error
    i/o-invalid-position-error?
    (position i/o-error-position))

  (define-condition-type &i/o-filename &i/o
    make-i/o-filename-error i/o-filename-error?
    (filename i/o-error-filename))

  (define-condition-type &i/o-file-protection
      &i/o-filename
    make-i/o-file-protection-error
    i/o-file-protection-error?)

  (define-condition-type &i/o-file-is-read-only &i/o-file-protection
    make-i/o-file-is-read-only-error
    i/o-file-is-read-only-error?)

  (define-condition-type &i/o-file-already-exists &i/o-filename
    make-i/o-file-already-exists-error
    i/o-file-already-exists-error?)

  (define-condition-type &i/o-file-does-not-exist &i/o-filename
    make-i/o-file-does-not-exist-error
    i/o-file-does-not-exist-error?)

  (define-condition-type &i/o-port &i/o
    make-i/o-port-error i/o-port-error?
    (port i/o-error-port))

  ; These conditions are exported by (rnrs io ports).

  (define-condition-type &i/o-decoding &i/o-port
    make-i/o-decoding-error i/o-decoding-error?)

  (define-condition-type &i/o-encoding &i/o-port
    make-i/o-encoding-error i/o-encoding-error?
    (char i/o-encoding-error-char))

  ; These conditions are exported by (rnrs arithmetic flonums).

  (define-condition-type &no-infinities &implementation-restriction
    make-no-infinities-violation
    no-infinities-violation?)

  (define-condition-type &no-nans &implementation-restriction
    make-no-nans-violation no-nans-violation?)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; FIXME: temporary hack, used by error.sch.
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (display-condition x . rest)
    (let ((out (if (null? rest) (current-output-port) (car rest))))
      (if (compound-condition? x)
          (begin (display "Compound condition has these components: " out)
                 (newline out)
                 (for-each (lambda (c) (display-record c out))
                           (simple-conditions x)))
          (apply display-record x rest))))
  
  (define (display-record x . rest)
    (assert (record? x))
    (parameterize ((print-length 7)
                   (print-level 7))
      (let* ((out (if (null? rest) (current-output-port) (car rest)))
             (rtd (record-rtd x))
             (name (rtd-name rtd))
             (field-names (rtd-all-field-names rtd))
             (n (vector-length field-names)))
        (write x out)
        (newline out)
        (do ((i 0 (+ i 1)))
            ((= i n))
          (display "    " out)
          (display (vector-ref field-names i) out)
          (display " : " out)
          (write ((rtd-accessor rtd (vector-ref field-names i)) x) out)
          (newline out)))))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; New for R7RS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (error-object? x) (condition? x))

(define (error-object-message c)
  (define (loop cs)
    (if (null? cs)
        ""
        (let ((c (car cs)))
          (cond ((message-condition? c)
                 (let* ((msg (condition-message c))
                        (msg2 (loop (cdr cs))))
                   (if (string=? msg2 "")
                       msg
                       (string-append msg "\n" msg2))))
                (else
                 (loop (cdr cs)))))))
  (loop (simple-conditions c)))

(define (error-object-irritants c)
  (define (loop cs)
    (if (null? cs)
        '()
        (let ((c (car cs)))
          (cond ((irritants-condition? c)
                 (append (condition-irritants c)
                         (loop (cdr cs))))
                (else
                 (loop (cdr cs)))))))
  (loop (simple-conditions c)))

(define (read-error? x)
  (or (lexical-violation? x)
      (i/o-read-error? x)
      (i/o-decoding-error? x)))

(define (file-error? x)
  (i/o-filename-error? x))

; eof
