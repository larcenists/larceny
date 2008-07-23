;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Deprecated in Larceny, but not so deprecated as
;;; (rnrs records syntactic), which is in a separate file.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (rnrs records procedural (6))
  (export
   make-record-type-descriptor record-type-descriptor record-type-descriptor?
   make-record-constructor-descriptor record-constructor
   record-predicate record-accessor record-mutator)
  (import
   (primitives
    make-record-type-descriptor record-type-descriptor record-type-descriptor?
    make-record-constructor-descriptor record-constructor
    record-predicate record-accessor record-mutator)))

(library (rnrs records inspection (6))
  (export
   record? record-rtd record-type-name record-type-parent record-type-uid
   record-type-generative? record-type-sealed? record-type-opaque?
   record-type-field-names record-field-mutable?)
  (import
   (primitives
    record? record-rtd record-type-name record-type-parent record-type-uid
    record-type-generative? record-type-sealed? record-type-opaque?
    record-type-field-names record-field-mutable?)))

