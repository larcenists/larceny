;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERR5RS records.
;
; The syntactic layer is defined in a separate file.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (err5rs records procedural)
  (export
   make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator)
  (import
   (primitives
    make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator)))

(library (err5rs records inspection)
  (export
   record? record-rtd rtd-name rtd-parent
   rtd-field-names rtd-all-field-names rtd-field-mutable?)
  (import
   (primitives
    record? record-rtd rtd-name rtd-parent
    rtd-field-names rtd-all-field-names rtd-field-mutable?)))

