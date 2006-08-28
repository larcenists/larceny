;; This file introduces aliases for compiler internal procedures that
;; have periods in their names, so that one can still access them from
;; code that has has (recognize-javadot-symbols? #t).

;; Make sure you load this file with (recognize-javadot-symbols? #f)

(define-syntax def-alias 
  (syntax-rules ()
    ((def-alias def-name ref-name)
     (define (def-name . args) (apply ref-name args)))))

(def-alias il-label:key il-label.key)
(def-alias il:code il.code)
(def-alias il:args il.args)
(def-alias il-class:assembly il-class.assembly)
(def-alias il-class:namespaces il-class.namespaces)
(def-alias il-class:name il-class.name)
(def-alias il-classtype:class il-classtype.class)
(def-alias il-primtype:class il-primtype.class)
(def-alias il-arraytype:basetype il-arraytype.basetype)
(def-alias il-field:type il-field.type)
(def-alias il-field:class il-field.class)
(def-alias il-field:name il-field.name)
(def-alias il-method:instance? il-method.instance?)
(def-alias il-method:type il-method.type)
(def-alias il-method:name il-method.name)
(def-alias il-method:class il-method.class)
(def-alias il-method:argtypes il-method.argtypes)
