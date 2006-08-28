;; $Id$

;; Namespaces
;; ----------

;; make-namespace : [symbol] -> namespace
;; namespace? : value -> boolean
;; namespace-symbol->identifier : symbol -> syntax
;; namespace-variable-value : symbol [boolean (-> value) namespace] -> value
;; namespace-set-variable-value! : symbol value [boolean namespace] -> void
;; namespace-undefine-variable! : symbol namespace -> void
;; namespace-mapped-symbols : [namespace] -> (listof symbol)
;; namespace-require : quoted-require-spec -> void
;; namespace-transformer-require : quoted-require-spec -> void
;; namespace-require/copy : quoted-require-spec -> void
;; namespace-require/expansion-time : quoted-require-spec -> void
;; namespace-attach-module : namespace symbol -> void
;; namespace-module-registry : namespace -> module-registry
;; module->namespace : module-path -> namespace
;; namespace-syntax-introduce : syntax -> syntax

