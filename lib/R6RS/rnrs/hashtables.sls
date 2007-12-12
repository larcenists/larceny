(library (rnrs hashtables (6))

  (export

   make-eq-hashtable
   make-eqv-hashtable
   make-hashtable
   hashtable?
   hashtable-size
   hashtable-ref
   hashtable-set!
   hashtable-delete!
   hashtable-contains?
   hashtable-update!
   hashtable-copy
   hashtable-clear!
   hashtable-keys
   hashtable-entries
   hashtable-equivalence-function
   hashtable-hash-function
   hashtable-mutable?
   equal-hash
   string-hash
   string-ci-hash
   symbol-hash)

  (import
   (rnrs base)
   (primitives

    make-eq-hashtable
    make-eqv-hashtable
    make-r6rs-hashtable                      ; [Larceny]
    hashtable?
    hashtable-size
    hashtable-ref
    hashtable-set!
    hashtable-delete!
    hashtable-contains?
    hashtable-update!
    hashtable-copy
    hashtable-clear!
    hashtable-keys
    hashtable-entries
    hashtable-equivalence-function
    hashtable-hash-function
    hashtable-mutable?
    equal-hash
    string-hash
    string-ci-hash
    symbol-hash))

  ; [Larceny]
  ; Larceny's traditional make-hashtable procedure is incompatible
  ; with the R6RS procedure of the same name, so the R6RS version
  ; goes under the name of make-r6rs-hashtable in R5RS mode.

  (define make-hashtable make-r6rs-hashtable))

