;; javadot-symbol? is exported in toplevel.sch

(let ((javadot-symbol-prop-name (gensym "javadot-symbol-prop-name")))

  ;; Mark the given symbol as a javadot symbol
  (set! symbol->javadot-symbol
        (lambda (sym)
          (putprop sym javadot-symbol-prop-name #t)
          sym))

  ;; Mark the given symbol as _not_ a javadot symbol
  (set! javadot-symbol->symbol
        (lambda (sym)
          (putprop sym javadot-symbol-prop-name #f)
          sym))

  ;; Consumes a string and produces a javadot symbol
  (set! string->javadot-symbol
        (lambda (str)
          (symbol->javadot-symbol (string->symbol str))))

  ;; any -> boolean
  (set! javadot-symbol?
        (lambda (sym)
          (and (symbol? sym)
               (getprop sym javadot-symbol-prop-name)))))


                      