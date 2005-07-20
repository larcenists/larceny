;; $Id$

;; Parameter Utilities
;; -------------------

;; make-parameter : value [(value -> value)] -> parameter procedure
;; parameter? : value -> boolean
;; parameter-procedure=? : value value -> boolean
;; current-parameterization : -> parameterization
;; parameterization? : value -> boolean



;; Primitive Parameters
;; --------------------

(define-syntax define-parameters
  (syntax-rules ()
    ((_ (param-name) . more)
     (begin
       (define param-name (make-parameter #f))
       (define-parameters . more)))
    ((_ (param-name . args) . more)
     (begin
       (define param-name (make-parameter . args))
       (define-parameters . more)))))

(define-parameters
 (current-directory #f guard:current-directory)
 (current-input-port)
 (current-output-port)
 (current-error-port)
 (global-port-print-handler)
 (port-count-lines-enabled #f)
 
 (read-case-sensitive)
 (read-square-bracket-as-paren)
 (read-curly-brace-as-paren)
 (read-accept-box)
 (read-accept-compiled)
 (read-accept-bar-quote)
 (read-accept-graph)
 (read-decimal-as-inexact)
 (read-accept-dot)
 (read-accept-quasiquote)
 
 (print-unreadable)
 (print-graph)
 (print-struct)
 (print-box)
 (print-vector-length)
 (print-hash-table)
 
 (current-prompt-read)
 (current-eval)
 (current-compile)
 (current-namespace)
 (current-print)
 (compile-allow-set!-undefined)
 
 (current-load)
 (current-load-extension)
 (current-load/use-compiled)
 (current-load-relative-directory)
 (use-compiled-file-paths)
 (current-library-collection-paths)
 (current-command-line-arguments)
 
 (current-exception-handler)
 (initial-exception-handler)
 (error-escape-handler)
 (error-display-handler)
 (error-print-width)
 (error-value->string-handler)
 (error-print-source-location)
 
 (current-security-guard)
 (current-custodian)
 (current-thread-group)
 (current-inspector)
 
 (exit-handler)
 
 (current-pseudo-random-generator)
 (current-evt-pseudo-random-generator)
 
 (current-locale)
 
 (current-module-name-resolver)
 (current-module-name-prefix)
 )

