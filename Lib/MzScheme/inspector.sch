;; This "module" provides MzScheme inspectors.  The spec is in
;; section 4.5 of the MzScheme language manual.
;;
;; TODO:  Play with the structure printers to make inspectors
;;        print as #<inspector> instead of #<structure>.
;;



;; This interface is provided by the inspector "module"
(define make-inspector (undefined))
(define inspector? (undefined))
(define current-inspector (undefined))

(let-values
    ((construct predicate current)
     (let ((inspector-type (make-record-type 'inspector '(depth superior))))
       (let ((make-inspector (record-constructor inspector-type))
             (inspector-depth (record-accessor inspector-type 'depth))
             (predicate (record-predicate inspector-type)))

         ;; FIXME: This is a Larceny parameter.  It wants to be a MzScheme
         ;; parameter, once we have them.  Shouldn't matter until we
         ;; want Mz. threads.
         (let ((current (make-parameter
                         'current-inspector
                         (make-inspector 1 (make-inspector 0 (undefined)))
                         predicate)))
           (letrec ((construct
                     (case-lambda
                       (() (construct (current)))
                       ((superior) (make-inspector
                                    (+ 1 (inspector-depth superior))
                                    superior)))))
             (values construct predicate current))))))

  ;; Hook up the interface to the implementation.
  (set! make-inspector construct)
  (set! inspector? predicate)
  (set! current-inspector current))