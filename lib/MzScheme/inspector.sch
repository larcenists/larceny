;; This "module" provides MzScheme inspectors.  The spec is in
;; section 4.5 of the MzScheme language manual.
;;
;; TODO:  Play with the structure printers to make inspectors
;;        print as #<inspector> instead of #<structure>.
;;
($$trace "inspector")

;; This interface is provided by the inspector "module"
(define make-inspector (undefined))
(define inspector? (undefined))
(define current-inspector (undefined))
;; This isn't supposed to be visible to MzScheme, but structs need
;; it to get the default opacity.
(define $sys.inspector->superior (undefined))

;; release_2 of larceny uses the MzScheme-like let-values syntax.
;; The one installed in /proj/will/Larceny requires one less pair of parens
;; (see SRFI 11)
(let ((interface-procs
       (let ((inspector-type (make-record-type 'inspector '(depth superior))))
         (let ((make-inspector (record-constructor inspector-type))
               (inspector-depth (record-accessor inspector-type 'depth))
               (predicate (record-predicate inspector-type))
               (->superior (record-accessor inspector-type 'superior)))

           ;; FIXME: This is a Larceny parameter.  It wants to be a MzScheme
           ;; parameter, once we have them.  Shouldn't matter until we
           ;; want Mz. threads.
           (let ((current (make-parameter
                           'current-inspector
                           (make-inspector 1 (make-inspector 0 (unspecified)))
                           predicate)))
             (letrec ((construct
                       ;; case-lambda is in Auxlib/macros.sch
                       (case-lambda
                         (() (construct (current)))
                         ((superior) (make-inspector
                                      (+ 1 (inspector-depth superior))
                                      superior)))))
               (vector construct predicate current ->superior)))))))

  ;; Hook up the interface to the implementation.
  (set! make-inspector (vector-ref interface-procs 0))
  (set! inspector? (vector-ref interface-procs 1))
  (set! current-inspector (vector-ref interface-procs 2))
  (set! $sys.inspector->superior (vector-ref interface-procs 3)))
