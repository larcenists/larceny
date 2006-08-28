;;; -*-Mode: Scheme; coding: iso-8859-1 -*-

; Some macros for MzScheme compatibility

(import syntax-case-module)

(define-syntax define-values
  (lambda (form)
    (syntax-case form ()
      ((define-values (var) value)
       (syntax (define var value)))

      ((define-values vars producer)
       (with-syntax ((.... (syntax ...)))
         (syntax (letrec-syntax ((expand
                                  (syntax-rules ()
                                    ;; When vars empty, emit
                                    ((expand () ((temp var) ....))
                                     (begin
                                       (define var #f) ....
                                       (call-with-values (lambda () producer)
                                                         (lambda (temp ....)
                                                           (set! var temp) ....))))

                                    ;; generate one temporary
                                    ((expand (var . more) (temps ....))
                                     (expand more (temps .... (temp var)))))))

                                (expand vars ()))))))))

(define-syntax define-syntaxes
  (lambda (form)
    (syntax-case form ()
      ((define-syntaxes (var) producer)
       (define-syntax var producer))

      ((define-syntaxes vars producer)
       (with-syntax ((.... (syntax ...)))
         (syntax
          (letrec-syntax ((expand
                           (syntax-rules ()
                             ;; When vars empty, emit
                             ((expand () ((temp var) ....))
                              (begin
                                (define-syntax var
                                  (lambda (form)
                                    (error "Syntax definition not yet available"))) ....
                                (call-with-values (lambda () producer)
                                                  (lambda (temp ....)
                                                    (set-syntax! var temp) ....))))


                             ;; generate one temporary
                             ((expand (var . more) (temps ....))
                              (expand more (temps .... (temp var)))))))

                 (expand vars ()))))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () form . forms)
     (begin form . forms))

    ((let*-values (((var) value) . remaining-bindings) form . forms)
     (let ((var value)) form . forms))

    ((let*-values (((vars ...) values-producer) . remaining-bindings) form . forms)
     (call-with-values
      (lambda () values-producer)
      (lambda (vars ...)
        (let*-values remaining-bindings form . forms))))))

(define-syntax let-values
  (syntax-rules ()
    ;; Special case, no bindings
    ((let-values () form . forms)
     (begin form . forms))

    ;; Special case, binding isn't multiple value.
    ((let-values (((var) value)) form . forms)
     (let ((var value)) form . forms))

    ;; Special case, exactly one producer.
    ((let-values ((bindings producer)) form . forms)
     (call-with-values
      (lambda () producer)
      (lambda bindings form . forms)))

    ((let-values bdgs form . forms)
     (letrec-syntax
      ((expand (syntax-rules ()
                 ((expand "scan-bindings" () cwv-wrappers rebindings)
                  (expand "emit" cwv-wrappers (let rebindings form . forms)))

                 ((expand "scan-bindings" ((vars producer) . bindings) cwv-wrappers rebindings)
                  (expand "scan-binding" vars () producer bindings cwv-wrappers rebindings))

                 ((expand "scan-binding" () temp-pairs producer bindings cwv-wrappers rebindings)
                  (expand "build-wrapper" temp-pairs () producer bindings cwv-wrappers rebindings))

                 ((expand "scan-binding" (var . vars) temp-pairs producer bindings cwv-wrappers rebindings)
                  (expand "scan-binding" vars ((var temp) . temp-pairs) producer bindings cwv-wrappers rebindings))

                 ((expand "build-wrapper" () temps producer bindings cwv-wrappers rebindings)
                  (expand "scan-bindings" bindings ((temps producer) . cwv-wrappers) rebindings))

                 ((expand "build-wrapper" ((var temp) . temp-pairs) temps producer bindings cwv-wrappers rebindings)
                  (expand "build-wrapper" temp-pairs (temp . temps) producer bindings cwv-wrappers ((var temp) . rebindings)))

                 ((expand "emit" () body) body)

                 ;; Avoid call-with-values if binding just one.
                 ((expand "emit" (((temp) producer) . cwv-wrappers) body)
                  (expand "emit" cwv-wrappers (let ((temp producer)) body)))

                 ((expand "emit" ((temps producer) . cwv-wrappers) body)
                  (expand "emit" cwv-wrappers (call-with-values (lambda () producer)
                                                                (lambda temps body)))))))

      (expand "scan-bindings" bdgs () ())))))

(define-syntax letrec-values
  (syntax-rules ()
    ((letrec-values () form . forms)
     (begin form . forms))

    ((letrec-values (((var) value)) form . forms)
     (letrec ((var value)) form . forms))

    ((letrec-values bdgs form . forms)
     (letrec-syntax
      ((expand (syntax-rules ()
                 ((expand "scan-bindings" () outer cwv-wrappers assignments)
                  (expand "emit" outer cwv-wrappers (begin (begin . assignments) form . forms)))

                 ((expand "scan-bindings" ((vars producer) . bindings) outer cwv-wrappers assignments)
                  (expand "scan-binding" vars () producer bindings outer cwv-wrappers assignments))

                 ((expand "scan-binding" () temp-pairs producer bindings outer cwv-wrappers assignments)
                  (expand "build-wrapper" temp-pairs () producer bindings outer cwv-wrappers assignments))

                 ((expand "scan-binding" (var . vars) temp-pairs producer bindings outer cwv-wrappers assignments)
                  (expand "scan-binding" vars ((var temp) . temp-pairs) producer bindings ((var #f) . outer) cwv-wrappers assignments))
                 ((expand "build-wrapper" () temps producer bindings outer cwv-wrappers assignments)
                  (expand "scan-bindings" bindings outer ((temps producer) . cwv-wrappers) assignments))

                 ((expand "build-wrapper" ((var temp) . temp-pairs) temps producer bindings outer cwv-wrappers assignments)
                  (expand "build-wrapper" temp-pairs (temp . temps) producer bindings outer cwv-wrappers ((set! var temp) . assignments)))

                 ((expand "emit" outer () body)
                  (let outer body))

                 ((expand "emit" outer (((temp) producer) . cwv-wrappers) body)
                  (expand "emit" outer cwv-wrappers (let ((temp producer)) body)))

                 ((expand "emit" outer ((temps producer) . cwv-wrappers) body)
                  (expand "emit" outer cwv-wrappers (call-with-values (lambda () producer)
                                                                      (lambda temps body)))))))
      (expand "scan-bindings" bdgs () () ())))))





