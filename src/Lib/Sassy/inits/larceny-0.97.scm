(define &relocs-out-of-range-rtd 
  (make-record-type-descriptor '&sassy:relocs-out-of-range
                               &serious ;; parent
                               #f       ;; generative
                               #f       ;; not sealed (can be extended)
                               #f       ;; not opaque (can be reflected upon)
                               '#((immutable target-labels) ;; Listof[LabelSym]
                                  (immutable offsets))))    ;; Listof[Integer]

(define relocs-out-of-range-condition? 
  (condition-predicate &relocs-out-of-range-rtd))

(define relocs-out-of-range-rcd
  (make-record-constructor-descriptor &relocs-out-of-range-rtd
                                      #f ;; "default" parent rcd
                                      (lambda (make-maker-of-rest)
                                        (lambda (labels offsets)
                                          (let ((p (make-maker-of-rest)))
                                            (p labels offsets))))))

;; make-relocs-out-of-range : Listof[LabelSym] Listof[Integer] -> RelocsCondition

(define make-relocs-out-of-range 
  (record-constructor relocs-out-of-range-rcd))

;; relocs-out-of-range-labels : RelocsCondition -> Listof[LabelSym]

(define relocs-out-of-range-labels 
  (let ((real-relocs-out-of-range-labels
         (record-accessor &relocs-out-of-range-rtd 0)))
    (condition-accessor &relocs-out-of-range-rtd
                        real-relocs-out-of-range-labels)))

(define (sassy-signal-labels-out-of-range labels offsets)
  (raise (condition
          (make-relocs-out-of-range labels offsets)
          (make-message-condition
           (call-with-output-string
            (lambda (p) 
              (format p "sassy: out of range ~a ~a" labels offsets)))))))
