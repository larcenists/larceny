; (make-instance <type> kwd name ...) does what you think, if <type> is
; a record type descriptor.
;
; eg
; > (define point3 (make-record-type "point3" '(x y z)))
; > (make-instance point3 'x 10)
; #{"point3" (x 10) (y #f) (z #f)}

(define (make-instance rtd . inits)
  (let ((r ((record-constructor rtd '()))))
    (do ((i inits (cddr i)))
        ((null? i) r)
      (let ((kwd (car i))
            (val (cadr i)))
        ((record-updater rtd kwd) r val)))))

