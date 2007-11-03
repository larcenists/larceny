; Box data structure, compatible with Chez Scheme and MzScheme.
; 2000-05-21 / lth
;
; A box is a datum that holds a single value.
; Two boxes are EQUAL? if their contents are.
;
; Introduces BOX, BOX?, UNBOX, and SET-BOX!
; Introduces syntax #&<thing> as shorthand for a new box containing '<thing>.
;
; FIXME/BUGS/DISCUSSION:
;  - Relies on the default structure/record printer to handle printing.  It 
;    would be better to attach a printing procedure to the RTD.
;  - Relies on the default structure comparator to handle EQUAL?.  It would be
;    better to attach an equality procedure to the RTD.
;  - The macro expander prints a warning when it sees a box created by
;    the #& syntax not in a quoted constant, because it figures that
;    that constant is not self-quoting.  The right fix is probably to 
;    tell the expander that literal structures in the data stream are 
;    in fact constants -- just like procedures.

(define box)
(define box?)
(define unbox)
(define set-box!)

(let* ((boxtype (make-record-type "box" '(value)))
       (sharp-reader (readtable-ref #\#))
       (sharp-class (car sharp-reader))
       (sharp-dispatch (cadr sharp-reader))
       (sharp-dispatch-list (caddr sharp-reader)))

  (define (new-sharp-dispatch c p)
    (let ((next (peek-char p)))
      (cond ((eof-object? next)
             (error "EOF in # syntax."))
            ((char=? next #\&)
             (read-char p)
             (let ((x (read p)))
               (if (eof-object? x)
                   (error "EOF in #& syntax.")
                   (box x))))
            (else
             (sharp-dispatch c p)))))

  (set! box (record-constructor boxtype))
  (set! box? (record-predicate boxtype))
  (set! unbox (record-accessor boxtype 'value))
  (set! set-box! (record-updater boxtype 'value))

  ; The default behavior for sharp-dispatch-list is to call on the
  ; installed non-list sharp-dispatcher, so rely on that.

  (readtable-set! #\# (list sharp-class
                            new-sharp-dispatch
                            sharp-dispatch-list))

  'box)

; eof
