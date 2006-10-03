; Stateful engines.
; 2000-11-19 / lth
;
; MAKE-ENGINE thunk
;   Returns an engine, which is a procedure that takes an amount of
;   fuel and runs _thunk_ for an amount of time corresponding to the
;   fuel.  If _thunk_ returns before the fuel has been used up, then
;   the engine returns the amount of fuel remaining and the values
;   returned by _thunk_.  If the amount of fuel is used up before
;   _thunk_ returns, then the engine returns 0 values.  The fuel must
;   be a positive fixnum.
;
; ENGINE-BLOCK
;   Make the currently running engine return as if the fuel had expired,
;   surrendering any unused fuel.
;
; ENGINE-RETURN obj ...
;   Make the currently running engine return as if it had completed,
;   returning with the results obj ... .
;
;
; FIXME: if one engine runs another, the inner engine is not constrained
;   by the timeslice of the outer engine, ie, an engine can create another
;   with more fuel, or it can run itself with more fuel.
;
; FIXME: if one engine runs another, then the engine of the outer
;   is not preserved and restored across the subengine.

(define (make-engine thunk)

  (define ticks  0)                     ; assigned below

  (define return #f)                    ; assigned below

  (define (compute)                     ; assigned below
    (fluid-let ((engine-block
                 (lambda () (return)))
                (engine-return
                 (lambda results (apply return results))))
      (parameterize 
          ((timer-interrupt-handler
            (lambda ()
              (enable-interrupts (standard-timeslice))
              (call-with-current-continuation
               (lambda (again)
                 (set! compute (lambda () (again)))
                 (return)))
              (enable-interrupts ticks))))
        (enable-interrupts ticks)
        (let-values ((vs (thunk)))
          (let ((n (disable-interrupts)))
            (enable-interrupts (standard-timeslice))
            (apply return n vs))))))

  (define slop 1)                       ; implementation-dependent...

  (lambda (fuel)
    (if (not (and (positive? fuel) (fixnum? fuel)))
        (error "Invalid fuel: " fuel))
    (call-with-current-continuation
     (lambda (k)
       (set! ticks (+ fuel slop))
       (set! return k)
       (compute)))))

(define engine-block 
  (lambda ()
    (error "No engine running.")))

(define engine-return
  (lambda results
    (error "No engine running.")))
  
; eof

      




