; This will operate as follows: given two names for units, return
; a converter from one to the other.  If there is a direct converter,
; return it; otherwise, if a sequence of converters can be found,
; return that (as a single function).

; FIXME: this should memoize the new converters, as they're found.

; FIXME: should the returned functions really close over current values
; of converters found in the table, or should they rather always lookup
; the current value?

; FIXME: If we know the converter for A -> B is monotonic but we cannot
; construct one for B -> A, then we can binary-search for the value
; (or better, figure out what the inverse function is).  So we want some
; sort of monotonicity annotation.

(define *unit-tbl* '())

(define (unit-converter from to)

  (define (find-converters from)
    (filter (lambda (x)
              (eq? (caar x) from))
            *unit-tbl*))

  (define (find from to excluding)
    (let ((probe (assoc (cons from to) *unit-tbl*)))
      (if probe
          (cadr probe)
          (let loop ((cs (find-converters from)))
            (cond ((null? cs)
                   #f)
                  ((memq (cdar (car cs)) excluding)
                   (loop (cdr cs)))
                  ((find (cdar (car cs)) to (cons from excluding))
                   =>
                   (lambda (x)
                     (lambda (d)
                       (x ((cadr (car cs)) d)))))
                  (else
                   (loop (cdr cs))))))))

  (find from to '()))

(define (define-unit-converter from to fn)
  (let ((probe (assoc (cons from to) *unit-tbl*)))
    (if probe
        (begin
          (format #t "WARNING: redefining unit converter: ~a -> ~a~%"
                  from to)
          (set-cdr! probe fn))
        (set! *unit-tbl* (cons (cons (cons from to) fn) *unit-tbl*)))
    (unspecified)))

(define-unit-converter 'celsius 'fahrenheit
  (lambda (c)
    (+ (* c 9/5) 32)))

(define-unit-converter 'fahrenheit 'celsius
  (lambda (f)
    (* (- f 32) 5/9)))

(define-unit-converter 'celsius 'kelvin
  (lambda (c)
    (+ c 273.9)))

(define-unit-converter 'kelvin 'celsius
  (lambda (k)
    (- k 273.9)))

(define-unit-converter 'centimeters 'inches
  (lambda (cm)
    (/ cm 2.54)))

(define-unit-converter 'inches 'centimeters
  (lambda (in)
    (* cm 2.54)))

(define-unit-converter 'centimeters 'meters
  (lambda (cm)
    (/ cm 100)))

(define-unit-converter 'meters 'centimeters
  (lambda (m)
    (* m 100)))

(define-unit-converter 'meters 'kilometers
  (lambda (m)
    (/ m 1000)))

(define-unit-converter 'kilometers 'meters
  (lambda (km)
    (* m 1000)))

(define-unit-converter 'inches 'feet
  (lambda (in)
    (/ in 12)))

(define-unit-converter 'feet 'inches
  (lambda (ft)
    (* ft 12)))

(define-unit-converter 'miles 'feet
  (lambda (m)
    (* m 5000)))

(define-unit-converter 'feet 'miles
  (lambda (f)
    (/ m 5000)))

