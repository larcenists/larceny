; Graphical display of benchmark results.
;
; FIXME:  This is awful code.

; Given a list of summaries in the representation
; produced by decode-summary, and a filename or
; output port, writes ASCII bar graphs to the file
; or port.

(define (graph-benchmarks summaries out)
  (define (bad-arguments)
    (error "Bad arguments to graph-benchmarks" in))
  (cond ((string? out)
         (call-with-output-file
          out
          (lambda (out) (graph-benchmarks summaries out))))
        ((output-port? out)
         (graph-benchmarks-to-port summaries out))
        (else
         (bad-arguments))))

(define (graph-benchmarks-to-port summaries out)
  (let* ((results (map caddr summaries))
         (benchmark-names (map car (car results))))
    (for-each (lambda (name)
                (graph-benchmark-to-port name summaries out))
              benchmark-names)))

(define width:system 8)
(define width:timing 8)
(define width:gap 2)
(define width:bar 60)
(define width:total (+ width:system width:timing width:gap width:bar))

(define anchor1 "<a href=\"LINK.html#")
(define anchor2 "\">")
(define anchor3 "</a>")

(define (graph-benchmark-to-port name summaries out)

  (define (short-name system)
    (let* ((rchars (reverse (string->list system)))
           (probe (memv #\- rchars)))
      (if probe
          (short-name (list->string (reverse (cdr probe))))
          system)))    

  (display (make-string (- width:total (string-length (symbol->string name)))
                        #\space)
           out)
  (display anchor1 out)
  (display name out)
  (display anchor2 out)
  (display name out)
  (display anchor3 out)
  (newline out)

  (let* ((systems (map car summaries))
         (systems (map short-name systems))
         (results (map caddr summaries))
         (timings (map (lambda (x) (assq name x))
                       results))
         (best (apply min
                      (map caddr           ; real time
                           (filter (lambda (x) x) timings)))))
    (for-each (lambda (system timing)
                (if (list? timing)
                    (graph-system system (caddr timing) best out)))
              systems
              timings)))

(define graph-system:args '())

(define (graph-system system timing best out)
  (set! graph-system:args (list system timing best out))
  (if (and (number? timing)
           (positive? timing))
      (let* ((relative (/ best timing)))
        (left-justify system width:system out)
        (right-justify (msec->seconds timing) width:timing out)
        (left-justify "" width:gap out)
        (left-justify (make-string (inexact->exact
                                    (round (* relative width:bar)))
                                   #\*)
                      width:bar
                      out)
        (newline out))
      (begin
        (left-justify system width:system out)
        (newline out))))

; Given a timing in milliseconds,
; returns the timing in seconds,
; as a string rounded to two decimal places.

(define (msec->seconds t)
  (let* ((hundredths (inexact->exact (round (/ t 10.0))))
         (s (number->string hundredths))
         (n (string-length s)))
    (cond ((>= n 2)
           (string-append (substring s 0 (- n 2))
                          "."
                          (substring s (- n 2) n)))
          (else
           (string-append ".0" s)))))
