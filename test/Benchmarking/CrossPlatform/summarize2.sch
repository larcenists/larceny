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
  (let* ((results (map summary:timings summaries))
         (benchmark-names (map timing:benchmark (car results))))
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

  ; Strips -r6rs-fixflo and similar suffixes from system names.

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

  (let* ((systems (map summary:system summaries))
         (systems (map short-name systems))
         (results (map summary:timings summaries))
         (timings (map (lambda (x) (assq name x))
                       results))
         (best (apply min
                      (map timing:real
                           (filter (lambda (x) x) timings)))))
    (for-each (lambda (system timing)
                (if (list? timing)
                    (graph-system system (timing:real timing) best out)
                    (graph-system system 0 best out)))
              systems
              timings)))

(define graph-system:args '())
(define graph-system:bar1 "<span style=\"background-color:#")
(define graph-system:bar2 "\">")
(define graph-system:bar3 "</span>")

(define graph-system:colors
  '(("Larceny"   "800000")
    ("Bigloo"    "000080")
    ("Chez"      "004040")
    ("Chicken"   "a06000")
    ("Gambit"    "400060")
    ("MIT"       "2000c0")
    ("MzScheme"  "008020")
    ("Petite"    "004080")
    ("Scheme48"  "600040")))

; Returns a nice color for certain popular systems,
; or returns black.

(define (system-color system)
  (let ((probe (assoc system graph-system:colors)))
    (if probe (cadr probe) "000000")))
    

(define (graph-system system timing best out)
  (if (and (number? timing)
           (positive? timing))
      (let* ((relative (/ best timing))
             (color (system-color system)))
        (left-justify system width:system out)
        (right-justify (msec->seconds timing) width:timing out)
        (left-justify "" width:gap out)
        (display graph-system:bar1 out)
        (display color out)
        (display graph-system:bar2 out)
        (display (make-string (inexact->exact
                               (round (* relative width:bar)))
                              #\space)
                 out)
        (display graph-system:bar3 out)
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

; Given a summary and a list of summaries,
; returns a list of relative performance (0.0 to 1.0)
; for every benchmark in the summary.

(define (relative-performance summary summaries)

  (let* ((timings (summary:timings summary))
         (timings (filter (lambda (t)
                            (let ((realtime (timing:real t)))
                              (and (number? realtime)
                                   (positive? realtime))))
                          timings))
         (other-results (map summary:timings summaries)))
    (map (lambda (t)
           (let* ((name (timing:benchmark t))
                  (timings (map (lambda (x) (assq name x))
                                other-results))
                  (best (apply min
                               (map timing:real
                                    (filter (lambda (x) x) timings)))))
             (/ best (timing:real t))))
         timings)))

; Given a list of positive numbers,
; returns its geometric mean.

(define (geometric-mean xs)
  (expt (apply * xs) (/ 1 (length xs))))

; Given a list of summaries, returns a list of summaries
; augmented by the geometric mean over all benchmarks.

(define (summaries-with-geometric-means summaries)

  (define name (string->symbol "geometricMean"))

  (map (lambda (summary)
         (define mean
           (* 1000
              (/ (geometric-mean (relative-performance summary summaries)))))
         (make-summary (summary:system summary)
                       (summary:hostetc summary)
                       (cons
                        (make-timing name mean mean 0)
                        (summary:timings summary))))
       summaries))
