(load "../run-benchmark.sch")

(define (read-file-as-string name)
  (call-with-input-file
    name
    (lambda (in)
      (do ((x (read-char in) (read-char in))
           (chars '() (cons x chars)))
        ((eof-object? x)
         (list->string (reverse chars)))))))

(define reading-benchmark
  (case-lambda
    (()  (reading-benchmark 1000))
    ((n) (reading-benchmark n "../run-benchmark.sch"))
    ((n input)
         (let* ((str (read-file-as-string input)))
           (do ((n n (- n 1)))
             ((zero? n))
             (datum-source-locations-clear!)
             (do ((port     (open-input-string str))
                  (datum #f (read port)))
               ((eof-object? datum))))))))

(datum-source-locations? #t)

(run-benchmark
  'reading
  (lambda () (reading-benchmark 10000)))

(quit)

