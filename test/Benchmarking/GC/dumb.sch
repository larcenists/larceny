; Dumb benchmark to test the reporting of words marked during gc.
; Example: (foo 1000000)

(define (ballast bytes)
  (do ((bytes bytes (- bytes 8))
       (x '() (cons bytes x)))
      ((zero? bytes) x)))

(define (words-benchmark bytes0 bytes1)
  (let ((x (ballast bytes0)))
    (do ((bytes1 bytes1 (- bytes1 8)))
        ((not (positive? bytes1))
         (car (last-pair x)))
        (cons (car x) bytes1))))

(define (foo n)
  (collect)
  (display-memstats (memstats))
  (run-benchmark "foo" (lambda () (words-benchmark 1000000 n)) 1)
  (display-memstats (memstats)))

