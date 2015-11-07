(DEFINE (FACT N)
  (IF (ZERO? N)
      1
      (* N (FACT (- N 1)))))
