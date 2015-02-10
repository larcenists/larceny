(import (scheme base)
        (scheme write)
        (tests scheme time)
        (tests scheme test))

(display "Running tests for (scheme time)\n")
(write (round (/ (run-time-tests) 1e6)))
(display " megaloops/s\n")
(report-test-results)
