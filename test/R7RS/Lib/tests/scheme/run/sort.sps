(import (scheme base)
        (scheme write)
        (tests scheme sort)
        (tests scheme test))

(display "Running tests for (scheme sort)\n")
(run-sort-tests)
(report-test-results)
