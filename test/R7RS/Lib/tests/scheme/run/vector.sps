(import (scheme base)
        (scheme write)
        (tests scheme vector)
        (tests scheme test))

(display "Running tests for (scheme vector)\n")
(run-vector-tests)
(report-test-results)
