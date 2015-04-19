(import (scheme base)
        (scheme write)
        (tests scheme load)
        (tests scheme test))

(display "Running tests for (scheme load)\n")
(run-load-tests)
(report-test-results)
