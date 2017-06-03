(import (scheme base)
        (scheme write)
        (tests scheme set)
        (tests scheme test))

(display "Running tests for (scheme set)\n")
(run-set-tests)
(report-test-results)
