(import (scheme base)
        (scheme write)
        (tests scheme base)
        (tests scheme test))

(display "Running tests for (scheme base)\n")
(run-base-tests)
(report-test-results)
