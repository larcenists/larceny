(import (scheme base)
        (scheme write)
        (tests scheme list)
        (tests scheme test))

(display "Running tests for (scheme list)\n")
(run-list-tests)
(report-test-results)
