(import (scheme base)
        (scheme write)
        (tests scheme list-queue)
        (tests scheme test))

(display "Running tests for (scheme list-queue)\n")
(run-list-queue-tests)
(report-test-results)
