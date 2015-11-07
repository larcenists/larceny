(import (scheme base)
        (scheme write)
        (tests scheme read)
        (tests scheme test))

(display "Running tests for (scheme read)\n")
(run-read-tests)
(report-test-results)
