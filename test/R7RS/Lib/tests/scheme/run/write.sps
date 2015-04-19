(import (scheme base)
        (scheme write)
        (tests scheme write)
        (tests scheme test))

(display "Running tests for (scheme write)\n")
(run-write-tests)
(report-test-results)
