(import (scheme base)
        (scheme write)
        (tests scheme comparator)
        (tests scheme test))

(display "Running tests for (scheme comparator)\n")
(run-comparator-tests)
(report-test-results)
