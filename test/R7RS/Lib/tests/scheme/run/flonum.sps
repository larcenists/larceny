(import (scheme base)
        (scheme write)
        (tests scheme flonum)
        (tests scheme test))

(display "Running tests for (scheme flonum)\n")
(run-flonum-tests)
(report-test-results)
