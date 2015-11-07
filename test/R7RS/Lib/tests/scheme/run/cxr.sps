(import (scheme base)
        (scheme write)
        (tests scheme cxr)
        (tests scheme test))

(display "Running tests for (scheme cxr)\n")
(run-cxr-tests)
(report-test-results)
