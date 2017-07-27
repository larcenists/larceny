(import (scheme base)
        (scheme write)
        (tests scheme ephemeron)
        (tests scheme test))

(display "Running tests for (scheme ephemeron)\n")
(run-ephemeron-tests)
(report-test-results)
