(import (scheme base)
        (scheme write)
        (tests scheme complex)
        (tests scheme test))

(display "Running tests for (scheme complex)\n")
(run-complex-tests)
(report-test-results)
