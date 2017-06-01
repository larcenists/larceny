(import (scheme base)
        (scheme write)
        (tests scheme box)
        (tests scheme test))

(display "Running tests for (scheme box)\n")
(run-box-tests)
(report-test-results)
