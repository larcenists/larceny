(import (scheme base)
        (scheme write)
        (tests scheme ideque)
        (tests scheme test))

(display "Running tests for (scheme ideque)\n")
(run-ideque-tests)
(report-test-results)
