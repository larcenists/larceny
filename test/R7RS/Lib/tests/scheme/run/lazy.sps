(import (scheme base)
        (scheme write)
        (tests scheme lazy)
        (tests scheme test))

(display "Running tests for (scheme lazy)\n")
(run-lazy-tests)
(report-test-results)
