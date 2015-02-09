(import (scheme base)
        (scheme write)
        (tests scheme eval)
        (tests scheme test))

(display "Running tests for (scheme eval)\n")
(run-eval-tests)
(report-test-results)
