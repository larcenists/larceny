(import (scheme base)
        (scheme write)
        (tests scheme text)
        (tests scheme test))

(display "Running tests for (scheme text)\n")
(run-text-tests)
(report-test-results)
