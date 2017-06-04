(import (scheme base)
        (scheme write)
        (tests scheme stream)
        (tests scheme test))

(display "Running tests for (scheme stream)\n")
(run-stream-tests)
(report-test-results)
