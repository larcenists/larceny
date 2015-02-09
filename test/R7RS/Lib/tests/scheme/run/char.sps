(import (scheme base)
        (scheme write)
        (tests scheme char)
        (tests scheme test))

(display "Running tests for (scheme char)\n")
(run-char-tests)
(report-test-results)
