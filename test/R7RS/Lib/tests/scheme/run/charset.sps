(import (scheme base)
        (scheme write)
        (tests scheme charset)
        (tests scheme test))

(display "Running tests for (scheme charset)\n")
(run-charset-tests)
(report-test-results)
