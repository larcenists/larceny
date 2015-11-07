(import (scheme base)
        (scheme write)
        (tests scheme file)
        (tests scheme test))

(display "Running tests for (scheme file)\n")
(run-file-tests)
(report-test-results)
