(import (scheme base)
        (scheme write)
        (tests scheme hash-table)
        (tests scheme test))

(display "Running tests for (scheme hash-table)\n")
(run-hash-table-tests)
(report-test-results)
