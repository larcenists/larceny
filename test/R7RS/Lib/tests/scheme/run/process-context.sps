(import (scheme base)
        (scheme write)
        (tests scheme process-context)
        (tests scheme test))

(display "Running tests for (scheme process-context)\n")
(run-process-context-tests)
(report-test-results)
