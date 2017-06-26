(import (scheme base)
        (scheme write)
        (tests scheme generator)
        (tests scheme test))

(display "Running tests for (scheme generator)\n")
(run-generator-tests)
(report-test-results)
