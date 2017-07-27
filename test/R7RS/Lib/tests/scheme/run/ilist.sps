(import (scheme base)
        (scheme write)
        (tests scheme ilist)
        (tests scheme test))

(display "Running tests for (scheme ilist)\n")
(run-ilist-tests)
(report-test-results)
