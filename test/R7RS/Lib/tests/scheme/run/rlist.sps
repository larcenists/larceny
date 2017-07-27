(import (scheme base)
        (scheme write)
        (tests scheme rlist)
        (tests scheme test))

(display "Running tests for (scheme rlist)\n")
(run-rlist-tests)
(report-test-results)
