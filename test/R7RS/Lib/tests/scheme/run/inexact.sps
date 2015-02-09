(import (scheme base)
        (scheme write)
        (tests scheme inexact)
        (tests scheme test))

(display "Running tests for (scheme inexact)\n")
(run-inexact-tests)
(report-test-results)
