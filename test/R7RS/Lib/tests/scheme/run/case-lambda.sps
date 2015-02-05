(import (tests scheme case-lambda)
        (tests scheme test)
        (scheme write))

(display "Running tests for (scheme case-lambda)\n")
(run-case-lambda-tests)
(report-test-results)
