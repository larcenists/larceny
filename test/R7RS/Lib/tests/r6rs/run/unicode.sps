(import (tests r6rs unicode)
        (tests scheme test)
        (scheme write))
(display "Running tests for (rnrs unicode)\n")
(run-unicode-tests)
(report-test-results)
