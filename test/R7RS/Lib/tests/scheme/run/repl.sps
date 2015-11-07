(import (scheme base)
        (scheme write)
        (tests scheme repl)
        (tests scheme test))

(display "Running tests for (scheme repl)\n")
(run-repl-tests)
(report-test-results)
