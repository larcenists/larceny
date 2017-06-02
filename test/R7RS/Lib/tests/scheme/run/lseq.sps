(import (scheme base)
        (scheme write)
        (tests scheme lseq)
        (tests scheme test))

(display "Running tests for (scheme lseq)\n")
(run-lseq-tests)
(report-test-results)
