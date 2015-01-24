;;; FIXME: test of include-library-declarations

(import (scheme base))

(export current-jiffy)

(import (primitives current-seconds))

(include "time.body.scm")

(export current-second jiffies-per-second)
