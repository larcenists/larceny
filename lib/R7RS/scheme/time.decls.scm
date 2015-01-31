;;; FIXME: test of include-library-declarations

(import (scheme base))

(export current-jiffy)

(import (primitives current-second current-seconds))

(include "time.body.scm")

(export current-second jiffies-per-second)
