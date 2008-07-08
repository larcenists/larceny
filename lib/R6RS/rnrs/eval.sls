(library (rnrs eval (6))
  (export eval environment)
  (import (core primitives)))

;; Nonstandard library for reflection on library import sets.
;; See examples file for sevaral examples.

(library (rnrs eval reflection (6))
  (export environment-bindings)
  (import (core primitives)))

