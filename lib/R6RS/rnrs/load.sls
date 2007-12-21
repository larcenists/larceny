;; Nonstandard library for loading files into a
;; top-level interactive REPL environment.
;; The files may contain libraries in source form,
;; which are then dynamically loaded.  

(library (rnrs load)
  (export (rename (ex:load load)))
  (import (primitives ex:load)))
