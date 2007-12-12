;; Nonstandard library for loading files into a
;; top-level interactive REPL environment.
;; The files may contain libraries in source form,
;; which are then dynamically loaded.  

(library (rnrs load)
  (export (rename (ex:load load)))
  (import (primitives ex:load)))
  
; FIXME: the old definition was

#;
(library (rnrs load)
  (export load)
  (import (rnrs)
          (primitives ex:repl))
  
  (define (load filename)
    (define (read-file fn)
      (let ((p (open-input-file fn)))
        (let f ((x (read p)))
          (if (eof-object? x)
              (begin (close-input-port p) '())
              (cons x
                    (f (read p)))))))
    (ex:repl (read-file filename)))
  )
