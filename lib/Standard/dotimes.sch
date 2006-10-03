; Grossly compatible with DOTIMES in Common Lisp (though for all I know
; CL updates ?var by assignment rather than rebinding).

(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (?var ?count) ?body ...)
     (dotimes (?var ?count ?var) ?body ...))
    ((dotimes (?var ?count ?result) ?body ...)
     (let ((n ?count))
       (let loop ((?var 0))
         (if (< ?var n)
             (begin ?body ...
                    (loop (+ ?var 1)))
             ?result))))))

; eof
