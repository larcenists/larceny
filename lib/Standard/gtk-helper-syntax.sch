(define-syntax invoke-form/target-dep-paths
  (syntax-rules ()
    ((_ FORM . ARGS)
     (cond-expand
      (macosx 
       (FORM
        ("/sw/include/gtk-2.0"
         "/sw/include/glib-2.0"
         "/sw/lib/glib-2.0/include"
         "/sw/lib/gtk-2.0/include"
         "/sw/lib/pango-ft219/include/pango-1.0"
         "/sw/include/cairo"
         "/sw/include/atk-1.0"
         "/sw/include/gtk-2.0")
        . ARGS))
      (unix
       (FORM
        ("/usr/include/glib-2.0"
         "/usr/lib/glib-2.0/include"
         "/usr/lib/gtk-2.0/include"
         "/usr/include/pango-1.0"
         "/usr/include/cairo"
         "/usr/include/atk-1.0"
         "/usr/include/gtk-2.0")
        . ARGS))
      (else
       (error 'define-cfields-offsets ": no support for your target..."))))))

(define-syntax invoke-form/expand-paths
  (syntax-rules ()
    ((_ (PATHS ...) macro-cmd . ARGS)
     (macro-cmd (path PATHS) ... . ARGS))))

(define-syntax define-cfields-offsets/target-dep-paths
  (syntax-rules ()
    ((_ (HEADERS ...) FORMS ...)
     (invoke-form/target-dep-paths 
      invoke-form/expand-paths
      define-c-info
      (include<> HEADERS) ... FORMS ...))))
