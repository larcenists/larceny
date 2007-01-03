(require 'std-ffi)

(let ((os (assq 'os-name (system-features))))
  (cond 
   ((equal? os '(os-name . "Linux"))
    (foreign-file "/usr/lib/libgtk-x11-2.0.so.0"))    
   ((equal? os '(os-name . "MacOS X"))
    (foreign-file "/sw/lib/libgtk-x11-2.0.dylib"))
   (else
    (error "Add case in gtk.sch for os: " os))))

(define (foreign-name->string sym)
  (let* ((str (symbol->string sym))
         (lst (string->list str))
         (lst (apply append
                     (map (lambda (c) 
                            (case c
                              ((#\-) (list #\_))
                              ((#\!) (list))
                              (else  (list c))))
                          lst)))
         (str (list->string lst)))
    str))
    
(define-syntax define-foreign
  (syntax-rules ()
    ((define-foreign (NAME ARG-TYPES ...) RESULT-TYPE)
     (define NAME
       (foreign-procedure (foreign-name->string 'NAME) '(ARG-TYPES ...) 'RESULT-TYPE)))))

(define-foreign (gtk-init boxed boxed) void)
(define-foreign (gtk-window-new int) void*)
(define-foreign (gtk-widget-show void*) void)
(define-foreign (gtk-main) void)
(define-foreign (gtk-main-quit) void)
(define-foreign (g-signal-connect-data 
                 void* 
                 string 
                 (-> (void* void*) bool)
                 (maybe void*)
                 (maybe (-> (void* void*) void))
                 unsigned)
  void*)

(define-foreign (gtk-container-set-border-width void* int) void)
(define-foreign (gtk-button-new-with-label string) void*)
(define-foreign (gtk-container-add void* void*) void)
(define-foreign (gtk-window-set-title! void* string) void)
(define-foreign (gtk-hbox-new bool int) void*)
(define-foreign (gtk-vbox-new bool int) void*)
(define-foreign (gtk-label-new string) void*)
(define-foreign (gtk-hseparator-new) void*)
(define-foreign (gtk-box-pack-start void* void* bool bool int) void)
(define-foreign (gtk-box-pack-end   void* void* bool bool int) void)
(define-foreign (gtk-misc-set-alignment void* int int) void)
(define-foreign (gtk-widget-set-size-request void* int int) void)
(define-foreign (gtk-table-new int int bool) void*)
(define-foreign (gtk-table-attach-defaults void* void* int int int int)
  void)
(define-foreign (gtk-image-new-from-file string) void*)
(define-foreign (gtk-button-new) void*)
(define-foreign (gtk-radio-button-new-with-label (maybe void*) string) 
  void*)
(define-foreign (gtk-radio-button-new-with-label-from-widget 
                 void* string) 
  void*)
(define-foreign (gtk-radio-button-get-group void*) void*)
(define-foreign (gtk-toggle-button-set-active void* bool) void)

(define (g-signal-connect source signal-name f d)
  (g-signal-connect-data source signal-name f d #f 0))
(define (g-signal-connect-swapped source signal-name f d)
  (g-signal-connect-data source signal-name f d #f 2))

(define GTK-WINDOW-TOPLEVEL 0)

(define GTK-EXPAND 1)
(define GTK-SHRINK 2)
(define GTK-FILL   4)
