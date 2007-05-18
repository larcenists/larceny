(require 'std-ffi)
(require 'foreign-stdlib)
(require 'foreign-sugar)

(let ((os (assq 'os-name (system-features))))
  (cond 
   ((equal? os '(os-name . "MacOS X"))
    (foreign-file "/sw/lib/libgdk-x11-2.0.dylib"))
   ((equal? os '(os-name . "Linux"))
    (foreign-file "/usr/lib/libgdk-x11-2.0.so"))
   ((equal? os '(os-name . "SunOS"))
    (foreign-file "/usr/lib/libgdk-x11-2.0.so"))
   (else
    (error "Add case in gdk.sch for os: " os))))

(establish-void*-subhierarchy! '(gdkwindow*))
(establish-void*-subhierarchy! '(gdkpixmap*))
(establish-void*-subhierarchy! '(gdkbitmap*))
(establish-void*-subhierarchy! '(gdkfont*))
(establish-void*-subhierarchy! '(gdkcolor*))

(define-foreign (gdk-pixmap-new void* int int int) gdkpixmap*)
;;; XXX how the hell am I going to handle mutable data?
;;; I can't have the C code installing untagged pointers into the vectors...
;;; Solutions:
;;; 1. I could pass a bytevector in and do a lot of nastiness...
;;; 2. I could make a (cellof X) typector that the FFI tags and untags *in place*...
(define-foreign (gdk-pixmap-create-from-xpm 
                 void* void* (maybe void*) string)
  gdkpixmap*)
(define-foreign (gdk-pixmap-colormap-create-from-xpm 
                 void* void* void* void* string)
  gdkpixmap*)

;; XXX I don't know how to manage this either... converting a
;; [Vectorof String] is quite difficult...
(define-foreign (gdk-pixmap-create-from-xpm-d 
                 void* void* (maybe void*) char**)
  gdkpixmap*)
;
;(define-foreign (gdk-pixmap-colormap-create-from-xpm-d
;                 void* void* void**

(define GDK-SHIFT-MASK    #b0000000000001)
(define GDK-LOCK-MASK     #b0000000000010)
(define GDK-CONTROL-MASK  #b0000000000100)
(define GDK-MOD1-MASK     #b0000000001000)
(define GDK-MOD2-MASK     #b0000000010000)
(define GDK-MOD3-MASK     #b0000000100000)
(define GDK-MOD4-MASK     #b0000001000000)
(define GDK-MOD5-MASK     #b0000010000000)
(define GDK-BUTTON1-MASK  #b0000100000000)
(define GDK-BUTTON2-MASK  #b0001000000000)
(define GDK-BUTTON3-MASK  #b0010000000000)
(define GDK-BUTTON4-MASK  #b0100000000000)
(define GDK-BUTTON5-MASK  #b1000000000000)
(define GDK-RELEASE-MASK  #b1000000000000000000000000000000)
(define GDK-MODIFIER-MASK (integer-logior GDK-RELEASE-MASK #x1fff))

(define-syntax define-cfields-offsets/target-dep-paths
  (syntax-rules ()
    ((_ (HEADERS ...) FORMS ...)
     (cond-expand
      (macosx 
       (define-c-info 
         (path "/sw/include/glib-2.0")
         (path "/sw/lib/glib-2.0/include")
         (path "/sw/lib/gtk-2.0/include") 
         (path "/sw/include/pango-1.0")
         (path "/sw/include/atk-1.0")
         (path "/sw/include/gtk-2.0")
         (include<> HEADERS) ...
         FORMS ...))
      (unix
       (define-c-info
         (path "/usr/include/glib-2.0")
         (path "/usr/lib/glib-2.0/include")
         (path "/usr/lib/gtk-2.0/include") 
         (path "/usr/include/pango-1.0")
         (path "/usr/include/cairo")
         (path "/usr/include/atk-1.0") 
         (path "/usr/include/gtk-2.0")
         (include HEADERS) ...
         FORMS ...))
      (else
       (error 'define-cfields-offsets ": no support for your target..."))))))

(define-cfields-offsets/target-dep-paths ("gdk/gdk.h")
  (fields "GdkEventKey" (gdkeventkey-keyval-offset "keyval")))
(define (gdk-event-keyval e)
  (integer->char (void*-word-ref e gdkeventkey-keyval-offset)))
