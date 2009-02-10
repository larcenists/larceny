(require 'std-ffi)

(let ((os (assq 'os-name (system-features))))
  (cond 
   ((equal? os '(os-name . "MacOS X"))
    (foreign-file "/System/Library/Frameworks/OpenGL.framework/Libraries/libGL.dylib")
    (foreign-file "/System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib"))
   (else
    (error "Add case in gtk.sch for os: " os))))

(require 'opengl-constants)
(require 'opengl-enums)
;; The opengl-functions library is allowed to use 
;; FFI attributes exported by opengl-enums;
;; thus the require invocations come in this order.
(require 'opengl-functions)
