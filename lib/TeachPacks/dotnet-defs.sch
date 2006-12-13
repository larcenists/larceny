;; This file contains all of the aliases for .NET Reflection objects,
;; as well as any .NET-related helper functions

;; helper functions

;; Wrapper for the clr/%add-event-handler syscall
;; clr/add-event-handler! : System.Object string procedure -> #t
(define (clr/add-event-handler! publisher event proc)
  (cond
    ((not (clr/%isa? publisher clr/object-type))
     (error 'clr/add-event-handler! ": publisher was not a System.Object"))
    ((not (string? event))
     (error 'clr/add-event-handler! ": invalid event"))
    ((not (procedure? proc)) 
     (error 'clr/add-event-handler! ": invalid callback procedure"))
    (else
     (clr/%add-event-handler publisher event (clr/%foreign-box proc)))))

;; clr/enum-value : dotnet-type string -> System.Object
;; returns the value of the given entry in the 
;; enumeration specified by the given type
(define (clr/enum-value type entry)
  (clr-enum/to-object type (clr/parse-enum type entry)))  

;; Reflection objects (Type, MethodInfo, PropertyInfo, etc.)

;; System
(define clr/object-type (find-clr-type "System.Object"))
(define clr/object-array-type (find-clr-type "System.Object[]"))
(define clr/string-type (find-clr-type "System.String"))
(define clr/int32-type (find-clr-type "System.Int32"))
(define clr/float-type (find-clr-type "System.Single"))
(define clr/boolean-type (find-clr-type "System.Boolean"))
(define clr/type-type (find-clr-type "System.Type"))

;; System.Reflection
(define clr/method-info-type (find-clr-type "System.Reflection.MethodInfo"))
(define clr/binding-flags-type (find-clr-type "System.Reflection.BindingFlags"))

;; System.Drawing
(define clr/gfx-type (find-clr-type "System.Drawing.Graphics"))
(define clr/image-type (find-clr-type "System.Drawing.Image"))
(define clr/bitmap-type (find-clr-type "System.Drawing.Bitmap"))
(define clr/color-type (find-clr-type "System.Drawing.Color"))
(define clr/pixel-format-type
  (find-clr-type "System.Drawing.Imaging.PixelFormat"))
(define clr/brush-type (find-clr-type "System.Drawing.Brush"))
(define clr/solid-brush-type (find-clr-type "System.Drawing.SolidBrush"))
(define clr/pen-type (find-clr-type "System.Drawing.Pen"))
(define clr/point-type (find-clr-type "System.Drawing.Point"))
(define clr/point-array-type (find-clr-type "System.Drawing.Point[]"))
(define clr/pixel-format-type
  (find-clr-type "System.Drawing.Imaging.PixelFormat"))
(define clr/rectangle-type (find-clr-type "System.Drawing.Rectangle"))
(define clr/gfx-unit-type (find-clr-type "System.Drawing.GraphicsUnit"))
(define clr/font-type (find-clr-type "System.Drawing.Font"))
(define clr/size-type (find-clr-type "System.Drawing.Size"))
(define clr/smoothing-mode-type 
  (find-clr-type "System.Drawing.Drawing2D.SmoothingMode"))

;; System.Windows.Forms
(define clr/control-type (find-clr-type "System.Windows.Forms.Control"))
(define clr/form-type (find-clr-type "System.Windows.Forms.Form"))
(define clr/timer-type (find-clr-type "System.Windows.Forms.Timer"))
(define clr/paint-event-args-type
  (find-clr-type "System.Windows.Forms.PaintEventArgs"))
(define clr/key-event-args-type
  (find-clr-type "System.Windows.Forms.KeyEventArgs"))
(define clr/text-renderer-type 
  (find-clr-type "System.Windows.Forms.TextRenderer"))
(define clr/textbox-type (find-clr-type "System.Windows.Forms.TextBox"))
(define clr/button-type (find-clr-type "System.Windows.Forms.Button"))
(define clr/control-collection-type 
  (find-clr-type "System.Windows.Forms.Control+ControlCollection"))
(define clr/control-styles-type
  (find-clr-type "System.Windows.Forms.ControlStyles"))

;; Properties and Methods
(define clr/type-get-method 
  (clr/%get-method clr/type-type "GetMethod" (vector clr/string-type
                                                     clr/binding-flags-type)))
(define clr/method-info-invoke
  (clr/%get-method clr/method-info-type
                   "Invoke" 
                   (vector clr/object-type
                           clr/object-array-type)))
                   
(define clr/binding-flags-nonpublic
  (clr/enum-value clr/binding-flags-type "NonPublic"))
(define clr/binding-flags-instance
  (clr/enum-value clr/binding-flags-type "Instance"))
(define clr/protected-method-bitmask
  (fxlogior (clr/%foreign->int clr/binding-flags-nonpublic)
            (clr/%foreign->int clr/binding-flags-instance)))

(define clr/control-styles-allpaintinginwmpaint
  (clr/enum-value clr/control-styles-type "AllPaintingInWmPaint"))
(define clr/control-styles-userpaint
  (clr/enum-value clr/control-styles-type "UserPaint"))
(define clr/control-styles-optimizeddoublebuffer
  (clr/enum-value clr/control-styles-type "OptimizedDoubleBuffer"))

(define clr/double-buffer-bitmask
  (fxlogior
   (fxlogior (clr/%foreign->int clr/control-styles-allpaintinginwmpaint)
             (clr/%foreign->int clr/control-styles-userpaint))
   (clr/%foreign->int clr/control-styles-optimizeddoublebuffer)))

(define clr/bitmap-ctor 
  (clr/%get-constructor clr/bitmap-type (vector clr/int32-type
                                                clr/int32-type
                                                clr/pixel-format-type)))
(define clr/bitmap-file-ctor 
  (clr/%get-constructor clr/bitmap-type (vector clr/string-type)))
(define clr/bitmap-set-pixel
  (clr/%get-method clr/bitmap-type "SetPixel" (vector clr/int32-type
                                                      clr/int32-type
                                                      clr/color-type)))
(define clr/bitmap-clone 
  (clr/%get-method clr/bitmap-type "Clone" (vector clr/rectangle-type
                                                   clr/pixel-format-type)))
(define clr/smoothing-mode-antialias
  (clr/enum-value clr/smoothing-mode-type "AntiAlias"))

(define clr/control-back-color
  (clr/%get-property clr/control-type "BackColor" (vector)))
(define clr/control-text (clr/%get-property clr/control-type "Text" (vector)))
(define clr/control-top (clr/%get-property clr/control-type "Top" (vector)))
(define clr/control-left (clr/%get-property clr/control-type "Left" (vector)))
(define clr/control-width (clr/%get-property clr/control-type "Width" (vector)))
(define clr/control-height
  (clr/%get-property clr/control-type "Height" (vector)))
(define clr/control-controls 
  (clr/%get-property clr/control-type "Controls" (vector)))
(define clr/control-focus
  (clr/%get-method clr/control-type "Focus" (vector)))
(define clr/control-visible
  (clr/%get-property clr/control-type "Visible" (vector)))

;; The following is a protected method, so we need to reflect "manually"
(define clr/control-set-style
  (clr/%invoke clr/type-get-method
                   clr/control-type 
                   (vector (clr/string->foreign "SetStyle")
                           (clr/int->foreign clr/protected-method-bitmask))))

(define clr/textbox-ctor (clr/%get-constructor clr/textbox-type (vector)))  

(define clr/button-ctor (clr/%get-constructor clr/button-type (vector)))

(define clr/control-collection-add
  (clr/%get-method clr/control-collection-type "Add" (vector clr/control-type)))

(define clr/timer-ctor (clr/%get-constructor clr/timer-type (vector)))
(define clr/timer-stop (clr/%get-method clr/timer-type "Stop" (vector)))
(define clr/timer-start (clr/%get-method clr/timer-type "Start" (vector)))
(define clr/timer-interval
  (clr/%get-property clr/timer-type "Interval" (vector)))

(define clr/form-ctor (clr/%get-constructor clr/form-type (vector)))
(define clr/form-show-dialog 
  (clr/%get-method clr/form-type "ShowDialog" (vector)))
(define clr/form-refresh
  (clr/%get-method clr/form-type "Refresh" (vector)))

(define clr/gfx-smoothing-mode
  (clr/%get-property clr/gfx-type "SmoothingMode" (vector)))
(define clr/gfx-dispose (clr/%get-method clr/gfx-type "Dispose" (vector)))
(define clr/gfx-from-image
  (clr/%get-method clr/gfx-type "FromImage" (vector clr/image-type)))
(define clr/gfx-clear 
  (clr/%get-method clr/gfx-type "Clear" (vector clr/color-type)))
(define clr/gfx-draw-image
  (clr/%get-method clr/gfx-type "DrawImage" (vector clr/image-type 
                                                    clr/int32-type
                                                    clr/int32-type)))
(define clr/gfx-draw-image-with-size
  (clr/%get-method clr/gfx-type "DrawImage" (vector clr/image-type 
                                                    clr/int32-type
                                                    clr/int32-type
                                                    clr/int32-type
                                                    clr/int32-type)))
(define clr/gfx-draw-image-rect
  (clr/%get-method clr/gfx-type "DrawImage" (vector clr/image-type 
                                                    clr/int32-type 
                                                    clr/int32-type
                                                    clr/rectangle-type
                                                    clr/gfx-unit-type)))
(define clr/gfx-draw-image-unscaled
  (clr/%get-method clr/gfx-type "DrawImageUnscaled" (vector clr/image-type
                                                            clr/int32-type
                                                            clr/int32-type)))
(define clr/gfx-draw-line 
  (clr/%get-method clr/gfx-type "DrawLine" (vector clr/pen-type  
                                                   clr/int32-type 
                                                   clr/int32-type 
                                                   clr/int32-type 
                                                   clr/int32-type)))
(define clr/gfx-draw-rectangle 
  (clr/%get-method clr/gfx-type "DrawRectangle" (vector clr/pen-type 
                                                        clr/int32-type
                                                        clr/int32-type
                                                        clr/int32-type 
                                                        clr/int32-type)))
(define clr/gfx-fill-rectangle
  (clr/%get-method clr/gfx-type "FillRectangle" (vector clr/brush-type 
                                                        clr/int32-type 
                                                        clr/int32-type 
                                                        clr/int32-type 
                                                        clr/int32-type)))
(define clr/gfx-draw-ellipse 
  (clr/%get-method clr/gfx-type "DrawEllipse" (vector clr/pen-type 
                                                      clr/int32-type 
                                                      clr/int32-type 
                                                      clr/int32-type 
                                                      clr/int32-type)))
(define clr/gfx-fill-ellipse
  (clr/%get-method clr/gfx-type "FillEllipse" (vector clr/brush-type
                                                      clr/int32-type 
                                                      clr/int32-type 
                                                      clr/int32-type 
                                                      clr/int32-type)))
(define clr/gfx-draw-string
  (clr/%get-method clr/gfx-type "DrawString" (vector clr/string-type
                                                     clr/font-type
                                                     clr/brush-type
                                                     clr/int32-type
                                                     clr/int32-type)))
(define clr/gfx-draw-polygon 
  (clr/%get-method clr/gfx-type "DrawPolygon" (vector clr/pen-type 
                                                      clr/point-array-type)))
(define clr/gfx-fill-polygon
  (clr/%get-method clr/gfx-type "FillPolygon" (vector clr/brush-type 
                                                      clr/point-array-type)))


(define clr/paint-event-args-graphics
  (clr/%get-property clr/paint-event-args-type "Graphics" (vector)))

(define clr/key-event-args-key-code
  (clr/%get-property clr/key-event-args-type "KeyCode" (vector)))
(define clr/key-event-args-shift
  (clr/%get-property clr/key-event-args-type "Shift" (vector)))

(define clr/rectangle-ctor
  (clr/%get-constructor clr/rectangle-type (vector clr/int32-type
                                                   clr/int32-type
                                                   clr/int32-type
                                                   clr/int32-type)))

(define clr/solid-brush-ctor 
  (clr/%get-constructor clr/solid-brush-type (vector clr/color-type)))

(define clr/pen-ctor
  (clr/%get-constructor clr/pen-type (vector clr/color-type)))

(define clr/point-ctor 
  (clr/%get-constructor clr/point-type (vector clr/int32-type clr/int32-type)))

(define clr/font-ctor 
  (clr/%get-constructor clr/font-type (vector clr/string-type clr/float-type)))

(define clr/image-clone (clr/%get-method clr/image-type "Clone" (vector)))
                                              
(define clr/image-height
  (clr/%get-property clr/image-type "Height" (vector)))
(define clr/image-width
  (clr/%get-property clr/image-type "Width" (vector)))
(define clr/image-pixel-format
  (clr/%get-property clr/image-type "PixelFormat" (vector)))

(define clr/color-from-name 
  (clr/%get-method clr/color-type "FromName" (vector clr/string-type)))
(define clr/color-from-argb-alpha
  (clr/%get-method clr/color-type "FromArgb" (vector clr/int32-type
                                                     clr/int32-type
                                                     clr/int32-type
                                                     clr/int32-type)))
(define clr/color-from-argb
  (clr/%get-method clr/color-type "FromArgb" (vector clr/int32-type
                                                     clr/int32-type
                                                     clr/int32-type)))
(define clr/color-a
  (clr/%get-property clr/color-type "A" (vector)))
(define clr/color-r
  (clr/%get-property clr/color-type "R" (vector)))
(define clr/color-g
  (clr/%get-property clr/color-type "G" (vector)))
(define clr/color-b
  (clr/%get-property clr/color-type "B" (vector)))
(define clr/color-is-known-color
  (clr/%get-property clr/color-type "IsKnownColor" (vector)))

(define clr/text-renderer-measure-text
  (clr/%get-method clr/text-renderer-type 
                   "MeasureText" 
                   (vector clr/string-type clr/font-type)))

(define clr/size-width
  (clr/%get-property clr/size-type "Width" (vector)))
(define clr/size-height
  (clr/%get-property clr/size-type "Height" (vector)))