;; Microsoft .NET implementation of image teachpack abstraction layer

;; The following functions still need implementation:
;; star
;; image-inside?
;; find-image
;; image->alpha-color-list
;; alpha-color-list->image

;; KNOWN BUG: this file's implementation of add-line actually conforms
;; to the specification of add-line in the world teachpack, i.e. it does
;; not resize the image to be a larger size if the coordinates of the line
;; extend beyond the boundaries of the image to which the lines is being
;; added. This should be fixed.

;; require the image abstraction, and dotnet definitions files

(require "TeachPacks/Support/dotnet-defs")

;; ANTIALIASING
;; right now this is a flag that if set as true, 
;; set-gfx-antialias! will turn on antialiasing.
;; (there is probably a better approach to this)
(define *antialias-gfx* #t)

;; -----------------------------------------------------------------------------
;; Helper Functions

;; a default font, change this to be something available on your system
(define DEFAULT-FONT "Arial")

;; A mode is one of:  'outline, "outline", 'solid, or "solid"

;; solid-mode? : Any -> boolean
;; predicate that determines if the given mode is solid-mode
(define (solid-mode? mode)
  (member mode '(solid "solid")))

;; outline-mode? : Any -> boolean 
;; predicate that determines if the given mode is outline-mode
(define (outline-mode? mode)
  (member mode '(outline "Outline")))

;; A color is a symbol, string, or a color record-type

;; make-dotnet-color : (symbol or string or color) -> System.Drawing.Color
(define (make-dotnet-color c)
  (cond
    ((symbol? c) (symbol->dotnet-color c))
    ((string? c) (string->dotnet-color c))
    ((color? c) (color->dotnet-color c))
    (else (error 'make-dotnet-color ": invalid color specified"))))

;; string->dotnet-color : string -> System.Drawing.Color
(define (string->dotnet-color s)
  (clr/%invoke clr/color-from-name #f (vector (clr/string->foreign s))))

;; symbol->dotnet-color : symbol -> System.Drawing.Color
(define (symbol->dotnet-color s)
  (clr/%invoke clr/color-from-name #f (vector (clr/string->foreign 
                                               (symbol->string s)))))
  
;; make-dotnet-pen : (symbol or string) -> System.Drawing.Pen
(define (make-dotnet-pen color)
  (clr/%invoke-constructor clr/pen-ctor 
                           (vector (make-dotnet-color color))))

;; make-dotnet-brush : (symbol or string) -> System.Drawing.SolidBrush
(define (make-dotnet-brush color)
  (clr/%invoke-constructor clr/solid-brush-ctor 
                           (vector (make-dotnet-color color))))

;; make-dotnet-point : int int -> System.Drawing.Point
(define (make-dotnet-point x y)
  (clr/%invoke-constructor 
   clr/point-ctor 
   (vector (clr/int->foreign (inexact->exact x)) 
           (clr/int->foreign (inexact->exact y)))))

;; make-dotnet-font : string float -> System.Drawing.Font
(define (make-dotnet-font name size)
  (clr/%invoke-constructor
   clr/font-ctor
   (vector (clr/string->foreign name) 
           (clr/flonum->foreign-single (exact->inexact size)))))

;; make-dotnet-bitmap : int int -> System.Drawing.Bitmap
(define (make-dotnet-bitmap w h)
  (clr/%invoke-constructor clr/bitmap-ctor 
                           (vector (clr/int->foreign (inexact->exact w)) 
                                   (clr/int->foreign (inexact->exact h))
                                   (clr/enum-value clr/pixel-format-type
                                                   "Format32bppArgb"))))

;; make-dotnet-bitmap-from-file : string -> System.Drawing.Bitmap
(define (make-dotnet-bitmap-from-file path)
  (clr/%invoke-constructor clr/bitmap-file-ctor 
                           (vector (clr/string->foreign path))))

;; make-dotnet-rectangle : int int int int -> System.Drawing.Rectangle
(define (make-dotnet-rectangle x y w h)
  (clr/%invoke-constructor clr/rectangle-ctor
                           (vector (clr/int->foreign x)
                                   (clr/int->foreign y)
                                   (clr/int->foreign w)
                                   (clr/int->foreign h))))

;; clone-dotnet-image : System.Drawing.Image -> System.Drawing.Image
(define (clone-dotnet-image i)
  (clr/%invoke clr/image-clone i (vector)))

;; dotnet-image->dotnet-gfx : System.Drawing.Image
;;   -> System.Drawing.Graphics
(define (dotnet-image->dotnet-gfx i)
  (clr/%invoke clr/gfx-from-image #f (vector i)))

;; dotnet-image-height : System.Drawing.Image -> int
(define (dotnet-image-height i)
  (clr/foreign->int (clr/%property-ref clr/image-height i (vector))))

;; dotnet-image-width : System.Drawing.Image -> int
(define (dotnet-image-width i)
  (clr/foreign->int (clr/%property-ref clr/image-width i (vector))))

;; dotnet-bitmap-pixel : System.Drawing.Bitmap int int -> 
;;   -> System.Drawing.Color
(define (dotnet-bitmap-pixel b x y)
  (clr/%invoke clr/bitmap-get-pixel
               b 
               (vector (clr/int->foreign (inexact->exact x)) 
                       (clr/int->foreign (inexact->exact y)))))

;; set-dotnet-bitmap-pixel!
;; System.Drawing.Bitmap int int System.Drawing.Color -> 
(define (set-dotnet-bitmap-pixel! b x y c)
  (clr/%invoke clr/bitmap-set-pixel
               b 
               (vector (clr/int->foreign (inexact->exact x)) 
                       (clr/int->foreign (inexact->exact y)) 
                       c)))

;; set-gfx-antialias! : System.Drawing.Graphics ->
(define (set-gfx-antialias! g)
  (if *antialias-gfx*
      (clr/%property-set! clr/gfx-smoothing-mode 
                          g 
                          clr/smoothing-mode-antialias
                          (vector))))

;; drawing procedure (fill/draw) selection
;; make-draw-proc : mode symbol -> System.Reflection.MethodInfo
(define (make-draw-proc mode shape)
  (if (solid-mode? mode)
      (make-fill-proc shape)
      (make-outline-proc shape)))

;; make-fill-proc : symbol -> System.Reflection.MethodInfo
(define (make-fill-proc shape)
  (case shape
    (rectangle clr/gfx-fill-rectangle)
    (ellipse clr/gfx-fill-ellipse)
    (circle clr/gfx-fill-ellipse)
    (triangle clr/gfx-fill-polygon)))

;; make-outline-proc : symbol -> System.Reflection.MethodInfo
(define (make-outline-proc shape)
  (case shape
    (rectangle clr/gfx-draw-rectangle)
    (ellipse clr/gfx-draw-ellipse)
    (circle clr/gfx-draw-ellipse)
    (triangle clr/gfx-draw-polygon)))

;; drawing tool (brush/pen) selection
;; make-draw-tool : mode color -> System.Reflection.MethodInfo
(define (make-draw-tool mode color)
  (if (solid-mode? mode)
      (make-dotnet-brush color)
      (make-dotnet-pen color)))

;; -----------------------------------------------------------------------------
;; Color Marshaling : Scheme -> Dotnet -> Scheme -> Dotnet ... and so on.

;; dotnet-color->color : System.Drawing.Color -> color
(define (dotnet-color->color c)
  (let ((r (clr/foreign->int (clr/%property-ref clr/color-r c (vector))))
        (g (clr/foreign->int (clr/%property-ref clr/color-g c (vector))))
        (b (clr/foreign->int (clr/%property-ref clr/color-b c (vector)))))
    (make-color r g b)))

;; dotnet-color->alpha-color : System.Drawing.Color -> alpha-color
;; the CLR defines an alpha value of 255 to be fully opaque, but 
;; image teachpack specifies that 0 should be fully opaque and 255 fully 
;; transparent, so we need to correct for that
(define (dotnet-color->alpha-color c)
  (let ((a (clr/foreign->int (clr/%property-ref clr/color-a c (vector))))
        (r (clr/foreign->int (clr/%property-ref clr/color-r c (vector))))
        (g (clr/foreign->int (clr/%property-ref clr/color-g c (vector))))
        (b (clr/foreign->int (clr/%property-ref clr/color-b c (vector)))))
    (make-alpha-color (- 255 a) r g b)))

;; color->dotnet-color : color -> System.Drawing.Color
(define (color->dotnet-color c)
  (clr/%invoke clr/color-from-argb
               #f 
               (vector (clr/int->foreign (color-red c))
                       (clr/int->foreign (color-green c))
                       (clr/int->foreign (color-blue c)))))

;; alpha-color->dotnet-color : alpha-color -> System.Drawing.Color
;; the CLR defines an alpha value of 255 to be fully opaque, but 
;; image teachpack specifies that 0 should be fully opaque and 255 fully 
;; transparent, so we need to correct for that
(define (alpha-color->dotnet-color c)
  (clr/%invoke clr/color-from-argb-alpha 
               #f 
               (vector (clr/int->foreign (- 255 (alpha-color-alpha c)))
                       (clr/int->foreign (alpha-color-red c))
                       (clr/int->foreign (alpha-color-green c))
                       (clr/int->foreign (alpha-color-blue c)))))

;; -----------------------------------------------------------------------------
;; Implementation of image abstraction layer functions
;; These definitions override error-throwing definitions in the image.sch file

(define (impl-image-color? c)
  (if (or (symbol? c) (string? c))
      (let ((the-color (make-dotnet-color c)))
        (clr/foreign->bool 
         (clr/%property-ref clr/color-is-known-color the-color (vector))))
      #f))

(define (impl-bitmap-rep? bmp)
  (clr/%isa? bmp clr/bitmap-type))

(define (impl-image-from-file path)
  (let* ((bmp (make-dotnet-bitmap-from-file path))
         (iw (dotnet-image-width bmp))
         (ih (dotnet-image-height bmp)))
    (make-image-center-pinhole iw ih (make-bitmap bmp))))
  
(define (make-image-center-pinhole w h bmp)
  (let ((ph (make-posn (inexact->exact (round (/ w 2))) 
                       (inexact->exact (round (/ h 2))))))
    (make-image w h bmp ph)))

(define (impl-rectangle w h m c)
  (make-image-center-pinhole w h (make-bitmap (dotnet-rectangle w h m c))))

(define (impl-circle r m c)
  (let ((d (inexact->exact (round (* 2 r)))))
    (make-image-center-pinhole d d (make-bitmap (dotnet-circle d m c)))))

(define (impl-ellipse w h m c)
  (make-image-center-pinhole w h (make-bitmap (dotnet-ellipse w h m c))))

(define (impl-triangle b m c)
  (let ((h (inexact->exact (round (sqrt (- (expt b 2) (expt (/ b 2) 2)))))))
    (make-image-center-pinhole b h (make-bitmap (dotnet-triangle b h m c)))))

(define (impl-line x y c)
  (make-image (+ 1 x) 
              (+ 1 y) 
              (make-bitmap (dotnet-line x y c)) 
              (make-posn 0 0)))

(define (impl-add-line i x0 y0 x1 y1 c)
  (make-image (image-width i) 
              (image-height i) 
              (make-bitmap 
               (dotnet-add-line (bitmap-rep (:image-bitmap i)) x0 y0 x1 y1 c))
              (make-posn 0 0)))

(define (impl-text s size c)
  (let* ((bmp (dotnet-text s size c))
         (w (dotnet-image-width bmp))
         (h (dotnet-image-height bmp)))
    (make-image-center-pinhole w h (make-bitmap bmp))))

(define (impl-overlay/xy a delta-x delta-y b)
  (let* ((first-bmp (bitmap-rep (:image-bitmap a)))
         (second-bmp (bitmap-rep (:image-bitmap b)))
         (a-w (image-width a)) (a-h (image-height a))
         (b-w (image-width b)) (b-h (image-height b))
         (a-px (pinhole-x a)) (a-py (pinhole-y a))
         (b-px (pinhole-x b)) (b-py (pinhole-y b))
         (delta-x (+ delta-x a-px (- b-px)))
         (delta-y (+ delta-y a-py (- b-py)))
         (left (min 0 delta-x))
         (top (min 0 delta-y))
         (right (max (+ delta-x b-w) a-w))
         (bottom (max (+ delta-y b-h) a-h))
         (new-w (inexact->exact (ceiling (- right left))))
         (new-h (inexact->exact (ceiling (- bottom top))))
         (a-dx (- left))
         (a-dy (- top))
         (b-dx (- delta-x left))
         (b-dy (- delta-y top))
         (new-px (- a-px left))
         (new-py (- a-py top)))
    (let*
         ((new-bmp (make-bitmap (dotnet-overlay-bitmap first-bmp
                                                      second-bmp 
                                                      new-w
                                                      new-h
                                                      a-dx
                                                      a-dy
                                                      b-dx
                                                      b-dy)))
         (new-img (make-image new-w new-h new-bmp (make-posn new-px new-py))))
      new-img)))

(define (impl-shrink img left up right down)
  (let* ((i-px (pinhole-x img))
         (i-py (pinhole-y img))
         (i-w (image-width img))
         (i-h (image-height img))
         (delta-w (- i-px left))
         (delta-h (- i-py up))
         (width (+ left right 1))
         (height (+ up down 1))
         (bmp (dotnet-crop-bitmap (bitmap-rep (:image-bitmap img))
                                  delta-w
                                  delta-h
                                  width
                                  height)))
    (make-image width height (make-bitmap bmp) (make-posn left up))))

;; The image->color-list and color-list->image functions
;; could probably benefit from an investigation into more
;; efficient implementations. Perhaps, a syscall that
;; uses the System.Drawing.Bitmap.LockBits() method to 
;; get/set color values would be more efficient.
(define (impl-image->color-list i) 
  (let* ((ih (image-height i))
         (iw (image-width i))
         (bmp (bitmap-rep (:image-bitmap i)))
         (cols (make-vector (* iw ih))))
    (let y-loop ((y 0))
      (if (< y ih)
          (let x-loop ((x 0))
            (if (= x iw)
                (y-loop (+ 1 y))
                (let ((col (dotnet-bitmap-pixel bmp x y)))
                  (vector-set! cols (+ (* y iw) x) (dotnet-color->color col))
                  (x-loop (+ 1 x)))))))
    (vector->list cols)))
          
(define (impl-color-list->image cl w h px py)
  (let* ((bmp (make-dotnet-bitmap w h))
         (cols (list->vector cl)))
    (let y-loop ((y 0))
      (if (< y h)
          (let x-loop ((x 0))
            (if (= x w)
                (y-loop (+ 1 y))
                (begin
                  (set-dotnet-bitmap-pixel! bmp 
                                           x 
                                           y
                                           (make-dotnet-color 
                                            (vector-ref cols (+ x (* y w)))))
                  (x-loop (+ 1 x)))))))
    (make-image w h (make-bitmap bmp) (make-posn px py))))

;; ----------------------------------------------------------------------------
;; Shape Drawing : functions that take Scheme-world arguments and then
;; invoke .NET drawing methods via reflection and the dotnet-ffi to 
;; produce .NET System.Drawing.Bitmap objects.

;; Many of these drawing functions 'tweak' the width, height, radius, etc.
;; arguments depending on whether or not the mode is solid or outline.
;; This is because .NET's DrawX and FillX methods act differently with 
;; respect to given size arguments. A first approach to resolving these
;; differences involved using a screen ruler to measure resulting shapes
;; and tweak the dimensions, redraw, measure... until the sizes were right. 
;; A more thorough investigation might be warranted if shapes end up not
;; drawing as expected.

;; dotnet-rectangle : int int Mode Color -> System.Drawing.Image
;; to create a rectangle using the given width, height, mode, and color 
(define (dotnet-rectangle w h mode color)
  (let* ((draw-proc (make-draw-proc mode 'rectangle))
         (draw-tool (make-draw-tool mode color))
         (bmp (make-dotnet-bitmap w h))
         (bmp-gfx (dotnet-image->dotnet-gfx bmp))
         (w (if (solid-mode? mode) w (- w 1)))
         (h (if (solid-mode? mode) h (- h 1))))
    (set-gfx-antialias! bmp-gfx)
    (clr/%invoke draw-proc bmp-gfx (vector draw-tool
                                           (clr/int->foreign 0)
                                           (clr/int->foreign 0)
                                           (clr/int->foreign w)
                                           (clr/int->foreign h)))
    (clr/%invoke clr/gfx-dispose bmp-gfx (vector))
    bmp))

;; dotnet-circle : int Mode Color -> System.Drawing.Image 
;; to create a circle using the given diameter, mode, and color 
(define (dotnet-circle d mode color)
  (let* ((draw-proc (make-draw-proc mode 'circle))
         (draw-tool (make-draw-tool mode color))
         (bmp (make-dotnet-bitmap d d))
         (bmp-gfx (dotnet-image->dotnet-gfx bmp))
         (w (if (solid-mode? mode) (+ d 1) (- d 1)))
         (h (if (solid-mode? mode) (+ d 1) (- d 1)))
         (x (if (solid-mode? mode) -1 0))
         (y (if (solid-mode? mode) -1 0)))
    (set-gfx-antialias! bmp-gfx)
    (clr/%invoke draw-proc bmp-gfx (vector draw-tool
                                           (clr/int->foreign x)
                                           (clr/int->foreign y)
                                           (clr/int->foreign w)
                                           (clr/int->foreign h)))
    (clr/%invoke clr/gfx-dispose bmp-gfx (vector))
    bmp))

;; dotnet-ellipse : int int Mode Color -> System.Drawing.Image 
;; to create an ellipse using the given width, height, and color 
(define (dotnet-ellipse w h mode color)
  (let* ((draw-proc (make-draw-proc mode 'ellipse))
         (draw-tool (make-draw-tool mode color))
         (bmp (make-dotnet-bitmap w h))
         (bmp-gfx (dotnet-image->dotnet-gfx bmp))
         (w (if (solid-mode? mode) (+ w 1) (- w 1)))
         (h (if (solid-mode? mode) (+ h 1) (- h 1)))
         (x (if (solid-mode? mode) -1 0))
         (y (if (solid-mode? mode) -1 0)))
    (set-gfx-antialias! bmp-gfx)
    (clr/%invoke draw-proc bmp-gfx (vector draw-tool
                                           (clr/int->foreign x)
                                           (clr/int->foreign y)
                                           (clr/int->foreign w)
                                           (clr/int->foreign h)))
    (clr/%invoke clr/gfx-dispose bmp-gfx (vector))
    bmp))

;; dotnet-triangle : int int Mode Color -> System.Drawing.Image 
;; to create an upward-pointing with a given base and height
(define (dotnet-triangle b h mode color)
  (let* ((draw-proc (make-draw-proc mode 'triangle))
         (draw-tool (make-draw-tool mode color))
         (bmp (make-dotnet-bitmap (+ b 1) h))
         (bmp-gfx (dotnet-image->dotnet-gfx bmp))
         (top (make-dotnet-point (round (/ b 2)) 0))
         (bl (make-dotnet-point 0 (if (solid-mode? mode) h (- h 1))))
         (br (make-dotnet-point (if (solid-mode? mode) (+ b 1) b)
                                (if (solid-mode? mode) h (- h 1))))
         (point-array (allocate-clr-array clr/point-type 3)))
    (set-gfx-antialias! bmp-gfx)
    (clr/%foreign-aset point-array 0 top)
    (clr/%foreign-aset point-array 1 bl)
    (clr/%foreign-aset point-array 2 br)
    (clr/%invoke draw-proc bmp-gfx (vector draw-tool point-array))
    (clr/%invoke clr/gfx-dispose bmp-gfx (vector))
    bmp))

;; dotnet-line : number number Color -> System.Drawing.Image 
;; to create an image with a colored line from (0,0) to the point with 
;; the given coordinates 
(define (dotnet-line x y color)
  (if (and (= x 0) (= y 0)) ;; special (0,0) case
      (let* ((bmp (make-dotnet-bitmap 1 1)))
        (clr/%invoke clr/bitmap-set-pixel
                     bmp
                     (vector (clr/int->foreign 0)
                             (clr/int->foreign 0)
                             (make-dotnet-color color)))
        bmp)
      (let* ((bmp (make-dotnet-bitmap (+ 1 x) (+ 1 y)))
             (bmp-gfx (dotnet-image->dotnet-gfx bmp)))
        (set-gfx-antialias! bmp-gfx)
        (clr/%invoke clr/gfx-draw-line bmp-gfx (vector (make-dotnet-pen color)
                                                       (clr/int->foreign 0) 
                                                       (clr/int->foreign 0)
                                                       (clr/int->foreign x) 
                                                       (clr/int->foreign y)))
        (clr/%invoke clr/gfx-dispose bmp-gfx (vector))
        bmp)))

;; dotnet-add-line : 
;;   System.Drawing.Image number number number number Color
;;     -> System.Drawing.Image 
;; to add a line to an existing image, drawn between the two given points 
(define (dotnet-add-line i x0 y0 x1 y1 color)
  (let* ((bmp (clone-dotnet-image i))
         (bmp-gfx (dotnet-image->dotnet-gfx bmp)))
    (set-gfx-antialias! bmp-gfx)
    (clr/%invoke clr/gfx-draw-line bmp-gfx (vector (make-dotnet-pen color)
                                                   (clr/int->foreign x0)
                                                   (clr/int->foreign y0)
                                                   (clr/int->foreign x1)
                                                   (clr/int->foreign y1)))
    (clr/%invoke clr/gfx-dispose bmp-gfx (vector))
    bmp))

;; dotnet-text : string size Color -> System.Drawing.Image 
;; to create an image of the text in the given string, 
;; with the point size, and color specified by the last two arguments 
(define (dotnet-text s size color)
  (let* ((f (make-dotnet-font DEFAULT-FONT size))
         (text-string (clr/string->foreign s))
         (text-size (clr/%invoke clr/text-renderer-measure-text 
                                 #f 
                                 (vector text-string f)))
         (w (clr/%property-ref clr/size-width text-size (vector)))
         (h (clr/%property-ref clr/size-height text-size (vector)))
         (bmp (make-dotnet-bitmap (+ 2 (clr/foreign->int w))
                                  (+ 2 (clr/foreign->int h))))
         (bmp-gfx (dotnet-image->dotnet-gfx bmp))
         (o (clr/int->foreign 0)))
    (set-gfx-antialias! bmp-gfx)
    (clr/%invoke clr/gfx-draw-string
                 bmp-gfx
                 (vector text-string f (make-dotnet-brush color) o o))
    bmp))

;; dotnet-overlay-bitmap : 
;; System.Drawing.Bitmap System.Drawing.Bitmap int int int int int int 
;;   -> System.Drawing.Bitmap
;; creates a w by h bitmap and draws b1 at (x1,y1) and b2 at (x2,y2)
(define (dotnet-overlay-bitmap b1 b2 w h x1 y1 x2 y2)
  (let* ((bmp (make-dotnet-bitmap w h))
         (gfx (dotnet-image->dotnet-gfx bmp))
         (b1-w (dotnet-image-width b1))
         (b1-h (dotnet-image-height b1))
         (b2-w (dotnet-image-width b2))
         (b2-h (dotnet-image-height b2)))
     (clr/%invoke clr/gfx-draw-image-with-size
                  gfx 
                  (vector b1
                          (clr/int->foreign x1)
                          (clr/int->foreign y1)
                          (clr/int->foreign b1-w)
                          (clr/int->foreign b1-h)))
    (clr/%invoke clr/gfx-draw-image-with-size
                 gfx 
                 (vector b2
                          (clr/int->foreign x2)
                          (clr/int->foreign y2)
                          (clr/int->foreign b2-w)
                          (clr/int->foreign b2-h)))
     (clr/%invoke clr/gfx-dispose gfx (vector))
     bmp))

;; dotnet-crop-bitmap : System.Drawing.Bitmap int int int int -> 
;;   -> System.Drawing.Bitmap
;; crops the given bitmap to fit a w by h rectangle located at (x,y)
(define (dotnet-crop-bitmap bmp x y w h)
  (let* ((rect (make-dotnet-rectangle x y w h))
         (pxfmt (clr/%property-ref clr/image-pixel-format bmp (vector))))
    (clr/%invoke clr/bitmap-clone bmp (vector rect pxfmt))))
