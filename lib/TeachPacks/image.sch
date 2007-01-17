;; This is a port of the PLT HTDP images teachpack.
;; The methods that users are intended to "see" follow. All other methods 
;; should be left unexposed.

;; image-width
;; image-height
;; overlay
;; overlay/xy
;; pinhole-x
;; pinhole-y
;; move-pinhole
;; put-pinhole
;; rectangle
;; circle
;; ellipse
;; triangle
;; line
;; star
;; add-line
;; text
;; shrink
;; shrink-tl
;; shrink-tr
;; shrink-bl
;; shrink-br
;; image-inside?
;; find-image
;; image->color-list
;; color-list->image
;; image->alpha-color-list
;; alpha-color-list->image
;; image-color?
;; make-color
;; color-red
;; color-green
;; color-blue
;; color?
;; make-alpha-color
;; alpha-color-alpha
;; alpha-color-red
;; alpha-color-green
;; alpha-color-blue
;; alpha-color?

;; Additionally, we define 
;;   image-from-file
;; to be an exposed method to allow users to create an image from a bitmap file

;; Lastly, the image record-type and the image? predicate should be 
;; exposed to the user, but the record-type's constructor and accessor 
;; methods should remain hidden.

(require 'srfi-9)
(require "TeachPacks/Support/image-dotnet")
(require "TeachPacks/Support/misc")

;; argument-checking procedures
(define (check name p? v desc arg-posn) 
  (check-arg name (p? v) desc arg-posn v))

(define (check-coordinate name val arg-posn) 
  (check name number? val "number" arg-posn))
(define (check-size name val arg-posn) 
  (check name posi? val "positive exact integer" arg-posn))
(define (check-size/0 name val arg-posn) 
  (check name nnosi? val "non-negative exact integer" arg-posn))
(define (check-image name val arg-posn) 
  (check name image? val "image" arg-posn))
(define (check-image-color name val arg-posn)
  (check name image-color? val "image-color" arg-posn))
(define (check-mode name val arg-posn)
  (check name mode? val mode-str arg-posn))

(define mode-str "'solid \"solid\" 'outline or \"outline\"")

;; predicates
(define (posi? i) 
  (and (number? i) (integer? i) (positive? i) (exact? i)))
(define (nnosi? i) 
  (and (number? i) (integer? i) (exact? i) (or (zero? i) (positive? i))))
(define (mode? x)
  (member x '(solid "solid" outline "outline")))

;; alpha-color data type
(define-record-type :alpha-color
  (alpha-color alpha red green blue)
  alpha-color?
  (alpha alpha-color-alpha)
  (red alpha-color-red)
  (green alpha-color-green)
  (blue alpha-color-blue))

;; color data type
(define-record-type :color
  (color red green blue)
  color?
  (red color-red)
  (green color-green)
  (blue color-blue))

;; bitmap data type
;; p : (vector-of argb)
;; w : int
(define-record-type :bitmap
  (bitmap b)
  bitmap?
  (b bitmap-rep))

;; posn data type
;; x : int
;; y : int
(define-record-type :posn
  (posn x y)
  posn?
  (x posn-x set-posn-x!)
  (y posn-y set-posn-y!))

;; image data type
(define-record-type :image
  (new:image w h b ph)
  image?
  (w :image-width)
  (h :image-height)
  (b :image-bitmap)
  (ph :image-pinhole))
  
;; type-checked constructors for above types

(define (make-alpha-color a r g b)
  (cond
    ((not (and (integer? a) (>= a 0) (<= a 255)))
     (error 'make-alpha-color ": alpha must be an integer between 0 and 255"))
    ((not (and (integer? r) (>= r 0) (<= r 255)))
     (error 'make-alpha-color ": red must be an integer between 0 and 255"))
    ((not (and (integer? g) (>= g 0) (<= g 255)))
     (error 'make-alpha-color ": green must be an integer between 0 and 255"))
    ((not (and (integer? b) (>= b 0) (<= b 255)))
     (error 'make-alpha-color ": blue must be an integer between 0 and 255"))
    (else (alpha-color a r g b))))

(define (make-color r g b)
  (cond
    ((not (and (integer? r) (>= r 0) (<= r 255)))
     (error 'make-color ": red must be an integer between 0 and 255"))
    ((not (and (integer? g) (>= g 0) (<= g 255)))
     (error 'make-color ": green must be an integer between 0 and 255"))
    ((not (and (integer? b) (>= b 0) (<= b 255)))
     (error 'make-color ": blue must be an integer between 0 and 255"))
    (else (color r g b))))

(define (make-bitmap b)
  (cond 
    ((not (bitmap-rep? b))
     (error 'make-bitmap ": invalid bitmap representation"))
    (else (bitmap b))))

(define (make-posn x y)
  (cond
    ((not (integer? x)) 
     (error 'make-posn ": x must be an integer"))
    ((not (integer? y))
     (error 'make-posn ": y must be an integer"))
    (else (posn x y))))

(define (make-image w h b ph)
  (cond 
    ((not (integer? w))
     (error 'make-image ": width must be an integer"))
    ((not (integer? h))
     (error 'make-image ": height must be an integer"))
    ((not (bitmap? b))
     (error 'make-image ": invalid bitmap"))
    ((not (and (posn? ph) (<= 0 (posn-x ph) w) (<= 0 (posn-y ph) h)))
     (error 'make-image ": invalid pinhole"))
    (else (new:image w h b ph))))

;; Image operations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These operations are implementation-independent.

(define (image-width i)
  (check-image 'image-width i "first")
  (:image-width i))

(define (image-height i)
  (check-image 'image-height i "first")
  (:image-height i))

(define (pinhole-x i)
  (check-image 'pinhole-x i "first")
  (posn-x (:image-pinhole i)))

(define (pinhole-y i)
  (check-image 'pinhole-y i "first")
  (posn-y (:image-pinhole i)))

(define (move-pinhole i dx dy)
  (check-image 'move-pinhole i "first")
  (check-coordinate 'move-pinhole dx "second")
  (check-coordinate 'move-pinhole dy "third")
  (make-image (image-width i)
              (image-height i)
              (:image-bitmap i)
              (make-posn (+ (inexact->exact dx) (pinhole-x i)) 
                         (+ (inexact->exact dy) (pinhole-y i)))))

(define (put-pinhole i px py)
  (check-image 'put-pinhole i "first")
  (check-coordinate 'put-pinhole px "second")
  (check-coordinate 'put-pinhole py "third")
  (make-image (image-width i)
              (image-height i)
              (:image-bitmap i)
              (make-posn (inexact->exact px)
                         (inexact->exact py))))

(define (color-list? cl)
  (foldl (lambda (x y) (and (image-color? x) y)) #t cl))

(define (alpha-color-list? cl)
  (foldl (lambda (x y) (and (alpha-color? x) y)) #t cl))

;; overlay : image image image ... -> image 
;; to add the pixels of the second image onto the 
;; first image, lining up the pinholes 
(define (overlay a b . cs)
  (check-image 'overlay a "first")
  (check-image 'overlay b "second")
  ;; need to check all of the images in cs
  (let ((all-imgs (reverse (list* a b cs))))
    (let loop ((imgs (cdr all-imgs))
               (comp (car all-imgs)))
      (cond 
        ((null? imgs) comp)
        (else (loop (cdr imgs)
                    (overlay/xy (car imgs) 0 0 comp)))))))

;; shrink-tl : image number number -> image 
;; to shrink the image, starting from the top-left corner. 
;; The two numbers indicate how many pixels to save. 
;; The pinhole of the resulting image is in the middle of the image. 
(define (shrink-tl raw-img x y)
  (check-image 'shrink-tl raw-img "first")
  (check-size 'shrink-tl x "second")
  (check-size 'shrink-tl y "third")
  (put-pinhole 
   (shrink (put-pinhole raw-img 0 0) 0 0 (- x 1) (- y 1)) (/ x 2) (/ y 2)))  

;; shrink-tr : image number number -> image 
;; to shrink the image, starting from the top-right corner.
;; The two numbers indicate how many pixels to save. 
;; The pinhole of the resulting image is in the middle of the image. 
(define (shrink-tr raw-img x y)
  (check-image 'shrink-tr raw-img "first")
  (check-size 'shrink-tr x "second")
  (check-size 'shrink-tr y "third")
  (put-pinhole 
   (shrink 
    (put-pinhole raw-img (- (image-width raw-img) 1) 0) (- x 1) 0 0 (- y 1))
   (/ x 2)
   (/ y 2)))

;; shrink-bl : image number number -> image 
;; to shrink the image, starting from the bottom-left corner. 
;; The two numbers indicate how many pixels to save. 
;; The pinhole of the resulting image is in the middle of the image. 
(define (shrink-bl raw-img x y)
  (check-image 'shrink-bl raw-img "first")
  (check-size 'shrink-bl x "second")
  (check-size 'shrink-bl y "third")
  (put-pinhole 
   (shrink 
    (put-pinhole raw-img 0 (- (image-height raw-img) 1)) 0 (- y 1) (- x 1) 0)
   (/ x 2)
   (/ y 2)))

;; shrink-br : image number number -> image 
;; to shrink the image, starting from the bottom-right corner. 
;; The two numbers indicate how many pixels to save. 
;; The pinhole of the resulting image is in the middle of the image. 
(define (shrink-br raw-img x y)
  (check-image 'shrink-br raw-img "first")
  (check-size 'shrink-br x "second")
  (check-size 'shrink-br y "third")
  (put-pinhole 
   (shrink (put-pinhole raw-img (- (image-width raw-img) 1) 
                        (- (image-height raw-img) 1))
           (- x 1) (- y 1) 0 0)
   (/ x 2)
   (/ y 2)))

;; -----------------------------------------------------------------------------

;; These operations are defined by the individual implementations of the image
;; abstraction layer. For example image-dotnet.sch uses Microsoft's .NET classes
;; to implement the operations. The implementations return images as specified
;; by the record types above.

;; image-color? Any -> boolean
;; to determine if the input is a valid image color 
(define (image-color? c)
  (or (color? c) 
      (impl-image-color? c)))

;; bitmap-rep? Any -> boolean
;; determines if the given argument is a valid representation of a
;; bitmap according to the implementation
(define (bitmap-rep? b) (impl-bitmap-rep? b))

;; image-from-file : string -> image
(define (image-from-file path) (impl-image-from-file path))

;; rectangle : int int mode image-color -> image 
;; to create a rectangle using the given width, height, mode, and color 
(define (rectangle w h mode color) 
  (check-size 'rectangle w "first")
  (check-size 'rectangle h "second")
  (check-mode 'rectangle mode "third")
  (check-image-color 'rectangle color "fourth")
  (impl-rectangle w h mode color))

;; circle : int mode image-color -> image 
;; to create a circle using the given radius, mode, and color 
(define (circle r mode color)
  (check-size 'circle r "first")
  (check-mode 'circle mode "second")
  (check-image-color 'circle color "third")
  (impl-circle r mode color))

;; ellipse : int int mode image-color -> image 
;; to create an ellipse using the given width, height, and color 
(define (ellipse w h mode color)
  (check-size 'ellipse w "first")
  (check-size 'ellipse h "second")
  (check-mode 'ellipse mode "third")
  (check-image-color 'ellipse color "fourth")
  (impl-ellipse w h mode color))

;; triangle : int mode image-color -> image 
;; to create an upward pointing equilateral triangle 
;; using the given edge size and color 
(define (triangle size mode color)
  (check 'triangle
         (lambda (x) (and (real? x) (< 2 x 10000)))
         size 
         "positive real number bigger than 2"
         "first")
  (check-mode 'triangle mode "second")
  (check-image-color 'triangle color "third")
  (impl-triangle size mode color))

;; line : number number image-color -> image 
;; to create an image with a colored line from (0,0) 
;; to the point with the given coordinates 
(define (line x y color) 
  (check-coordinate 'line x "first")
  (check-coordinate 'line y "second")
  (check-image-color 'line color "third")
  (check-sizes 'line (+ x 1) (+ y 1))
  (impl-line x y color))

;; add-line : image number number number number image-color -> image 
;; to add a line to an existing image, drawn between the two given points 
(define (add-line i x0 y0 x1 y1 c)
  (check-image 'add-line i "first")
  (check-coordinate 'add-line x0 "second")
  (check-coordinate 'add-line y0 "third")
  (check-coordinate 'add-line x1 "fourth")
  (check-coordinate 'add-line y1 "fifth")
  (check-image-color 'add-line c "sixth")
  (impl-add-line i x0 y0 x1 y1 c))

;; text : string size image-color -> image 
;; to create an image of the text in the given string, 
;; with the point size, and color specified by the last two arguments 
(define (text str size color) 
  (check 'text string? str "string" "first")
  (check 'text (lambda (x) (and (integer? x) (<= 1 x 255))) size
         "integer between 1 and 255" "second")
  (check-image-color 'text color-in "third")
  (impl-text str size color))

;; overlay/xy : image int int image -> image
;; to add the pixels of the second image onto the first image. 
;; Instead of lining up on the pinhole, the second image's pinhole 
;; is lined up with an offset from the first image's pinhole. 
;; The two coordinates specify how far to the right and down the offset 
;; should be. The pinhole of the resulting image is the same place as 
;; the pinhole in the first image.
(define (overlay/xy a dx dy b) 
  (check-image 'overlay/xy a "first")
  (check-coordinate 'overlay/xy dx "second")
  (check-coordinate 'overlay/xy dy "third")
  (check-image 'overlay/xy b "fourth")
  (impl-overlay/xy a dx dy b))

;; shrink : image number number number number -> image 
;; to shrink an image around its pinhole. 
;; The numbers are the pixels to save to left, above, to the right, and
;; below the pinhole, respectively. 
;; The pixel directly on the pinhole is always saved. 
(define (shrink img left up right down)
  (check-image 'shrink img "first")
  (check-size/0 'shrink left "second")
  (check-size/0 'shrink up "third")
  (check-size/0 'shrink right "fourth")
  (check-coordinate 'shrink down "fifth")
  (impl-shrink img left up right down))

;; image->color-list : image -> list-of-color 
;; to convert an image to a list of colors 
(define (image->color-list i-raw)
  (check-image 'image->color-list i-raw "first")
  (impl-image->color-list i-raw))

;; color-list->image : list-of-color int int int int -> image 
;; to convert a list of colors to an image with the given width and height, and 
;; pinhole coordinates (the pinhole coordinates are with respect to the 
;; top-left of the image). 
(define (color-list->image cl w h px py)
  (check 'color-list->image color-list? cl "list-of-colors" "first")
  (check-size 'color-list->image w "second")
  (check-size 'color-list->image h "third")
  (check-coordinate 'color-list->image px "fourth")
  (check-coordinate 'color-list->image py "fifth")
  (if (not (and (< 0 w 10000) (< 0 h 10000)))
      (error (format-string "cannot make ~a x ~a image" w h)))
  (if (not (= (* w h) (length cl)))
      (error (format-string (string-append "given width times given height is"
                                           " ~a, but the given color list has"
                                           " ~a items")
                            (* w h) (length cl))))
  (impl-color-list->image cl w h px py))

;; star : Int[>=2] Int[>=1] Int[>=1] Mode Color -> Image
;; to create a multi-pointed star; 
;; the first number specifies the number of points, 
;; the second specifies the radius where the points begin and 
;; the third specifies the radius where they end. 
(define (star points inner-radius outer-radius mode color)
  (check 'star
         (lambda (x) (and (real? x) (< 3 x 10000)))
         points
         "positive real number bigger than or equal to 4"
         "first")
  (check-size 'star inner-radius "second")
  (check-size 'star outer-radius "second")
  (check-mode 'star mode "fourth")
  (check-image-color 'star color "fifth")
  (impl-star points inner-radius outer-radius mode color))

;; image-inside? : Image Image -> Boolean 
;; to determine whether the pixels of the second image appear in the first. 
(define (image-inside? i a) (impl-image-inside? i a))

;; find-image : Image Image -> Posn 
;; to determine where the pixels of the second image appear in the first, 
;; with respect to the pinhole of the first image. 
(define (find-image i a) (impl-find-image i a))

;; image->alpha-color-list : image -> list-of-alpha-color 
;; to convert an image to a list of alpha colors 
(define (image->alpha-color-list i)
  (check-image 'image->alpha-color-list i "first")
  (impl-image->alpha-color-list i))

;; alpha-color-list->image : list-of-alpha-color int int int int -> image 
;; to convert a list of alpha colors to an image with the given width and
;; height, and pinhole coordinates (the pinhole coordinates are 
;; with respect to the top-left of the image). 
(define (alpha-color-list->image cl w h px py)
  (check 'alpha-color-list->image 
         alpha-color-list? 
         cl 
         "list-of-alpha-colors" "first")
  (check-size 'alpha-color-list->image w "second")
  (check-size 'alpha-color-list->image h "third")
  (check-coordinate 'alpha-color-list->image px "fourth")
  (check-coordinate 'alpha-color-list->image py "fifth")
  (unless (and (< 0 w 10000) (< 0 h 10000))
    (error (format "cannot make ~a x ~a image" w h)))
  (unless (= (* w h) (length cl))
    (error (format-string (string-append "given width times given height "
                                         "is ~a, but the given color list "
                                         "has ~a items")
                          (* w h) (length cl))))
  (impl-alpha-color-list->image cl w h px py))
