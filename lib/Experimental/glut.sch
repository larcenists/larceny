;;; TODO: port to freeglut

;;; Some notes from the glut-3 spec
;;; 
;;; GLUT State
;;; TYPES: Bitmask, Boolean, Callback, ColorCell, Cursor, Integer
;;;        Layer, MenuItem, MenuState, Stacking, State, String, Timer
;;; GLOBALS: currentWindow, currentMenu, initWindowX, initWindowY, 
;;;          initWindowWidth, initWindowHeight, initDisplayMode,
;;;          idleCallback, menuState, menuStateCallback, timerList
;;; FIXED: screenWidth, screenHeight, screenWidthMM, screenHeightMM,
;;;        hasKeyboard, hasMouse, hasSpaceball, hasDialAndButtonBox
;;;        hasTable, numMouseButtons, numSpaceballButtons, 
;;;        numButtonBoxButtons, numDials, numTabletButtons
;;; WINDOW: number, x, y, width, height, 
;;;         cursor, stacking, displayState, visibility, redisplay,
;;;         displayCallback, reshapeCallback, keyboardCallback, 
;;;         mouseCallback, motionCallback, passiveMotionCallback, 
;;;         specialCallback, spaceballMotionCallback, 
;;;         spaceballRotateCallback, spaceballButtonCallback, 
;;;         buttonBoxCallback, dialsCallback, tabletMotionCallback,
;;;         tabletButtonCallback, visibilityCallback, entryCallback
;;;         colormap, windowParent, numChildren, 
;;;         leftMenu, middleMenu, rightmenu
;;; TOPLEVEL: fullScreen, windowTitle, iconTitle

(require 'std-ffi)
(require 'foreign-sugar)
(require 'foreign-ctools)
(require 'foreign-cenums)
(require 'foreign-stdlib) ; for int*, char** FFI attributes
(let ((os (assq 'os-name (system-features))))
  (cond 
   ((equal? os '(os-name . "MacOS X"))
    (foreign-file "/System/Library/Frameworks/GLUT.framework/GLUT"))
   (else
    (error "Add case in gtk.sch for os: " os))))

(define-syntax define-glut-constants
  (transformer 
   (lambda (exp ren cmp)
     (let ((ids (cdr exp)))
       `(,(ren 'define-c-info) (,(ren 'include<>) "GLUT/glut.h")
         ,@(map (lambda (x) `(,(ren 'ifdefconst) 
                              ,x
                              ,(ren 'uint) 
                              ,(symbol->string x)))
                ids))))))

(define-glut-constants 
  GLUT_RGB

  GLUT_RGBA GLUT_INDEX GLUT_SINGLE GLUT_DOUBLE GLUT_ACCUM
  GLUT_ALPHA GLUT_DEPTH GLUT_STENCIL
;; #if (GLUT_API_VERSION >= 2)
  GLUT_MULTISAMPLE GLUT_STEREO
;; #endif
;; #if (GLUT_API_VERSION >= 3)
  GLUT_LUMINANCE
;; #endif
  GLUT_NO_RECOVERY

;;; /* Mouse buttons. */
  GLUT_LEFT_BUTTON GLUT_MIDDLE_BUTTON GLUT_RIGHT_BUTTON

;;; /* Mouse button  state. */
  GLUTf_DOWN GLUT_UP

;; #if (GLUT_API_VERSION >= 2)
;;;/* function keys */
 GLUT_KEY_F1 GLUT_KEY_F2 GLUT_KEY_F3 GLUT_KEY_F4
 GLUT_KEY_F5 GLUT_KEY_F6 GLUT_KEY_F7 GLUT_KEY_F8
 GLUT_KEY_F9 GLUT_KEY_F10 GLUT_KEY_F11 GLUT_KEY_F12

;;; /* directional keys */
 GLUT_KEY_LEFT GLUT_KEY_UP GLUT_KEY_RIGHT GLUT_KEY_DOWN
 GLUT_KEY_PAGE_UP GLUT_KEY_PAGE_DOWN GLUT_KEY_HOME GLUT_KEY_END
 GLUT_KEY_INSERT
;; #endif

;;; /* Entry/exit  state. */
 GLUT_LEFT GLUT_ENTERED

;;; /* Menu usage  state. */
 GLUT_MENU_NOT_IN_USE GLUT_MENU_IN_USE

;;; /* Visibility  state. */
 GLUT_NOT_VISIBLE GLUT_VISIBLE

;;; /* Window status  state. */
GLUT_HIDDEN GLUT_FULLY_RETAINED 
GLUT_PARTIALLY_RETAINED GLUT_FULLY_COVERED

;;; /* Color index component selection values. */
GLUT_RED GLUT_GREEN GLUT_BLUE

;;; /* Layers for use. */
GLUT_NORMAL GLUT_OVERLAY 
;;; /* glutGet parameters. */
GLUT_WINDOW_X GLUT_WINDOW_Y GLUT_WINDOW_WIDTH GLUT_WINDOW_HEIGHT
GLUT_WINDOW_BUFFER_SIZE GLUT_WINDOW_STENCIL_SIZE GLUT_WINDOW_DEPTH_SIZE
GLUT_WINDOW_RED_SIZE GLUT_WINDOW_GREEN_SIZE GLUT_WINDOW_BLUE_SIZE 
GLUT_WINDOW_ALPHA_SIZE GLUT_WINDOW_ACCUM_RED_SIZE 
GLUT_WINDOW_ACCUM_GREEN_SIZE GLUT_WINDOW_ACCUM_BLUE_SIZE 
GLUT_WINDOW_ACCUM_ALPHA_SIZE GLUT_WINDOW_DOUBLEBUFFER
GLUT_WINDOW_RGBA GLUT_WINDOW_PARENT GLUT_WINDOW_NUM_CHILDREN 
GLUT_WINDOW_COLORMAP_SIZE
;; #if (GLUT_API_VERSION >= 2)
GLUT_WINDOW_NUM_SAMPLES GLUT_WINDOW_STEREO
;; #endif
;; #if (GLUT_API_VERSION >= 3)
GLUT_WINDOW_CURSOR
;; #endif
GLUT_SCREEN_WIDTH GLUT_SCREEN_HEIGHT GLUT_SCREEN_WIDTH_MM
GLUT_SCREEN_HEIGHT_MM GLUT_MENU_NUM_ITEMS GLUT_DISPLAY_MODE_POSSIBLE
GLUT_INIT_WINDOW_X GLUT_INIT_WINDOW_Y GLUT_INIT_WINDOW_WIDTH
GLUT_INIT_WINDOW_HEIGHT GLUT_INIT_DISPLAY_MODE
;; #if (GLUT_API_VERSION >= 2)
GLUT_ELAPSED_TIME
;; #endif
;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
GLUT_WINDOW_FORMAT_ID
;; #endif

;; #if (GLUT_API_VERSION >= 2)
;;; /* glutDeviceGet parameters. */
GLUT_HAS_KEYBOARD GLUT_HAS_MOUSE GLUT_HAS_SPACEBALL 
GLUT_HAS_DIAL_AND_BUTTON_BOX
GLUT_HAS_TABLET GLUT_NUM_MOUSE_BUTTONS GLUT_NUM_SPACEBALL_BUTTONS
GLUT_NUM_BUTTON_BOX_BUTTONS GLUT_NUM_DIALS GLUT_NUM_TABLET_BUTTONS
;; #endif
;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
GLUT_DEVICE_IGNORE_KEY_REPEAT GLUT_DEVICE_KEY_REPEAT
GLUT_HAS_JOYSTICK GLUT_OWNS_JOYSTICK GLUT_JOYSTICK_BUTTONS
GLUT_JOYSTICK_AXES GLUT_JOYSTICK_POLL_RATE
;; #endif

;; #if (GLUT_API_VERSION >= 3)
;;; /* glutLayerGet parameters. */
GLUT_OVERLAY_POSSIBLE GLUT_LAYER_IN_USE GLUT_HAS_OVERLAY 
GLUT_TRANSPARENT_INDEX GLUT_NORMAL_DAMAGED GLUT_OVERLAY_DAMAGED

;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)
;;; /* glutVideoResizeGet parameters. */
GLUT_VIDEO_RESIZE_POSSIBLE GLUT_VIDEO_RESIZE_IN_USE
GLUT_VIDEO_RESIZE_X_DELTA GLUT_VIDEO_RESIZE_Y_DELTA
GLUT_VIDEO_RESIZE_WIDTH_DELTA GLUT_VIDEO_RESIZE_HEIGHT_DELTA
GLUT_VIDEO_RESIZE_X GLUT_VIDEO_RESIZE_Y
GLUT_VIDEO_RESIZE_WIDTH GLUT_VIDEO_RESIZE_HEIGHT
;; #endif

;;; /* glutUseLayer parameters. */
GLUT_NORMAL GLUT_OVERLAY

;;; /* glutGetModifiers return mask. */
GLUT_ACTIVE_SHIFT GLUT_ACTIVE_CTRL GLUT_ACTIVE_ALT

;;; /* glutSetCursor parameters. */
;;; /* Basic arrows. */
GLUT_CURSOR_RIGHT_ARROW GLUT_CURSOR_LEFT_ARROW
;;; /* Symbolic cursor shapes. */
GLUT_CURSOR_INFO GLUT_CURSOR_DESTROY GLUT_CURSOR_HELP
GLUT_CURSOR_CYCLE GLUT_CURSOR_SPRAY GLUT_CURSOR_WAIT GLUT_CURSOR_TEXT
GLUT_CURSOR_CROSSHAIR
;;; /* Directional cursors. */
GLUT_CURSOR_UP_DOWN GLUT_CURSOR_LEFT_RIGHT
;;; /* Sizing cursors. */
GLUT_CURSOR_TOP_SIDE
GLUT_CURSOR_BOTTOM_SIDE GLUT_CURSOR_LEFT_SIDE
GLUT_CURSOR_RIGHT_SIDE GLUT_CURSOR_TOP_LEFT_CORNER 
GLUT_CURSOR_TOP_RIGHT_CORNER
GLUT_CURSOR_BOTTOM_RIGHT_CORNER GLUT_CURSOR_BOTTOM_LEFT_CORNER
;;; /* Inherit from parent window. */
GLUT_CURSOR_INHERIT
;;; /* Blank cursor. */
GLUT_CURSOR_NONE
;;; /* Fullscreen crosshair (if available). */
GLUT_CURSOR_FULL_CROSSHAIR
;; #endif


;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
;;; /* GLUT device control sub-API. */
;;; /* glutSetKeyRepeat modes. */
GLUT_KEY_REPEAT_OFF
GLUT_KEY_REPEAT_ON
GLUT_KEY_REPEAT_DEFAULT

;;; /* Joystick button masks. */
GLUT_JOYSTICK_BUTTON_A GLUT_JOYSTICK_BUTTON_B
GLUT_JOYSTICK_BUTTON_C GLUT_JOYSTICK_BUTTON_D

;;; /* GLUT game mode sub-API. */
;;; /* glutGameModeGet. */
GLUT_GAME_MODE_ACTIVE GLUT_GAME_MODE_POSSIBLE
GLUT_GAME_MODE_WIDTH GLUT_GAME_MODE_HEIGHT
GLUT_GAME_MODE_PIXEL_DEPTH GLUT_GAME_MODE_REFRESH_RATE
GLUT_GAME_MODE_DISPLAY_CHANGED 

)

(define-c-enum-set glut-get-modifiers-return-mask
  ((include<> "GLUT/glut.h"))
  (shift "GLUT_ACTIVE_SHIFT")
  (ctrl  "GLUT_ACTIVE_CTRL")
  (alt   "GLUT_ACTIVE_ALT"))

(define-foreign (glut-ignore-key-repeat int) void)
(define-foreign (glut-set-key-repeat int) void)
(define-foreign (glut-force-joystick-func) void)


(define-foreign (glut-game-mode-string string) void)
(define-foreign (glut-enter-game-mode) void)
(define-foreign (glut-leave-game-mode) void)
(define-foreign (glut-game-mode-get uint) int)

(define-c-enum-set glut-display-mode
  ((include<> "GLUT/glut.h"))
  (rgba  "GLUT_RGBA") (rgb   "GLUT_RGB") (index  "GLUT_INDEX")
  (single "GLUT_SINGLE") (double "GLUT_DOUBLE")
  (accum "GLUT_ACCUM") (alpha "GLUT_ALPHA")
  (depth "GLUT_DEPTH") 
  (stencil "GLUT_STENCIL")
  (multisample "GLUT_MULTISAMPLE") 
  (stereo "GLUT_STEREO")
  (luminance "GLUT_LUMINANCE"))

;;; /* GLUT initialization sub-API. */
(define glut-init
  (let ((p (foreign-procedure "glutInit" '(int* char**) 'void)))
    (lambda args
      (call-with-int* (vector (length args))
                      (lambda (i*)
                        (call-with-char** (list->vector args)
                                          (lambda (c**) (p i* c**))))))))
(define-foreign (glut-init-window-position int int) void)
(define-foreign (glut-init-window-size int int) void)
(define-foreign (glut-init-display-mode glut-display-mode) void)
;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)
(define-foreign (glut-init-display-string string) void)
;; #endif
(define-foreign (glut-main-loop) void)

;;; /* GLUT window sub-API. */
(define-foreign (glut-create-window string) int)
;; (win x y width height) -> win
(define-foreign (glut-create-sub-window int int int int int) int)
(define-foreign (glut-set-window int) void)
(define-foreign (glut-get-window) int)
(define-foreign (glut-destroy-window int) void)
(define-foreign (glut-post-redisplay) void)
(define-foreign (glut-swap-buffers) void)
;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 11)
(define-foreign (glut-post-window-redisplay int) void)
;; #endif
(define-foreign (glut-reshape-window int int) void)
(define-foreign (glut-full-screen) void) ;; (GLUT_API_VERSION >= 3)

;; (change stacking order of current window relative to siblings)
(define-foreign (glut-pop-window) void) 
(define-foreign (glut-push-window) void)

(define-foreign (glut-show-window) void)
(define-foreign (glut-hide-window) void)
(define-foreign (glut-iconify-window) void)

(define-foreign (glut-set-window-title string) void)
(define-foreign (glut-set-icon-title string) void)

(define-syntax define-glut-cursor-enum-helper
  (transformer 
   (lambda (exp ren cmp)
     `(,(ren 'define-c-enum) ,(cadr exp)
       ((,(ren 'include) "GLUT/glut.h"))
       ,@(map (lambda (id)
                `(,id
                  ,(string-append
                    "GLUT_CURSOR_"
                    (list->string (map (lambda (c)
                                         (case c 
                                           ((#\-) #\_)
                                           (else (char-upcase c))))
                                       (string->list (symbol->string id)))))))
              (caddr exp))))))

(define-glut-cursor-enum-helper glut-cursor-name
  (right-arrow left-arrow info destroy help cycle spray wait
   text crosshair up-down left-right top-side bottom-side 
   left-side right-side top-left-corner top-right-corner
   bottom-right-corner bottom-left-corner full-crosshair none inherit))

(define-foreign (glut-set-cursor glut-cursor-name) void) ;; (GLUT_API_VERSION >= 3)

;;; /* GLUT overlay sub-API. */
(define-foreign (glut-establish-overlay) void)
(define-c-enum glut-layer-enum ((include<> "GLUT/glut.h"))
  (normal "GLUT_NORMAL") (overlay "GLUT_OVERLAY"))
(define-foreign (glut-use-layer glut-layer-enum) void)
(define-foreign (glut-remove-overlay) void)
(define-foreign (glut-post-overlay-redisplay) void)
(define-foreign (glut-show-overlay) void)
(define-foreign (glut-hide-overlay) void)
(define-foreign (glut-post-window-overlay-redisplay int) void) ;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 11)

;;; /* GLUT menu sub-API. */
(define-foreign (glut-create-menu (-> (int) void)) int)
(define-foreign (glut-get-menu) int)
(define-foreign (glut-set-menu int) void)
(define-foreign (glut-destroy-menu int) void)
(define-foreign (glut-add-menu-entry string int) void)
(define-foreign (glut-add-sub-menu string int) void)
(define-foreign (glut-change-to-menu-entry int string int) void)
(define-foreign (glut-change-to-sub-menu int string int) void)
(define-foreign (glut-remove-menu-item int) void)
(define-c-enum glut-mouse-button ((include<> "GLUT/glut.h"))
  (left "GLUT_LEFT_BUTTON") 
  (right "GLUT_RIGHT_BUTTON")
  (middle "GLUT_MIDDLE_BUTTON"))
(define-c-enum glut-button-state ((include<> "GLUT/glut.h"))
  (up "GLUT_UP") 
  (down "GLUT_DOWN"))
(define-foreign (glut-attach-menu glut-mouse-button) void)
(define-foreign (glut-detach-menu glut-mouse-button) void)

;;; /* GLUT window callback sub-API. */
(define-foreign (glut-display-func (-> () void)) void)
(define-foreign (glut-overlay-display-func (-> () void)) void) ;; #if (GLUT_API_VERSION >= 3)
(define-foreign (glut-reshape-func (-> (int int) void)) void)
(define-foreign (glut-keyboard-func (-> (uchar int int) void)) void)
(define-foreign (glut-mouse-func 
                 (-> (glut-mouse-button glut-button-state int int) void)) 
  void)
(define-foreign (glut-motion-func (-> (int int) void)) void)
(define-foreign (glut-passive-motion-func (-> (int int) void)) void)

(define-c-enum glut-visibility-state ((include<> "GLUT/glut.h"))
  (visible "GLUT_VISIBLE")
  (not-visible "GLUT_NOT_VISIBLE"))
(define-foreign (glut-visibility-func (-> (glut-visibility-state) void)) void)
(define-c-enum glut-entry-state ((include<> "GLUT/glut.h"))
  (left "GLUT_LEFT")
  (entered "GLUT_ENTERED"))
(define-foreign (glut-entry-func (-> (glut-entry-state) void)) void)

(define-foreign (glut-position-window int int) void)
;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)
(define-foreign (glut-warp-pointer int int) void)
;; #if (GLUT_MACOSX_IMPLEMENTATION >= 1)
;;; /* surface texturing API Mac OS X specific */
(define-foreign (glut-surface-texture unsigned unsigned int) void)
;; #endif
;; #if (GLUT_MACOSX_IMPLEMENTATION >= 2)
;;; /* Mac OS X specific API */
(define-foreign (glut-w-m-close-func (-> () void)) void)
(define-foreign (glut-check-loop) void)
(define (glut-main-larceny-loop)
  (do () (#f) (glut-check-loop)))
;; #endif
;; #endif

(define-foreign (glut-idle-func (maybe (-> () void))) void)
(define-foreign (glut-timer-func uint (-> (int) void) int) void)

(define-syntax define-glut-special-key-helper
  (transformer 
   (lambda (exp ren cmp)
     `(,(ren 'define-c-enum) 
       ,(list-ref exp 1) ,(list-ref exp 2)
       ,@(map (lambda (x) 
                (let ((GLUT_KEY_X
                       (string-append "GLUT_KEY_" 
                                      (list->string 
                                       (map (lambda (c)
                                              (case c ((#\-) #\_) (else c)))
                                            (string->list
                                             (string-upcase
                                              (symbol->string x))))))))
                  `(,x ,GLUT_KEY_X)))
              (list-ref exp 3))))))

(define-glut-special-key-helper glut-special-key ((include<> "GLUT/glut.h"))
  (f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 left up right down
      page-up page-down home end insert))
(define-foreign (glut-special-func (-> (glut-special-key int int) void)) 
  void) ;; #if (GLUT_API_VERSION >= 2)
(define-foreign (glut-spaceball-motion-func (-> (int int int) void)) void)
(define-foreign (glut-spaceball-rotate-func (-> (int int int) void)) void)
(define-foreign (glut-spaceball-button-func (-> (int int) void)) void)
(define-foreign (glut-button-box-func (-> (int glut-button-state) void)) void)
(define-foreign (glut-dials-func (-> (int int) void)) void)
(define-foreign (glut-tablet-motion-func (-> (int int) void)) void)
(define-foreign (glut-tablet-button-func (-> (int glut-button-state int int) void)) void)

(define-c-enum glut-menu-state ((include<> "GLUT/glut.h"))
  (in-use "GLUT_MENU_IN_USE")
  (not-in-use "GLUT_MENU_NOT_IN_USE"))
(define-foreign (glut-menu-status-func (-> (glut-menu-state int int) void)) void) ;; #if (GLUT_API_VERSION >= 3)
;; [glut-menu-state-func is deprecated]
(define-foreign (glut-menu-state-func (-> (glut-menu-state) void)) void)


(define-foreign (glut-window-status-func (-> (int) void)) void) ;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)

;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
(define-foreign (glut-keyboard-up-func (-> (uchar int int) void)) void)
(define-foreign (glut-special-up-func (-> (int int int) void)) void)
(define-foreign (glut-joystick-func (-> (uint int int int) void) int) void)
;; #endif
;; #endif
;; #endif

;;; /* GLUT color index sub-API. */
(define-foreign (glut-set-color int float float float) void)
(define-foreign (glut-get-color int int) float)
(define-foreign (glut-copy-colormap int) void)

;;; /* GLUT state retrieval sub-API. */

(define glut-get
  (let ((p (foreign-procedure "glutGet" '(uint) 'int))
        (id (lambda (x) x))
        (bool (lambda (x) (not (zero? x))))
        (cursor-name (caddr glut-cursor-name)) ; XXX documentme
        )
    ;; XXX quoting out here band-aids expansion ---
    (let-syntax ((state-dispatcher 
                  (syntax-rules ()
                    ((_ (post GLUT_NAME sym ...) ...)
                     ;; --- but quoting out here does not
                     (let () 
                       (define-glut-constants GLUT_NAME ...)
                       (lambda (state)
                         (case state
                           ((sym ...) (post (p GLUT_NAME))) ...
                           (else (error state
                                        " must be one of "
                                        (append '(sym ...) ...)
                                        )))))))))
      (state-dispatcher 
       (id GLUT_WINDOW_X window-x x)
       (id GLUT_WINDOW_Y window-y y)
       (id GLUT_WINDOW_WIDTH window-width)
       (id GLUT_WINDOW_HEIGHT window-height)
       (id GLUT_WINDOW_BUFFER_SIZE  window-buffer-size buffer-size)
       (id GLUT_WINDOW_STENCIL_SIZE window-stencil-size stencil-size)
       (id GLUT_WINDOW_DEPTH_SIZE   window-depth-size depth-size)
       (id GLUT_WINDOW_RED_SIZE     window-red-size red-size)
       (id GLUT_WINDOW_GREEN_SIZE   window-green-size green-size)
       (id GLUT_WINDOW_BLUE_SIZE    window-blue-size blue-size)
       (id GLUT_WINDOW_ALPHA_SIZE   window-alpha-size alpha-size)
       (id GLUT_WINDOW_ACCUM_RED_SIZE
           window-accum-red-size accum-red-size)
       (id GLUT_WINDOW_ACCUM_GREEN_SIZE
           window-accum-green-size accum-green-size)
       (id GLUT_WINDOW_ACCUM_BLUE_SIZE
           window-accum-blue-size accum-blue-size)
       (id GLUT_WINDOW_ACCUM_ALPHA_SIZE
           window-accum-alpha-size accum-alpha-size)
       (bool GLUT_WINDOW_DOUBLEBUFFER window-doublebuffer doublebuffer)
       (bool GLUT_WINDOW_RGBA window-rgba rgba)
       (id GLUT_WINDOW_PARENT window-parent parent)
       (id GLUT_WINDOW_NUM_CHILDREN window-num-children num-children)
       (id GLUT_WINDOW_COLORMAP_SIZE window-colormap-size colormap-size)
       (id GLUT_WINDOW_NUM_SAMPLES window-num-samples num-samples)
       (bool GLUT_WINDOW_STEREO window-stereo)
       (cursor-name GLUT_WINDOW_CURSOR window-cursor cursor)
       (id GLUT_SCREEN_WIDTH screen-width)
       (id GLUT_SCREEN_HEIGHT screen-height)
       (id GLUT_SCREEN_WIDTH_MM screen-width-mm)
       (id GLUT_SCREEN_HEIGHT_MM screen-height-mm)
       (id GLUT_MENU_NUM_ITEMS menu-num-items)
       (bool GLUT_DISPLAY_MODE_POSSIBLE display-mode-possible)
       (id GLUT_INIT_DISPLAY_MODE init-display-mode) ; XXX
       (id GLUT_INIT_WINDOW_X init-window-x)
       (id GLUT_INIT_WINDOW_Y init-window-y)
       (id GLUT_INIT_WINDOW_WIDTH init-window-width)
       (id GLUT_INIT_WINDOW_HEIGHT init-window-height)
       (id GLUT_ELAPSED_TIME elapsed-time)
       ))))

(define glut-layer-get ;; #if (GLUT_API_VERSION >= 3)
  (let ((p (foreign-procedure "glutLayerGet" '(uint) 'int)))
    (define-glut-constants 
      GLUT_OVERLAY_POSSIBLE GLUT_LAYER_IN_USE GLUT_HAS_OVERLAY
      GLUT_TRANSPARENT_INDEX GLUT_NORMAL_DAMAGED GLUT_OVERLAY_DAMAGED)
    (lambda (state)
      (case state
        ((overlay-possible) (not (zero? (p GLUT_OVERLAY_POSSIBLE))))
        ((layer-in-use) ((caddr glut-layer-enum) (p GLUT_LAYER_IN_USE)))
        ((has-overlay) (not (zero? (p GLUT_HAS_OVERLAY))))
        ((transparent-index) (let ((r (p GLUT_TRANSPARENT_INDEX)))
                               (cond ((= r -1) #f)
                                     (else r))))
        ((normal-damaged) (not (zero? (p GLUT_NORMAL_DAMAGED))))
        ((overlay-damaged) (let ((r (p GLUT_OVERLAY_DAMAGED)))
                             (cond ((= r -1) 
                                    (error 'glut-layer-get " no overlay in use"))
                                   (else (not (zero? r))))))))))

(define glut-device-get
  (let ((p (foreign-procedure "glutDeviceGet" '(uint) 'int)))
    (define-glut-constants
      GLUT_HAS_KEYBOARD GLUT_HAS_MOUSE
      GLUT_HAS_SPACEBALL GLUT_HAS_DIAL_AND_BUTTON_BOX
      GLUT_HAS_TABLET GLUT_NUM_MOUSE_BUTTONS GLUT_NUM_SPACEBALL_BUTTONS
      GLUT_NUM_BUTTON_BOX_BUTTONS GLUT_NUM_DIALS GLUT_NUM_TABLET_BUTTONS)
    (lambda (state)
      (case state
        ((has-keyboard) (not (zero? (p GLUT_HAS_KEYBOARD))))
        ((has-mouse) (not (zero? (p GLUT_HAS_MOUSE))))
        ((has-spaceball) (not (zero? (p GLUT_HAS_SPACEBALL))))
        ((has-dial-and-button-box) 
         (not (zero? (p GLUT_HAS_DIAL_AND_BUTTON_BOX))))
        ((has-tablet) (not (zero? (p GLUT_HAS_TABLET))))
        ((num-mouse-buttons) (p GLUT_NUM_MOUSE_BUTTONS))
        ((num-spaceball-buttons) (p GLUT_NUM_SPACEBALL_BUTTONS))
        ((num-button-box-buttons) (p GLUT_NUM_BUTTON_BOX_BUTTONS))
        ((num-dials) (p GLUT_NUM_DIALS))
        ((num-tablet-buttons) (p GLUT_NUM_TABLET_BUTTONS))))))

(define-foreign (glut-get-modifiers) glut-get-modifiers-return-mask) ;; #if (GLUT_API_VERSION >= 3)


;;; /* GLUT extension support sub-API */
(define-foreign (glut-extension-supported string) bool) ;; #if (GLUT_API_VERSION >= 2)

(define glut-font*-rtd (ffi-install-void*-subtype 'glut-font*))
(define glut-bitmap-font*-rtd
  (ffi-install-void*-subtype 'glut-bitmap-font* glut-font*-rtd))

(define glut-bitmap-fonts ;; unabstract (relies on header details).  :(
  (let* ((ctor (record-constructor glut-bitmap-font*-rtd))
         (make (lambda (name) 
                 (list name (ctor ((foreign-variable name 'ulong)))))))
    (list (make "glutBitmap9By15")
          (make "glutBitmap8By13")
          (make "glutBitmapTimesRoman10")
          (make "glutBitmapTimesRoman24")
          (make "glutBitmapHelvetica10")
          (make "glutBitmapHelvetica12")
          (make "glutBitmapHelvetica18"))))

(define-foreign (glut-bitmap-character glut-bitmap-font* char) void)
(define-foreign (glut-bitmap-width glut-bitmap-font* char) int)

(define glut-stroke-font*-rtd
  (ffi-install-void*-subtype 'glut-stroke-font* glut-font*-rtd))
(define glut-stroke-fonts ;; definitely unabstract
  ;; XXX (needs to be different for WIN32!!!) XXX
  (let* ((ctor (record-constructor glut-stroke-font*-rtd))
         (make (lambda (name)
                 (list name (ctor ((foreign-variable name 'ulong)))))))
    (list (make "glutStrokeRoman")
          (make "glutStrokeMonoRoman"))))

(define-foreign (glut-stroke-character glut-stroke-font* char) void)
(define-foreign (glut-stroke-width glut-stroke-font* char) int)

(define (glut-font-dispatch font-name char stroke-handler bitmap-handler)
  (define (stroke nm) (stroke-handler
                       (cadr (assoc nm glut-stroke-fonts)) char))
  (define (bitmap nm) (bitmap-handler
                       (cadr (assoc nm glut-bitmap-fonts)) char))
  (case font-name
    ((stroke-roman)      (stroke "glutStrokeRoman"))
    ((stroke-mono-roman) (stroke "glutStrokeMonoRoman"))
    ((bitmap-9-by-15)        (bitmap "glutBitmap9By15"))
    ((bitmap-8-by-13)        (bitmap "glutBitmap8By13"))
    ((bitmap-times-roman-10) (bitmap "glutBitmapTimesRoman10"))
    ((bitmap-times-roman-24) (bitmap "glutBitmapTimesRoman24"))
    ((bitmap-helvetica-12)   (bitmap "glutBitmapHelvetica12"))
    ((bitmap-helvetica-18)   (bitmap "glutBitmapHelvetica18"))))
(define (glut-font-character font-name char)
  (glut-font-dispatch font-name char 
                      glut-stroke-character glut-bitmap-character))
(define (glut-font-width font-name char)
  (glut-font-dispatch font-name char 
                      glut-stroke-width glut-bitmap-width))

;;; /* GLUT pre-built models sub-API */
(define-foreign (glut-solid-sphere double int int) void)
(define-foreign (glut-wire-sphere double int int) void)
(define-foreign (glut-solid-cube double) void)
(define-foreign (glut-wire-cube double) void)
(define-foreign (glut-solid-cone double double int int) void)
(define-foreign (glut-wire-cone double double int int) void)
(define-foreign (glut-solid-torus double double int int) void)
(define-foreign (glut-wire-torus double double int int) void)
(define-foreign (glut-solid-dodecahedron) void)
(define-foreign (glut-wire-dodecahedron) void)
(define-foreign (glut-solid-octahedron) void)
(define-foreign (glut-wire-octahedron) void)
(define-foreign (glut-solid-tetrahedron) void)
(define-foreign (glut-wire-tetrahedron) void)
(define-foreign (glut-solid-icosahedron) void)
(define-foreign (glut-wire-icosahedron) void)
(define-foreign (glut-solid-teapot double) void)
(define-foreign (glut-wire-teapot double) void)


;;;XXX are items below here documented in Glut-3 spec?
;;; (i guess not seeing as how they all require version >= 4

;; #if (GLUT_API_VERSION >= 5)
(define-foreign (glut-get-proc-address string) void)
;; #endif


;; #if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)
;;; /* GLUT video resize sub-API. */
(define-foreign (glut-video-resize-get uint) int)
(define-foreign (glut-setup-video-resizing) void)
(define-foreign (glut-stop-video-resizing) void)
(define-foreign (glut-video-resize int int int int) void)
(define-foreign (glut-video-pan int int int int) void)

;;; /* GLUT debugging sub-API. */
(define-foreign (glut-report-errors) void)
;; #endif
