;; Microsoft .NET implementation of world teachpack abstraction layer

;; dependencies:
;; srfi-9, dotnet-defs, image-dotnet, world

(require 'srfi-9)
(load "image-dotnet.sch") ;; loads dotnet-defs implicitly
(load "world.sch")

;; -----------------------------------------------------------------------------
;; Helper Functions

;; get-key-code : System.Windows.Forms.KeyEventArgs -> int
(define (get-key-code ka)
  (clr/foreign->int (clr/%property-ref clr/key-event-args-key-code 
                                       ka 
                                       (vector))))

;; get-shift : System.Windows.Forms.KeyEventArgs -> boolean
(define (get-shift ka)
  (clr/foreign->bool (clr/%property-ref clr/key-event-args-shift ka (vector))))

;; set-control-width : System.Windows.Forms.Control int -> void
(define (set-control-width! c w)
  (clr/%property-set! clr/control-width c (clr/int->foreign w) (vector)))

;; set-control-height : System.Windows.Forms.Control int -> void
(define (set-control-height! c h)
  (clr/%property-set! clr/control-height c (clr/int->foreign h) (vector)))

;; get-control-width : System.Windows.Forms.Control -> int
(define (get-control-width c)
  (clr/foreign->int (clr/%property-ref clr/control-width c (vector))))

;; get-control-height : System.Windows.Forms.Control -> int
(define (get-control-height c)
  (clr/foreign->int (clr/%property-ref clr/control-height c (vector))))

;; get-control-visible : System.Windows.Forms.Control -> int
(define (get-control-visible c)
  (clr/foreign->bool (clr/%property-ref clr/control-visible c (vector))))

;; set-control-back-color! : System.Windows.Forms.Control Color -> void
(define (set-control-back-color! c color)
  (clr/%property-set! clr/control-back-color 
                      c 
                      (make-dotnet-color color) 
                      (vector)))

;; -----------------------------------
;; Implementation

;; This bitmap holds a copy of the image to be drawn on the form
(define *the-bitmap* #f)

;; timer-rep? : Any -> boolean
(define (impl-timer-rep? rep) (clr/%isa? rep clr/timer-type))

;; timer-start : timer -> t
(define (impl-timer-start t) 
  (clr/%invoke clr/timer-start (timer-rep t) (vector)))

;; timer-stop : timer ->
(define (impl-timer-stop t)
  (clr/%invoke clr/timer-stop (timer-rep t) (vector)))

;; set-timer-interval! : timer int -> 
(define (impl-set-timer-interval! t i)
  (clr/%property-set! clr/timer-interval 
                      (timer-rep t) 
                      (clr/int->foreign i) 
                      (vector)))

;; frame-rep? : Any -> boolean
(define (impl-frame-rep? rep) (clr/%isa? rep clr/form-type))

;; new-frame : -> frame
(define (impl-new-frame w h)
  (let* ((f (clr/%invoke-constructor clr/form-ctor (vector)))
         (t (clr/%invoke-constructor clr/timer-ctor (vector)))
         (obj-array (allocate-clr-array clr/object-type 2)))

    ;; buffer bitmap
    (set! *the-bitmap* (make-dotnet-bitmap w h))
    
    ;; setting the width and height of the frame
    ;; this is a hack, currently, and should be changed to dynamically
    ;; pad the window depending on the display settings (i.e. Windows theme)
    (set-control-width! f (+ w 48))
    (set-control-height! f (+ h 74))
    (set-control-back-color! f 'white)
    
    ;; configure form to be double-buffered
    (clr/%foreign-aset obj-array 0 (clr/int->foreign clr/double-buffer-bitmask))
    (clr/%foreign-aset obj-array 1 (clr/bool->foreign #t))
    (clr/%invoke clr/method-info-invoke 
                 clr/control-set-style
                 (vector f obj-array))
    
    ;; focus when form is made visible
    (clr/add-event-handler! f "VisibleChanged"
                            (lambda (sender args)
                              (if (get-control-visible f)
                                  (clr/%invoke clr/control-focus f (vector)))))
    
    ;; stop timer on close
    (clr/add-event-handler! f "FormClosing"
                             (lambda (sender event-args) 
                               (clr/%invoke clr/timer-stop t (vector))))
    
    ;; handle key events
    (clr/add-event-handler! f "KeyDown"
                             (lambda (sender ka)
                               (let* ((kc (get-key-code ka))
                                      (sh (get-shift ka))
                                      (key-symbol (parse-key-code kc sh)))
                                 (if (not (eq? on-char-proc unspecified))
                                     (on-char-proc key-symbol)))))
    
    ;; paint the cached-bitmap during the Paint event
    (clr/add-event-handler! f "Paint"
                             (lambda (sender paint-args)
                               (let ((the-gfx (clr/%property-ref
                                               clr/paint-event-args-graphics 
                                               paint-args
                                               (vector)))
                                     (w (dotnet-image-width *the-bitmap*))
                                     (h (dotnet-image-height *the-bitmap*)))
                                 (clr/%invoke clr/gfx-draw-image-with-size
                                              the-gfx
                                              (vector *the-bitmap*
                                                      ;; see above note re: hack
                                                      (clr/int->foreign 20)
                                                      (clr/int->foreign 20)
                                                      (clr/int->foreign w)
                                                      (clr/int->foreign h))))))
    
    ;; call the timer-callback method on each tick
    (clr/add-event-handler! t "Tick" (lambda (sender args) (timer-callback)))
    
    (make-frame f (make-timer t))))

;; update-frame : frame image -> 
(define (impl-update-frame f i)
  (set! *the-bitmap* (bitmap-rep (:image-bitmap i)))
  (clr/%invoke clr/form-refresh (frame-rep f) (vector)))

;; show-frame : frame ->
(define (impl-show-frame f) 
  (let ()
    (clr/%invoke clr/form-show-dialog (frame-rep f) (vector))    
    (unspecified)))

;; Key Code Parser -------------------------------------------------------------

;; *** the following is Microsoft .NET and windows keyboard specific
;; *** missing the following MrEd symbols
;;       'numpad-enter 
;;       'start 
;;       'wheel-up 
;;       'wheel-down 
;;       'release 
;;       'press 

;; This mapping was done manually. A more thorough investigation into an 
;; generating the table automatically is warranted.

;; parse-key-code : int bool -> (symbol or character)
;; translates the pressing of a keyboard button to a
;; MrEd compatible key-code
(define (parse-key-code keycode shift)
  (case keycode
    (3 'cancel)
    (8 #\backspace)
    (9 #\tab)
    (12 'clear)
    (13 #\return)
    (16 'shift)
    (17 'control)
    (18 'menu)
    (19 'pause)
    (20 'capital)
    (27 'escape)
    (32 #\space)
    (33 'prior)
    (34 'next)
    (35 'end)
    (36 'home)
    (37 'left)
    (38 'up)
    (39 'right)
    (40 'down)
    (41 'select)
    (42 'print)
    (43 'execute)
    (44 'snapshot)
    (45 'insert)
    (46 #\delete) ;; this is #\rubout in MrEd
    (47 'help)
    (48 (if shift #\) #\0))
    (49 (if shift #\! #\1))
    (50 (if shift #\@ #\2))
    (51 (if shift #\# #\3))
    (52 (if shift #\$ #\4))
    (53 (if shift #\% #\5))
    (54 (if shift #\^ #\6))
    (55 (if shift #\& #\7))
    (56 (if shift #\* #\8))
    (57 (if shift #\( #\9))
    (65 (if shift #\A #\a))
    (66 (if shift #\B #\b))
    (67 (if shift #\C #\c))
    (68 (if shift #\D #\d))
    (69 (if shift #\E #\e))
    (70 (if shift #\F #\f))
    (71 (if shift #\G #\g))
    (72 (if shift #\H #\h))
    (73 (if shift #\I #\i))
    (74 (if shift #\J #\j))
    (75 (if shift #\K #\k))
    (76 (if shift #\L #\l))
    (77 (if shift #\M #\m))
    (78 (if shift #\N #\n))
    (79 (if shift #\O #\o))
    (80 (if shift #\P #\p))
    (81 (if shift #\Q #\q))
    (82 (if shift #\R #\r))
    (83 (if shift #\S #\s))
    (84 (if shift #\T #\t))
    (85 (if shift #\U #\u))
    (86 (if shift #\V #\v))
    (87 (if shift #\W #\w))
    (88 (if shift #\X #\x))
    (89 (if shift #\Y #\y))
    (90 (if shift #\Z #\z))
    (96 'numpad0)
    (97 'numpad1)
    (98 'numpad2)
    (99 'numpad3)
    (100 'numpad)
    (101 'numpad5)
    (102 'numpad6)
    (103 'numpad7)
    (104 'numpad8)
    (105 'numpad9)
    (106 'multiply)
    (107 'add)
    (108 'separator)
    (109 'subtract)
    (110 'decimal)
    (111 'divide)
    (112 'f1)
    (113 'f2)
    (114 'f3)
    (115 'f4)
    (116 'f5)
    (117 'f6)
    (118 'f7)
    (119 'f8)
    (120 'f9)
    (121 'f10)
    (122 'f11)
    (123 'f12)
    (124 'f13)
    (125 'f14)
    (126 'f15)
    (127 'f16)
    (128 'f17)
    (129 'f18)
    (130 'f19)
    (131 'f20)
    (132 'f21)
    (133 'f22)
    (134 'f23)
    (135 'f24)
    (144 'numlock)
    (145 'scroll)
    (186 (if shift #\: #\;))
    (187 (if shift #\+ #\=))
    (188 (if shift #\< #\,))
    (189 (if shift #\_ #\-))
    (190 (if shift #\> #\.))
    (191 (if shift #\? #\/))
    (192 (if shift #\~ #\`))
    (219 (if shift #\{ #\[))
    (220 (if shift #\| #\\))
    (221 (if shift #\} #\]))
    (else #\nul)))

;; the following are additional System.Windows.Forms.Keys enumeration values
;; that are not used currently
;    (0 'None)
;    (1 'LButton)
;    (2 'RButton)
;    (4 'MButton)
;    (5 'XButton1)
;    (6 'XButton2)
;    (10 'LineFeed)
;    (21 'KanaMode)
;    (21 'KanaMode)
;    (21 'KanaMode)
;    (23 'JunjaMode)
;    (24 'FinalMode)
;    (25 'HanjaMode)
;    (25 'HanjaMode)
;    (28 'IMEConvert)
;    (29 'IMENonconvert)
;    (30 'IMEAceept)
;    (30 'IMEAceept)
;    (31 'IMEModeChange)
;    (91 'LWin)
;    (92 'RWin)
;    (93 'Apps)
;    (95 'Sleep)
;    (160 'LShiftKey)
;    (161 'RShiftKey)
;    (162 'LControlKey)
;    (163 'RControlKey)
;    (164 'LMenu)
;    (165 'RMenu)
;    (166 'BrowserBack)
;    (167 'BrowserForward)
;    (168 'BrowserRefresh)
;    (169 'BrowserStop)
;    (170 'BrowserSearch)
;    (171 'BrowserFavorites)
;    (172 'BrowserHome)
;    (173 'VolumeMute)
;    (174 'VolumeDown)
;    (175 'VolumeUp)
;    (176 'MediaNextTrack)
;    (177 'MediaPreviousTrack)
;    (178 'MediaStop)
;    (179 'MediaPlayPause)
;    (180 'LaunchMail)
;    (181 'SelectMedia)
;    (182 'LaunchApplication1)
;    (183 'LaunchApplication2)
;    (229 'ProcessKey)
;    (231 'Packet)
;    (246 'Attn)
;    (247 'Crsel)
;    (248 'Exsel)
;    (249 'EraseEof)
;    (250 'Play)
;    (251 'Zoom)
;    (252 'NoName)
;    (253 'Pa1)
;    (254 'OemClear)
;    (222 'Oem7)
;    (223 'Oem8)
;    (226 'OemBackslash)
;    (226 'OemBackslash)
;    (65535 'KeyCode)
;    (65536 'shift)
;    (131072 'control)
;    (262144 'Alt)
;    (-65536 'Modifiers)
