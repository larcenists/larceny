(clr/%load-assembly "System.Windows.Forms" "2.0.0.0" "" "b77a5c561934e089")

;; :Basic:make-trap.sch is Mac specific
;; :Basic:mac-type is Mac specific 
;; :Basic:general is FFI stuff that we can do ourselves
;; :Basic:blt is also very lowlevel FFI stuff 
;; :Basic:event event handling protocols; want to do this ourselves
;; :Basic:tasks interrupt & event based multi-tasker
;; :Standard:data&traps seems mostly mac specific, but it might be
;;    good to know what state it defines...
;; :Standard:events standard event handlers -- worth looking over.
;; :Standard:objects routines used for Toolsmith's OOP; 
;;    I should take inspiration; but the code itself is useless
;; :Standard:menus routines to manipulate menus; 
;;    read over this carefully
;; :Standard:windows routines to manipulate windows and their agents
;; :Standard:editors (suprising little code!)
;; :Standard:scrollers maintains scrollbars and text in editor window

;; Toolsmith documentation describes the following operations and procedures:
;; [[ operations are denoted by two parens, procedures by one ]]
;; ((object 'operations))
;; (make-menu title)
;; ((menu 'close))
;; ((menu 'append) item action)
;; ((menu 'append) item action enableproc)
;; ((menu 'addresources) rtype raction)
;; ((menu 'update))
;; ((menu 'selectitem) itemnumber)
;; (pushmenubar)
;; (popmenubar)
;; ((menu 'id))
;; ((menu 'menuhandle))
;; (make-window) (make-window 'make-agent make-window-agent 'text 'bounds left top right bottom 'title string nogoaway 'nosizebox)
;; ((window 'close))
;; ((window 'closed?))
;; ((window 'agent))
;; ((window 'windowptr))
;; ((window 'width))
;; ((window 'height))
;; ((window 'update))
;; ((window 'activate))
;; ((window 'deactivate))
;; ((window 'keydown) char)
;; ((window 'autokey) char)
;; ((window 'mousedown) part where event)
;; ((window 'select))
;; (lookup-window-object windowptr)
;; ((agent 'close))
;; ((agent 'update) window-object)
;; ((agent 'activate) window-object)
;; ((agent 'deactivate) window-object)
;; ((agent 'mousedown) window-object where modifiers)
;; ((agent 'keydown) char)
;; ((agent 'autokey) char)
;; ((agent 'grow) rectangle)
;; (make-editor windowptr width height)
;; ((editor 'scroller))
;; ((editor 'texthandle))
;; ((editor 'cut))
;; ((editor 'copy))
;; ((editor 'paste))
;; ((editor 'insert) string)
;; ((editor 'textsize))
;; ((editor 'textstring))
;; ((editor 'set-textstring) string)
;; ((editor 'selectionstring))
;; ((editor 'selstart))
;; ((editor 'selend))
;; ((editor 'set-selection) start end)
;; ((editor 'line) n)
;; ((editor 'column) n)
;; ((scroller 'close))
;; ((scroller 'vertical))
;; ((scroller 'horizontal))
;; ((scroller 'update) window)
;; ((scroller 'activate))
;; ((scroller 'deactivate))
;; ((scroller 'line) window editor control direction)
;; ((scroller 'page) window editor control direction)
;; ((scroller 'show) editor n)
;; ((scroller 'set) editor)

(define (find-forms-type name)
  (find-clr-type (string-append "System.Windows.Forms." name)))
(define (find-drawing-type name)
  (find-clr-type (string-append "System.Drawing." name)))

(define (box foreign-or-scheme-object)
  (let ((x foreign-or-scheme-object))
    (cond ((%foreign? x) x)
          ((and (number? x) (exact? x))   (clr/%number->foreign-int32 x))
          ((and (number? x) (inexact? x)) (clr/%flonum->foreign-double x))
          ((string? x)  (clr/%string->foreign x))
          (else (error 'box ": unknown argument type to convert " x)))))
(define (unbox foreign)
  (let ((x foreign))
    (cond ((or (clr/%isa? x clr-type-handle/system-int32)
               (clr/%isa? x clr-type-handle/system-uint32)
               (clr/%isa? x clr-type-handle/system-int64)
               (clr/%isa? x clr-type-handle/system-uint64))
           (clr/%foreign->int x))
          ((or (clr/%isa? x clr-type-handle/system-string))
           (clr/%foreign->string x))
          (else 
           x))))

(define type->nullary-constructor 
  (lambda (type)
    (let ((ctor (clr/%get-constructor type '#())))
      (lambda ()
        (clr/%invoke-constructor ctor '#())))))
(define (type*args&convert->constructor type . argtypes*converters)
  (let* ((argtypes (map car argtypes*converters))
         (converters (map cadr argtypes*converters))
         (tvec (list->vector argtypes))
         (ctor (clr/%get-constructor type tvec)))
    (lambda actuals
      (if (not (= (vector-length tvec) (length actuals)))
          (error 'constructor (format #t ": ~a requires argument types ~a" 
                                      type tvec))
          (clr/%invoke-constructor
           ctor (list->vector (map (lambda (f x) (f x)) 
                                   converters actuals)))))))
(define make-property-setter 
  (lambda (type property-name-string . maybe-convert)
    (let* ((convert (if (null? maybe-convert) box (car maybe-convert)))
           (prop (clr/%get-property type property-name-string '#())))
      (lambda (obj new-val)
        (clr/%property-set! prop obj (convert new-val) '#())))))
        
(define int32-arg&convert
  (list clr-type-handle/system-int32 clr/%number->foreign-int32))

(define make-static-method
  (lambda (type method-name-string)
    (let ((method (clr/%get-method type method-name-string '#())))
      (lambda ()
        (clr/%invoke method #f '#())))))
(define make-unary-method
  (lambda (type method-name-string)
    (let ((method (clr/%get-method type method-name-string '#())))
      (lambda (obj)
        (unbox (clr/%invoke method obj '#()))))))
(define make-binary-method
  (lambda (type method-name-string arg-type)
    (let ((method (clr/%get-method type method-name-string (vector arg-type))))
      (lambda (obj arg)
        (unbox (clr/%invoke method obj (vector (box arg))))))))
(define make-property-ref
  (lambda (type property-name-string . maybe-convert)
    (let ((convert (if (null? maybe-convert) unbox (car maybe-convert)))
          (prop (clr/%get-property type property-name-string '#())))
      (lambda (obj)
        (convert (clr/%property-ref prop obj '#()))))))

(define type->predicate
  (lambda (type)
    (lambda (obj)
      (clr/%isa? obj type))))
(define type->name
  (make-property-ref type-type "Name" 
                     (lambda (x) (string->symbol (clr/foreign->string x)))))
(define enum-type->symbol-convert
  (lambda (enum-type)
    (let* ((names (clr-enum/get-names enum-type))
           (vals  (clr-enum/get-values enum-type))
           ;; handles both 'Right and 'right for an enum named "Right"
           (lower-syms  (map string->symbol (map string-downcase names)))
           (cased-syms  (map string->symbol names))
           (lookup-table (append (map list cased-syms vals)
                                 (map list lower-syms vals)))
           (lookup (lambda (s) 
                     (let ((entry (assq s lookup-table)))
                       (if entry (cadr entry)
                           (error 'convert "" (type->name enum-type)
                                  "unknown name" s 
                                  "for possible enums " names))))))
      (if (memq (string->symbol "System.FlagsAttribute")
                (clr-type/get-custom-attributes enum-type))
          ;; If flags enum, then accept arbitrary # of args.
          (lambda args
            (clr-enum/to-object 
             enum-type
             (foldr fxlogior 0 (map lookup args))))
          (lambda (arg) ;; (strict subrelation of above)
            (clr-enum/to-object enum-type (lookup arg)))))))
  
;;; System.Windows.Forms.Control class, properties, and methods

(define control-type             (find-forms-type "Control"))
(define control-anchor    (make-property-ref control-type "Anchor"))
(define control-controls  (make-property-ref control-type "Controls"))
(define control-text      (make-property-ref control-type "Text"))
(define set-control-text! (make-property-setter control-type "Text"))
(define control-top       (make-property-ref control-type "Top"))
(define set-control-top!      (make-property-setter control-type "Top"))
(define control-left          (make-property-ref control-type "Left"))
(define set-control-left!     (make-property-setter control-type "Left"))
(define control-width         (make-property-ref control-type "Width"))
(define set-control-width!    (make-property-setter control-type "Width"))
(define control-height        (make-property-ref control-type "Height"))
(define set-control-height!   (make-property-setter control-type "Height"))
(define control-location      (make-property-ref control-type "Location"))
(define set-control-location! (make-property-setter control-type "Location"))
(define control-parent        (make-property-ref control-type "Parent"))
(define control-size          (make-property-ref control-type "Size"))
(define set-control-size!     (make-property-setter control-type "Size"))
(define control-client-size      (make-property-ref control-type "ClientSize"))
(define set-control-client-size! (make-property-setter control-type "ClientSize"))
(define set-control-anchor! 
  (let* ((anchor-styles-type (find-forms-type "AnchorStyles"))
         (convert (enum-type->symbol-convert anchor-styles-type))
         (setter (make-property-setter control-type "Anchor"
                                       (lambda (argl) (apply convert argl)))))
    (lambda (control . args)
      (setter control args))))
(define set-control-dock!
  (let* ((dock-style-type (find-forms-type "DockStyle"))
         (convert (enum-type->symbol-convert dock-style-type))
         (setter (make-property-setter control-type "Dock"
                                       (lambda (argl) (apply convert argl)))))
    (lambda (control . args)
      (setter control args))))
(define control-set-bounds! 
  (let ((method (clr/%get-method control-type "SetBounds" 
                                 (vector clr-type-handle/system-int32 
                                         clr-type-handle/system-int32 
                                         clr-type-handle/system-int32 
                                         clr-type-handle/system-int32))))
    (lambda (control x y width height)
      (clr/%invoke method control
                   (clr/%number->foreign x)
                   (clr/%number->foreign y)
                   (clr/%number->foreign width)
                   (clr/%number->foreign height)))))

(define controls-collection-type (find-forms-type "Control+ControlCollection"))
(define add-controls 
  (let ((add! (make-binary-method controls-collection-type "Add" control-type)))
    (lambda (collection controls)
      (for-each (lambda (c) (add! collection c)) controls))))

(define form-type                (find-forms-type "Form"))
(define make-form (type->nullary-constructor form-type))

(define panel-type               (find-forms-type "Panel"))
(define make-panel (type->nullary-constructor panel-type))

(define form1                    (make-form))
(define form1-controls (control-controls form1))

;;; System.Drawing.Font and related classes
(define font-style-type (find-drawing-type "FontStyle"))
(define font-family-type (find-drawing-type "FontFamily"))
(define font-type (find-drawing-type "Font"))
(define system-fonts-type (find-drawing-type "SystemFonts"))
(define font-family-name (make-property-ref font-family-type "Name"))
(define font-families
  (let* ((installed-font-collection-type 
          (find-drawing-type "Text.InstalledFontCollection"))
         (prop-ref (make-property-ref installed-font-collection-type
                                      "Families"))
         (ctor (type->nullary-constructor installed-font-collection-type)))
    (lambda ()
      (clr-array->list (prop-ref (ctor))))))
(define generic-font-family
  (let* ((make-ref (lambda (x) (make-property-ref font-family-type x)))
         (monospace-prop-ref  (make-ref "GenericMonospace"))
         (sans-serif-prop-ref (make-ref "GenericSansSerif"))
         (serif-prop-ref      (make-ref "GenericSerif")))
    (lambda (x)
      (case x
        ((monospace)  (monospace-prop-ref clr/null))
        ((sans-serif) (sans-serif-prop-ref clr/null))
        ((serif)      (serif-prop-ref clr/null))
        (else (error 'generic-font-family ": unknown generic family " x))))))
(define system-font
  (let* ((get-prop 
          (lambda (x)
            (make-property-ref system-fonts-type (string-append x "Font"))))
         (caption       (get-prop "Caption"))
         (default       (get-prop "Default"))
         (dialog        (get-prop "Dialog"))
         (icon-title    (get-prop "IconTitle"))
         (menu          (get-prop "Menu"))
         (message-box   (get-prop "MessageBox"))
         (small-caption (get-prop "SmallCaption"))
         (status        (get-prop "Status")))
    (lambda (x)
      ((case x
         ((caption)       caption)
         ((default)       default)
         ((dialog)        dialog)
         ((icon-title)    icon-title)
         ((menu)          menu)
         ((message-box)   message-box)
         ((small-caption) small-caption)
         ((status)        status)) clr/null))))
(define make-font 
  (let* ((get-enum
          (lambda (name)
            (clr/%field-ref (clr/%get-field font-style-type name) #f)))
         (regular   (get-enum "Regular"))
         (bold      (get-enum "Bold"))
         (italic    (get-enum "Italic"))
         (underline (get-enum "Underline"))
         (strikeout (get-enum "Strikeout"))
         (convert (lambda (sym)
                    (case sym
                      ((regular)   regular)
                      ((bold)      bold)
                      ((italic)    italic)
                      ((underline) underline)
                      ((strikeout) strikeout)
                      (else (error 'make-font ": unknown style " sym)))))
         (make-font-family (type*args&convert->constructor
                            font-family-type
                            (list clr-type-handle/system-string clr/string->foreign)))
         (font-family?     (type->predicate font-family-type)))
    (type*args&convert->constructor 
     font-type
     (list font-family-type 
           (lambda (x)
             (cond ((string? x) (make-font-family x))
                   ((font-family? x) x)
                   (else (error 'make-font ": invalid family argument " x)))))
     (list clr-type-handle/system-single (lambda (n) (clr/%flonum->foreign-single
                                                      (exact->inexact n))))
     (list font-style-type convert))))

(define combo-box-type (find-forms-type "ComboBox"))
(define make-combo-box (type->nullary-constructor combo-box-type))
(define combo-box-items (make-property-ref combo-box-type "Items"))
(define combo-box-add-item! 
  (let* ((combo-box+object-collection (find-forms-type "ComboBox+ObjectCollection"))
         (add-item! 
          (make-binary-method combo-box+object-collection "Add" clr-type-handle/system-object)))
    (lambda (combo-box item)
      (let* ((items (combo-box-items combo-box))
             (retval (add-item! items item)))
        (clr/foreign->int retval)))))
         
(define (add-event-handler publisher event-name procedure) 
  (clr/%add-event-handler publisher event-name (clr/%foreign-box procedure)))

;; System.Windows.Forms.Application.DoEvents() processes event queue
(define application-do-events!
  (let* ((application-type (find-forms-type "Application")))
    (make-static-method application-type "DoEvents")))

(define toolsmith-interrupt-handler
  (lambda ()
    '(begin (display "timer interrupt was fired!") 
            (newline))
    (application-do-events!)
    (enable-interrupts (standard-timeslice))))

(timer-interrupt-handler toolsmith-interrupt-handler)

(standard-timeslice 1000)
(enable-interrupts (standard-timeslice))

(define show           (make-unary-method form-type "Show"))
(define show-dialog    (make-unary-method form-type "ShowDialog"))
(define close          (make-unary-method form-type "Close"))

(define menu-type      (find-forms-type "Menu"))
(define main-menu-type (find-forms-type "MainMenu"))
(define menu-item-type (find-forms-type "MenuItem"))
(define make-main-menu (type->nullary-constructor main-menu-type))
(define make-menu-item (type->nullary-constructor menu-item-type))
(define menu-add-menu-item!
  (let* ((main+menu-item-collection-type 
          (find-forms-type "Menu+MenuItemCollection"))
         (add-menu-item!
          (make-binary-method main+menu-item-collection-type
                              "Add" menu-item-type))
         (prop-ref (make-property-ref menu-type "MenuItems")))
    (lambda (menu menu-item)
      (let* ((menu-items (prop-ref menu))
             (retval (add-menu-item! menu-items menu-item)))
        (clr/foreign->int retval)))))
(define (add-child-menu-item! parent-menu name)
  (let ((mi (make-menu-item)))
    (menu-item-set-text! mi name)
    (menu-add-menu-item! parent-menu mi)
    mi))

(define text-box-type  (find-forms-type "TextBox"))
(define make-text-box  (type->nullary-constructor text-box-type))

(define point-type     (find-drawing-type "Point"))
(define make-point     (type*args&convert->constructor 
                        point-type int32-arg&convert int32-arg&convert))
(define point-x (make-property-ref point-type "X"))
(define point-y (make-property-ref point-type "Y"))
(define set-point-x! (make-property-setter point-type "X"))
(define set-point-y! (make-property-setter point-type "Y"))

(define size-type        (find-drawing-type "Size"))
(define make-size        (type*args&convert->constructor
                          size-type int32-arg&convert int32-arg&convert))
(define size-width       (make-property-ref    size-type "Width"))
(define set-size-width!  (make-property-setter size-type "Width"))
(define size-height      (make-property-ref    size-type "Height"))
(define set-size-height! (make-property-setter size-type "Height"))

(define form-set-menu! (make-property-setter form-type "Menu"))

(define menu-item-set-text! (make-property-setter menu-item-type "Text"))

;; (Generic and slow)
(define (set-text! obj new-text)
  (let* ((obj-type (clr/%object-type obj))
         (setter! (make-property-setter obj-type "Text")))
    (setter! obj new-text)))

(define split-container-type (find-forms-type "SplitContainer"))
(define orientation-type (find-forms-type "Orientation"))
(define make-split-container (type->nullary-constructor split-container-type))
(define set-split-container-orientation!
  (let* ((convert (enum-type->symbol-convert orientation-type))
         (prop-set! (make-property-setter split-container-type "Orientation"
                                          convert)))
    (lambda (sc o)
      (prop-set! sc o))))
(define split-container-panel1
  (make-property-ref split-container-type "Panel1"))
(define split-container-panel2 
  (make-property-ref split-container-type "Panel2"))

(define text-box1 (make-text-box))
(set-text! text-box1 "Welcome!")
(set-control-top! text-box1 20)

;; below temporary code snagged from event-handling-demo1.sch

(define button-type              (find-forms-type "Button"))
(define make-button      (type->nullary-constructor button-type))
(define button1                  (make-button))
(define label-type               (find-forms-type "Label"))
(define make-label       (type->nullary-constructor label-type))
(define message-box-type         (find-forms-type "MessageBox"))
(define label1                   (make-label))
(set-control-text! button1 "Click Me")
(set-control-top!  button1 50)
(set-control-text!  label1 "no click received")

(define (button1-click sender args)
  (set-control-text! label1 "Click Received Successfully!"))

(define panel1 (make-panel))

(define panel1-controls (prop-ref/name panel1 "Controls"))

;;; very strange; putting label1 *and* text-box1 on the same form
;;; leads to text-box1 not being rendered.
;;(add-controls form1-controls (list label1 button1))
(add-controls panel1-controls (list label1 button1 text-box1))
(add-controls form1-controls (list panel1))
;;(add-controls form1-controls (list label1 text-box1))
;;(add-controls form1-controls (list button1 text-box1)) 
(add-event-handler button1 "Click" button1-click)

(define fonts-combo-box (make-combo-box))
(for-each (lambda (fam)
            (combo-box-add-item! fonts-combo-box
                                 (clr/string->foreign (font-family-name fam))))
          (font-families))
;(add-controls form1-controls fonts-combo-box)
(add-controls panel1-controls (list fonts-combo-box))
(set-control-top! fonts-combo-box 70)

(define form2 (make-form))
(define main-menu2 (make-main-menu))
(define file-menu (add-child-menu-item! main-menu2 "File"))
(define edit-menu (add-child-menu-item! main-menu2 "Edit"))

(define file..open    (add-child-menu-item! file-menu "Open"))
(define file..save    (add-child-menu-item! file-menu "Save"))
(define file..save-as (add-child-menu-item! file-menu "Save As..."))
(define edit..cut     (add-child-menu-item! edit-menu "Cut"))
(define edit..copy    (add-child-menu-item! edit-menu "Copy"))
(define edit..paste   (add-child-menu-item! edit-menu "Paste"))

(form-set-menu! form2 main-menu2)

(define text-box2.1 (make-text-box))
(define text-box2.2 (make-text-box))
;(add-controls (control-controls form2) (list text-box2.1 text-box2.2))
(define split-container1 (make-split-container))
(add-controls (control-controls form2) (list split-container1))
(set-split-container-orientation! split-container1 'horizontal)
(add-controls (control-controls (split-container-panel1 split-container1))
              (list text-box2.1))
(add-controls (control-controls (split-container-panel2 split-container1))
              (list text-box2.2))

(define set-text-box-multiline!
  (make-property-setter text-box-type "Multiline" clr/bool->foreign))
(set-text-box-multiline! text-box2.1 #t)
(set-text-box-multiline! text-box2.2 #t)

(define (make-control-fill-available-space! control)
  (set-control-anchor! control 'top 'bottom 'right 'left)
  (set-control-size! control (control-client-size (control-parent control))))
(make-control-fill-available-space! split-container1)
(make-control-fill-available-space! text-box2.1)
(make-control-fill-available-space! text-box2.2)
'(begin
  (set-control-anchor! text-box2.1 'top 'right 'left)
  (set-control-size! text-box2.1 
                     (let ((sz (control-client-size form2)))
                       (make-size (size-width sz)
                                  (quotient (size-height sz) 2)))))
'(set-control-dock! text-box2.2 'bottom)
