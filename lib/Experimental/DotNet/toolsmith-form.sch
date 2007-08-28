(clr/%load-assembly "System.Windows.Forms" "2.0.0.0" "" "b77a5c561934e089")

(define (find-forms-type name)
  (find-clr-type (string-append "System.Windows.Forms." name)))
(define (find-drawing-type name)
  (find-clr-type (string-append "System.Drawing." name)))

(define (box foreign-or-scheme-object)
  (let ((x foreign-or-scheme-object))
    (cond ((%foreign? x) x)
          ((and (number? x) (exact? x))   (clr/%number->foreign-int32 x))
          ((and (number? x) (inexact? x)) (clr/%flonum->foreign-double x))
          ((boolean? x) (clr/bool->foreign x))
          ((string? x)  (clr/%string->foreign x))
          (else (error 'box ": unknown argument type to convert " x)))))
(define (unbox foreign)
  (let ((x foreign))
    (cond ((or (clr/%isa? x clr-type-handle/system-int32)
               (clr/%isa? x clr-type-handle/system-uint32)
               (clr/%isa? x clr-type-handle/system-int64)
               (clr/%isa? x clr-type-handle/system-uint64))
           (clr/%foreign->int x))
          ((or (clr/%isa? x clr-type-handle/system-single))
           (clr/%foreign-single->flonum x))
          ((or (clr/%isa? x clr-type-handle/system-double))
           (clr/%foreign-double->flonum x))
          ((or (clr/%isa? x clr-type-handle/system-string))
           (clr/%foreign->string x))
          ((or (clr/%isa? x clr-type-handle/system-boolean))
           (clr/foreign->bool x))
          (else 
           x))))

(define (name->string x)
  (cond
   ((symbol? x) (symbol->string x))
   ((string? x) x)
   (else (error 'name->string ": " x " is not a name."))))
(define (name->symbol x)
  (cond 
   ((symbol? x) x)
   ((string? x) (string->symbol x))
   (else (error 'name->symbol ": " x " is not a name."))))

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
(define single-arg&convert
  (list clr-type-handle/system-single clr/%flonum->foreign-single))
(define string-arg&convert
  (list clr-type-handle/system-string clr/string->foreign))

(define make-static-method
  (lambda (type method-name-string . arg-types)
    (let ((method (clr/%get-method type method-name-string (list->vector arg-types))))
      (lambda argl
        (cond ((not (= (length argl) (length arg-types)))
               (error (string->symbol method-name-string) 
                      ": argument count mismatch.")))
        (clr/%invoke method #f (list->vector argl))))))
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
  (make-property-ref (find-clr-type "System.Type") "Name" 
                     (lambda (x) (string->symbol (clr/foreign->string x)))))
  
;;; System.Windows.Forms.Control class, properties, and methods

(define control-type             (find-forms-type "Control"))
(define make-control      (type->nullary-constructor control-type))
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
(define control-preferred-size      
  (make-property-ref control-type "PreferredSize"))
(define set-control-preferred-size! 
  (make-property-setter control-type "PreferredSize"))
(define control-visible       (make-property-ref control-type "Visible"))
(define set-control-visible!  (make-property-setter control-type "Visible"))
(define control-font          (make-property-ref control-type "Font"))
(define set-control-font!     (make-property-setter control-type "Font"))
(define set-control-anchor! 
  (let* ((anchor-styles-type (find-forms-type "AnchorStyles"))
         (convert (enum-type->symbol->foreign anchor-styles-type))
         (setter (make-property-setter control-type "Anchor"
                                       (lambda (argl) (apply convert argl)))))
    (lambda (control . args)
      (setter control args))))
(define set-control-dock!
  (let* ((dock-style-type (find-forms-type "DockStyle"))
         (convert (enum-type->symbol->foreign dock-style-type))
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
(define control-dispose! (make-unary-method control-type "Dispose"))
(define control-focus!   (make-unary-method control-type "Focus"))
(define control-select!  (make-unary-method control-type "Select"))

(define controls-collection-type (find-forms-type "Control+ControlCollection"))
(define add-controls 
  (let ((add! (make-binary-method controls-collection-type "Add" control-type)))
    (lambda (collection controls)
      (for-each (lambda (c) (add! collection c)) controls))))

(define scrollable-control-type (find-forms-type "ScrollableControl"))
(define scrollable-control-autoscroll 
  (make-property-ref scrollable-control-type "AutoScroll" clr/foreign->bool))
(define set-scrollable-control-autoscroll!
  (make-property-setter scrollable-control-type "AutoScroll" clr/bool->foreign))

(define form-type                (find-forms-type "Form"))
(define make-form (type->nullary-constructor form-type))
(define form?     (type->predicate form-type))
(define form-close! (make-unary-method form-type "Close"))
(define set-form-keypreview! (make-property-setter form-type "KeyPreview"))

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

;;; System.Drawing.Bitmap and related classes
(define bitmap-type (find-drawing-type "Bitmap"))
(define make-bitmap (type*args&convert->constructor bitmap-type string-arg&convert))
(define clipboard-set-data-object 
  (make-static-method (find-forms-type "Clipboard") "SetDataObject" 
                      clr-type-handle/system-object))
(define combo-box-type (find-forms-type "ComboBox"))
(define combo-box-style-type (find-forms-type "ComboBoxStyle"))
(define make-combo-box (type->nullary-constructor combo-box-type))
(define combo-box-items (make-property-ref combo-box-type "Items"))
(define combo-box-drop-down-style
  (make-property-ref combo-box-type "DropDownStyle"
                     (enum-type->foreign->symbol combo-box-style-type)))
(define set-combo-box-drop-down-style! 
  (make-property-setter combo-box-type "DropDownStyle"
                        (enum-type->symbol->foreign combo-box-style-type)))
(define combo-box-add-item! 
  (let* ((combo-box+object-collection (find-forms-type "ComboBox+ObjectCollection"))
         (add-item! 
          (make-binary-method combo-box+object-collection "Add" clr-type-handle/system-object)))
    (lambda (combo-box item)
      (let* ((items (combo-box-items combo-box))
             (retval (add-item! items (clr/string->foreign item))))
        (clr/foreign->int retval)))))
         
(define (add-event-handler publisher event-name procedure) 
  (clr/%add-event-handler publisher event-name (clr/%foreign-box procedure)))

;; System.Windows.Forms.Application.DoEvents() processes event queue
(define application-do-events!
  (let* ((application-type (find-forms-type "Application")))
    (make-static-method application-type "DoEvents")))

(define application-message-loop?
  (let ((p (make-property-ref (find-forms-type "Application") "MessageLoop")))
    (lambda () 
      ;; (its a static property)
      (p clr/null))))

(define toolsmith-interrupt-handler
  (lambda ()
    '(begin (display "timer interrupt was fired!") 
            (newline))
    (application-do-events!)
    (enable-interrupts (standard-timeslice))))

(timer-interrupt-handler toolsmith-interrupt-handler)

(standard-timeslice 1000)
(enable-interrupts (standard-timeslice))

(define show             (make-unary-method form-type "Show"))
(define hide             (make-unary-method form-type "Hide"))
(define close            (make-unary-method form-type "Close"))

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

;; System.Windows.Forms.FileDialog and related classes
(define common-dialog-type    (find-forms-type "CommonDialog"))
(define file-dialog-type      (find-forms-type "FileDialog"))
(define open-file-dialog-type (find-forms-type "OpenFileDialog"))
(define save-file-dialog-type (find-forms-type "SaveFileDialog"))
(define dialog-result-type    (find-forms-type "DialogResult"))
(define make-open-file-dialog (type->nullary-constructor open-file-dialog-type))
(define make-save-file-dialog (type->nullary-constructor save-file-dialog-type))
(define dialog-show-dialog    (make-unary-method common-dialog-type "ShowDialog"))
(define form-show-dialog      (make-unary-method form-type "ShowDialog"))
(define common-dialog?        (type->predicate common-dialog-type))
(define file-dialog-filename  (make-property-ref file-dialog-type "FileName"))
(define show-dialog 
  (let ((convert-ret (enum-type->foreign->symbol dialog-result-type)))
    (lambda (x)
      (let ((result (cond ((common-dialog? x)
                           (dialog-show-dialog x))
                          ((form? x)
                           (form-show-dialog x))
                          (else
                           (error 'show-dialog ": unknown object" x)))))
        (convert-ret result)))))

;; System.Windows.Forms.TextBox and related classes
(define text-box-base-type  (find-forms-type "TextBoxBase"))
(define text-box-type       (find-forms-type "TextBox"))
(define make-text-box       (type->nullary-constructor text-box-type))
(define rich-text-box-type  (find-forms-type "RichTextBox"))
(define make-rich-text-box  (type->nullary-constructor rich-text-box-type))
(define auto-complete-mode-type   (find-forms-type "AutoCompleteMode"))
(define auto-complete-source-type (find-forms-type "AutoCompleteSource"))
(define text-box-auto-complete-source  
  (make-property-ref text-box-type "AutoCompleteSource" 
                     (enum-type->foreign->symbol auto-complete-source-type)))
(define set-text-box-auto-complete-source!
  (make-property-setter text-box-type "AutoCompleteSource" 
                        (enum-type->symbol->foreign auto-complete-source-type)))
(define text-box-auto-complete-mode
  (make-property-ref text-box-type "AutoCompleteMode"
                     (enum-type->foreign->symbol auto-complete-mode-type)))
(define set-text-box-auto-complete-mode!
  (make-property-setter text-box-type "AutoCompleteMode"
                        (enum-type->symbol->foreign auto-complete-mode-type)))
(define text-box-auto-complete-custom-source
  (make-property-ref text-box-type "AutoCompleteCustomSource"))


(define pointi-type     (find-drawing-type "Point"))
(define make-pointi     (type*args&convert->constructor 
                        pointi-type int32-arg&convert int32-arg&convert))
(define pointi-x (make-property-ref pointi-type "X"))
(define pointi-y (make-property-ref pointi-type "Y"))
(define set-pointi-x! (make-property-setter pointi-type "X"))
(define set-pointi-y! (make-property-setter pointi-type "Y"))
(define pointf-type     (find-drawing-type "PointF"))
(define make-pointf     (type*args&convert->constructor 
                        pointf-type single-arg&convert single-arg&convert))
(define pointf-x (make-property-ref pointf-type "X"))
(define pointf-y (make-property-ref pointf-type "Y"))
(define set-pointf-x! (make-property-setter pointf-type "X"))
(define set-pointf-y! (make-property-setter pointf-type "Y"))

(define color-type     (find-drawing-type "Color"))
(define color-alpha    (make-property-ref color-type "A"))
(define color-red      (make-property-ref color-type "R"))
(define color-green    (make-property-ref color-type "G"))
(define color-blue     (make-property-ref color-type "B"))
(define color-name     (make-property-ref color-type "Name"))

(define size-type        (find-drawing-type "Size"))
(define make-size        (type*args&convert->constructor
                          size-type int32-arg&convert int32-arg&convert))
(define size-width       (make-property-ref    size-type "Width"))
(define set-size-width!  (make-property-setter size-type "Width"))
(define size-height      (make-property-ref    size-type "Height"))
(define set-size-height! (make-property-setter size-type "Height"))
(define sizef-type        (find-drawing-type "SizeF"))
(define make-sizef        (type*args&convert->constructor
                           sizef-type single-arg&convert single-arg&convert))
(define sizef-width       (make-property-ref    sizef-type "Width"))
(define set-sizef-width!  (make-property-setter sizef-type "Width"))
(define sizef-height      (make-property-ref    sizef-type "Height"))
(define set-sizef-height! (make-property-setter sizef-type "Height"))

(define form-set-menu! (make-property-setter form-type "Menu"))

(define menu-item-set-text! (make-property-setter menu-item-type "Text"))
(define menu-item-text (make-property-ref menu-item-type "Text"))

;; (Generic and slow)
(define (set-text! obj new-text)
  (let* ((obj-type (clr/%object-type obj))
         (setter! (make-property-setter obj-type "Text")))
    (setter! obj new-text)))

(define split-container-type (find-forms-type "SplitContainer"))
(define orientation-type (find-forms-type "Orientation"))
(define make-split-container (type->nullary-constructor split-container-type))
(define set-split-container-orientation!
  (let* ((convert (enum-type->symbol->foreign orientation-type))
         (prop-set! (make-property-setter split-container-type "Orientation"
                                          convert)))
    (lambda (sc o)
      (prop-set! sc o))))
(define split-container-panel1
  (make-property-ref split-container-type "Panel1"))
(define split-container-panel2 
  (make-property-ref split-container-type "Panel2"))
(define split-container-panel1-collapsed (make-property-ref split-container-type "Panel1Collapsed"))
(define split-container-panel2-collapsed (make-property-ref split-container-type "Panel2Collapsed"))
(define set-split-container-panel1-collapsed!
  (make-property-setter split-container-type "Panel1Collapsed" clr/bool->foreign))
(define set-split-container-panel2-collapsed!
  (make-property-setter split-container-type "Panel2Collapsed" clr/bool->foreign))

(define flow-layout-panel-type (find-forms-type "FlowLayoutPanel"))
(define flow-direction-type    (find-forms-type "FlowDirection"))
(define make-flow-layout-panel (type->nullary-constructor flow-layout-panel-type))
(define flow-layout-panel-flow-direction 
  (make-property-ref flow-layout-panel-type "FlowDirection" 
                     (enum-type->foreign->symbol flow-direction-type)))
(define set-flow-layout-panel-flow-direction!
  (make-property-setter flow-layout-panel-type "FlowDirection"
                        (enum-type->symbol->foreign flow-direction-type)))
(define flow-layout-panel-wrap-contents 
  (make-property-ref flow-layout-panel-type "WrapContents" clr/foreign->bool))
(define set-flow-layout-panel-wrap-contents!
  (make-property-setter flow-layout-panel-type "WrapContents" clr/bool->foreign))

(define button-type              (find-forms-type "Button"))
(define make-button      (type->nullary-constructor button-type))
(define label-type               (find-forms-type "Label"))
(define make-label       (type->nullary-constructor label-type))
(define message-box-type         (find-forms-type "MessageBox"))
                                                 
(define set-text-box-multiline!
  (make-property-setter text-box-base-type "Multiline" clr/bool->foreign))

(define (add-display-event-handler publisher event-name)
  (add-event-handler publisher event-name
                     (lambda (sender args)
                       (display `(handling ,event-name ,sender ,args))
                       (newline))))

;;; Control events: (map event-info->name (type->events control-type))
; (AutoSizeChanged BackColorChanged BackgroundImageChanged BackgroundImageLayoutChanged
;  BindingContextChanged CausesValidationChanged ChangeUICues Click ClientSizeChanged 
;  ContextMenuChanged ContextMenuStripChanged ControlAdded ControlRemoved CursorChanged
;  DockChanged DoubleClick DragDrop DragEnter DragLeave DragOver EnabledChanged Enter
;  FontChanged ForeColorChanged GiveFeedback GotFocus HandleCreated HandleDestroyed
;  HelpRequested ImeModeChanged Invalidated KeyDown KeyPress KeyUp Layout Leave
;  LocationChanged LostFocus MarginChanged MouseCaptureChanged MouseClick MouseDoubleClick
;  MouseDown MouseEnter MouseHover MouseLeave MouseMove MouseUp MouseWheel Move PaddingChanged
;  Paint ParentChanged PreviewKeyDown QueryAccessibilityHelp QueryContinueDrag RegionChanged
;  Resize RightToLeftChanged SizeChanged StyleChanged SystemColorsChanged TabIndexChanged
;  TabStopChanged TextChanged Validated Validating VisibleChanged Disposed)


(define key-event-args-type (find-forms-type "KeyEventArgs"))
(define key-event-args-alt (make-property-ref key-event-args-type "Alt"))
(define key-event-args-control (make-property-ref key-event-args-type "Control"))
(define key-event-args-shift (make-property-ref key-event-args-type "Shift"))
(define key-event-args-keycode (make-property-ref key-event-args-type "KeyCode"))
(define key-event-args-keydata (make-property-ref key-event-args-type "KeyData"))
(define key-event-args-keyvalue (make-property-ref key-event-args-type "KeyValue"))
(define key-press-event-args-type (find-forms-type "KeyPressEventArgs"))
(define key-press-event-args-keychar (make-property-ref key-press-event-args-type "KeyChar"))

(define mouse-event-args-type (find-forms-type "MouseEventArgs"))
(define mouse-event-args-button (make-property-ref mouse-event-args-type "Button"))
(define mouse-event-args-clicks (make-property-ref mouse-event-args-type "Clicks"))
(define mouse-event-args-delta (make-property-ref mouse-event-args-type "Delta"))
(define mouse-event-args-x (make-property-ref mouse-event-args-type "X"))
(define mouse-event-args-y (make-property-ref mouse-event-args-type "Y"))
(define paint-event-args-type (find-forms-type "PaintEventArgs"))
(define paint-event-args-cliprectangle (make-property-ref paint-event-args-type "ClipRectangle"))
(define paint-event-args-graphics (make-property-ref paint-event-args-type "Graphics"))
(define rectangle-type (find-drawing-type "Rectangle"))
(define rectangle-height (make-property-ref rectangle-type "Height"))
(define rectangle-width (make-property-ref rectangle-type "Width"))
(define rectangle-x (make-property-ref rectangle-type "X"))
(define rectangle-y (make-property-ref rectangle-type "Y"))

(define graphics-type (find-drawing-type "Graphics"))
(define graphics-dispose! 
  (let ((dispose-method (clr/%get-method graphics-type "Dispose" '#())))
    (lambda (g)
      (clr/%invoke dispose-method g '#()))))
(define pen-type      (find-drawing-type "Pen"))
(define make-pen 
  (let ((pen-ctor (clr/%get-constructor pen-type (vector color-type))))
    (lambda (color)
      (clr/%invoke-constructor pen-ctor (vector color)))))
(define pen-dispose! 
  (let ((dispose-method (clr/%get-method pen-type "Dispose" '#())))
    (lambda (pen)
      (clr/%invoke dispose-method pen '#()))))
(define brush-type    (find-drawing-type "Brush"))
(define solid-brush-type    (find-drawing-type "SolidBrush"))
(define make-solid-brush 
  (let ((brush-ctor (clr/%get-constructor solid-brush-type (vector color-type))))
    (lambda (color)
      (clr/%invoke-constructor brush-ctor (vector color)))))
(define brush-dispose! 
  (let ((dispose-method (clr/%get-method brush-type "Dispose" '#())))
    (lambda (b)
      (clr/%invoke dispose-method b '#()))))

(define image-type    (find-drawing-type "Image"))

(define (available-fontnames)
  (map font-family-name (font-families)))
(define (monospace-fontname)
  (font-family-name (generic-font-family 'monospace)))
(define (sans-serif-fontname)
  (font-family-name (generic-font-family 'sans-serif)))
(define (serif-fontname)
  (font-family-name (generic-font-family 'serif)))

(define make-fnt 
  (let* ((clone-method (clr/%get-method font-type "Clone" '#()))
         (make-bool-pset 
          (lambda (pname)  
            (make-property-setter font-type pname clr/bool->foreign)))
         (get-name     (make-property-ref font-type "Name" '#()))
         (get-size     (make-property-ref font-type "Size" '#()))
         (get-italic   (make-property-ref font-type "Italic" '#()))
         (get-bold     (make-property-ref font-type "Bold" '#()))
         (get-uline    (make-property-ref font-type "Underline" '#()))
         (set-italic   (make-bool-pset "Italic"))
         (set-bold     (make-bool-pset "Bold"))
        (set-uline    (make-bool-pset "Underline"))
        (set-emsize   (make-property-setter font-type "Size" 
                                            clr/%number->foreign-int32)))
    (define (construct fontptr)
      (msg-handler
       ((clone . args) ;; should extend to allow turning OFF bold et al???
        (let* ((is-italic (memq 'italic args))
               (is-bold   (memq 'bold args))
               (is-uline  (memq 'underline args))
               (em-size   (cond ((memq 'em-size args) => cadr)
                                (else #f)))
               (newptr (clr/%invoke fontptr clone-method '#())))
          (cond (is-italic (set-italic newptr #t)))
          (cond (is-bold   (set-bold   newptr #t)))
          (cond (is-uline  (set-uline  newptr #t)))
          (cond (em-size   (set-emsize newptr em-size)))
          (construct newptr)))
       ((name)
        (name->symbol (get-name fontptr)))
       ((em-size)
        (get-size fontptr))
       ((italic?)    (get-italic fontptr))
       ((bold?)      (get-bold fontptr))
       ((underline?) (get-underline fontptr))
       ((fntptr)     fontptr)))
    (lambda (name em-size)
      (construct (make-font (name->string name) em-size 'regular)))))
(define (color->col colorptr)
  (msg-handler 
   ((name)     (name->symbol (color-name colorptr)))
   ((alpha)    (color-alpha colorptr))
   ((red)      (color-red   colorptr))
   ((green)    (color-green colorptr))
   ((blue)     (color-green colorptr))
   ((colptr)   colorptr)))
(define make-col
  (let ((from-argb-method
         (clr/%get-method color-type "FromArgb"
                          (vector clr-type-handle/system-int32
                                  clr-type-handle/system-int32
                                  clr-type-handle/system-int32
                                  clr-type-handle/system-int32))))
    (lambda (a r g b)
      (color->col
       (clr/%invoke from-argb-method 
                    #f (vector (clr/%number->foreign-int32 a)
                               (clr/%number->foreign-int32 r)
                               (clr/%number->foreign-int32 g)
                               (clr/%number->foreign-int32 b)))))))
(define name->col
  (let ((from-name-method
         (clr/%get-method color-type "FromName"
                          (vector clr-type-handle/system-string))))
    (lambda (name)
      (color->col
       (clr/%invoke from-name-method
                    #f (vector (clr/%string->foreign (name->string name))))))))
      
(define available-colornames
  (let ((color-sym (string->symbol "Color")))
    (lambda () 
      (map property-info->name 
           (filter (lambda (x) (eq? color-sym (type->name (property-info->type x)))) 
                   (type->properties color-type))))))
(define control-creategraphics 
  (let ((method (clr/%get-method control-type "CreateGraphics" '#())))
    (lambda (c)
      (clr/%invoke method c '#()))))
(define graphics->gfx 
  (let* ((text-renderer-type (find-forms-type "TextRenderer"))
         (measure-text/text-renderer
          (let ((measure-text-method (clr/%get-method
                                      text-renderer-type
                                      "MeasureText"
                                      (vector (find-drawing-type "IDeviceContext")
                                              clr-type-handle/system-string
                                              font-type))))
            (lambda (g string fnt)
              ;; XXX a hack to workaround the "helpful" trimming behavior
              ;; (is there a flag like MeasureTrailingSpaces for TextRenderer?)
              (let* ((string* (clr/%string->foreign (string-append string "a")))
                     (stringa (clr/%string->foreign "a"))
                     (fntptr ((fnt 'fntptr)))
                     (sza (clr/%invoke measure-text-method #f 
                                       (vector g stringa fntptr)))
                     (szf (clr/%invoke measure-text-method #f
                                       (vector g string* fntptr))))
                (values (- (size-width szf) (size-width sza)) (size-height szf))))))
         (draw-text/text-renderer
          (let ((draw-text-method (clr/%get-method 
                                   text-renderer-type 
                                   "DrawText"
                                   (vector (find-drawing-type "IDeviceContext")
                                           clr-type-handle/system-string
                                           font-type
                                           pointi-type
                                           color-type))))
            (lambda (g string fnt x y col)
              (clr/%invoke draw-text-method #f
                           (vector g
                                   (clr/%string->foreign string)
                                   ((fnt 'fntptr))
                                   (make-pointi x y)
                                   ((col 'colptr)))))))

         (string-format-type (find-drawing-type "StringFormat"))
         (string-format-flags-type (find-drawing-type "StringFormatFlags"))
         (make-string-format
          (let ((ctor (clr/%get-constructor string-format-type 
                                            (vector string-format-type))))
            (lambda (x)
              (clr/%invoke-constructor ctor (vector x)))))
         (format-flags->foreign (enum-type->symbol->foreign string-format-flags-type))
         (foreign->format-flags (enum-type->foreign->symbol string-format-flags-type))
         (generic-typographic-format-prop
          (clr/%get-property string-format-type "GenericTypographic" '#()))
         (generic-typographic-format 
          (clr/%property-ref generic-typographic-format-prop clr/null '#()))
         (string-format (let* ((format-flags-prop 
                                (clr/%get-property 
                                 string-format-type "FormatFlags" '#()))
                               (fmt (make-string-format 
                                     generic-typographic-format))
                               (flags 
                                (call-with-values 
                                    (lambda ()
                                      (foreign->format-flags
                                       (clr/%property-ref 
                                        format-flags-prop fmt '#())))
                                  list)))
                          (clr/%property-set! 
                           format-flags-prop fmt 
                           (apply format-flags->foreign
                                  'measuretrailingspaces flags) '#())
                          fmt))
         (measure-text/graphics
          (let ((measure-text-method (clr/%get-method
                                      graphics-type
                                      "MeasureString"
                                      (vector clr-type-handle/system-string
                                              font-type
                                              clr-type-handle/system-int32
                                              string-format-type))))
            
            (lambda (g string fnt)
              ;; XXX when Mono releases a fix for their MeasureTrailingSpaces bug
              ;; take this hack of appending "a" out.
              (let* ((string* (clr/%string->foreign (string-append string "a")))
                     (stringa (clr/%string->foreign "a"))
                     (fntptr ((fnt 'fntptr)))
                     (maxint30 (clr/%number->foreign-int32 (most-positive-fixnum)))
                     (sza (clr/%invoke measure-text-method 
                                       g (vector stringa fntptr maxint30 string-format)))
                     (szf (clr/%invoke measure-text-method 
                                       g (vector string* fntptr maxint30 string-format))))
                (values (- (sizef-width szf) (sizef-width sza)) (sizef-height szf))))))
         (draw-text/graphics 
          (let ((draw-string-method (clr/%get-method
                                     graphics-type
                                     "DrawString"
                                     (vector clr-type-handle/system-string
                                             font-type
                                             brush-type
                                             clr-type-handle/system-single
                                             clr-type-handle/system-single
                                             string-format-type))))
            (lambda (g string fnt x y col)
              (let* ((string* (clr/%string->foreign string))
                     (b (make-solid-brush ((col 'colptr))))
                     (fntptr ((fnt 'fntptr)))
                     (x* (clr/%flonum->foreign-single (exact->inexact x)))
                     (y* (clr/%flonum->foreign-single (exact->inexact y))))
                (clr/%invoke draw-string-method g
                             (vector string* fntptr b x* y* string-format))
                ))))

         (draw-line-method/inexact (clr/%get-method
                                    graphics-type
                                    "DrawLine"
                                    (vector pen-type 
                                            clr-type-handle/system-single
                                            clr-type-handle/system-single
                                            clr-type-handle/system-single
                                            clr-type-handle/system-single)))
         (draw-line-method/exact   (clr/%get-method
                                    graphics-type
                                    "DrawLine"
                                    (vector pen-type
                                            clr-type-handle/system-int32
                                            clr-type-handle/system-int32
                                            clr-type-handle/system-int32
                                            clr-type-handle/system-int32)))
         (draw-rect-method/exact   (clr/%get-method
                                    graphics-type
                                    "DrawRectangle"
                                    (vector pen-type
                                            clr-type-handle/system-int32
                                            clr-type-handle/system-int32
                                            clr-type-handle/system-int32
                                            clr-type-handle/system-int32)))
         (draw-rect-method/inexact (clr/%get-method
                                    graphics-type
                                    "DrawRectangle"
                                    (vector pen-type
                                            clr-type-handle/system-single
                                            clr-type-handle/system-single
                                            clr-type-handle/system-single
                                            clr-type-handle/system-single)))
         (draw-image-method (clr/%get-method 
                             graphics-type
                             "DrawImage"
                             (vector image-type
                                     clr-type-handle/system-int32
                                     clr-type-handle/system-int32)))
         ;;(measure-text measure-text/text-renderer)
         ;;(draw-text draw-text/text-renderer)
         (measure-text measure-text/graphics)
         (draw-text draw-text/graphics)
         )
    (lambda (g)
      (msg-handler 
       ((measure-text txt-string fnt)      (measure-text g txt-string fnt))
       ((draw-text txt-string fnt x y col) (draw-text g txt-string fnt x y col))
       ((draw-line col x1 y1 x2 y2) 
        (let ((pen (make-pen ((col 'colptr)))))
          (cond ((and (fixnum? x1) (fixnum? y1) (fixnum? x2) (fixnum? y2))
                 (clr/%invoke draw-line-method/exact g
                              (vector pen
                                      (clr/%number->foreign-int32 x1)
                                      (clr/%number->foreign-int32 y1)
                                      (clr/%number->foreign-int32 x2)
                                      (clr/%number->foreign-int32 y2))))
                (else
                 (clr/%invoke draw-line-method/inexact g
                              (vector pen 
                                      (clr/%flonum->foreign-single (exact->inexact x1))
                                      (clr/%flonum->foreign-single (exact->inexact y1))
                                      (clr/%flonum->foreign-single (exact->inexact x2))
                                      (clr/%flonum->foreign-single (exact->inexact y2))))))
          (pen-dispose! pen)))
       ((draw-rect col x1 y1 x2 y2) 
        (let ((pen (make-pen ((col 'colptr))))
              (x (min x1 x2))
              (y (min y1 y2))
              (w (abs (- x2 x1)))
              (h (abs (- y2 y1))))
          (clr/%invoke draw-rect-method/exact g
                       (vector pen
                               (clr/%number->foreign-int32 x)
                               (clr/%number->foreign-int32 y)
                               (clr/%number->foreign-int32 w)
                               (clr/%number->foreign-int32 h)))
          (pen-dispose! pen)))
                               
       ((draw-image img x y) 
        (clr/%invoke draw-image-method g
                     (vector (img 'imgptr) 
                             (clr/%number->foreign-int32 x) 
                             (clr/%number->foreign-int32 y))))
       ((gfxptr) g)
       ))))

;; This code to support double-buffering depends on funtionality
;; defined in simple-reflection; I can probably predicate it
;; accordingly in a relatively straight forward manner.

(begin
  (define set-double-buffered-meth 
    (let* ((binding-flags-type (find-reflection-type "BindingFlags"))
           (get-method/private-meth
            (clr/%get-method type-type "GetMethod" 
                             (vector clr-type-handle/system-string binding-flags-type)))
           (non-public-instance-flags
           ((enum-type->symbol->foreign binding-flags-type)
            'nonpublic 'instance)))
      (clr/%invoke get-method/private-meth
                   control-type 
                   (vector (clr/string->foreign "set_DoubleBuffered") 
                           non-public-instance-flags))))
  
  (define (create-type name supertype constructor-extension . 
		       method-extensions)
    (let* ((type-builder (define-type name supertype))
	   (ctor (define-constructor type-builder))
           (ilgen (constructor->ilgen ctor))
           (emit! (ilgen->emitter ilgen)))
      (emit! 'ldarg.0)
      (emit! 'call (clr/%get-constructor (type->superclass type-builder) 
					 '#()))
      (constructor-extension emit!)
      (emit! 'ret)
      (for-each (lambda (method-pieces) 
		  (call-with-values method-pieces
		    (lambda (name arg-types result-type extension)
		      (let* ((m (define-method type-builder
				  name arg-types result-type))
			     (ilgen (method->ilgen m))
			     (emit! (ilgen->emitter ilgen)))
			(extension emit!)))))
		method-extensions)
      (let* ((create-type-meth (clr/%get-method
                                typebuilder-type "CreateType" '#()))
             (type (clr/%invoke create-type-meth type-builder '#()))
             (make-object (type->nullary-constructor type)))
        make-object)))

  (define make-double-buffered-form 
    (create-type "DoubleBufferedForm" form-type
		 (lambda (emit!)
		   (emit! 'ldarg.0)
		   (emit! 'ldc.i4.1)
		   (emit! 'call set-double-buffered-meth))))

  (define make-double-buffered-control
    (let ((write-line-meth
	   (clr/%get-method (find-clr-type "System.Console") "WriteLine" 
			    (vector clr-type-handle/system-string))))
      (create-type
       "DoubleBufferedControl" control-type
       (lambda (emit!)
	 (emit! 'ldarg.0)
	 (emit! 'ldc.i4.1)
	 (emit! 'call set-double-buffered-meth))

       (lambda ()
	 ;; EVERYTHING'S an input key!!!
	 ;; ha ha ha!  So crazy it just might work!
	 (values "IsInputKey" (list (find-forms-type "Keys"))
		 clr-type-handle/system-boolean
		 (lambda (emit!)
		   ;;(emit! 'ldstr "Hi there!")
		   ;;(emit! 'call write-line-meth)
		   (emit! 'ldc.i4.1)
		   (emit! 'ret))))
       )))
  )

(define keys-type (find-forms-type "Keys"))
(define keys-foreign->symbols (enum-type->foreign->symbol keys-type))

(define scrollbar-type  (find-forms-type "ScrollBar"))
(define hscrollbar-type (find-forms-type "HScrollBar"))
(define vscrollbar-type (find-forms-type "VScrollBar"))
(define make-hscrollbar (type->nullary-constructor hscrollbar-type))
(define make-vscrollbar (type->nullary-constructor vscrollbar-type))
(define scrollbar-value (make-property-ref scrollbar-type "Value"))
(define set-scrollbar-value! (make-property-setter scrollbar-type "Value"))
(define scrollbar-maximum (make-property-ref scrollbar-type "Maximum"))
(define scrollbar-minimum (make-property-ref scrollbar-type "Minimum"))
(define set-scrollbar-maximum! (make-property-setter scrollbar-type "Maximum"))
(define set-scrollbar-minimum! (make-property-setter scrollbar-type "Minimum"))
(define scrollbar-largechange (make-property-ref scrollbar-type "LargeChange"))
(define scrollbar-smallchange (make-property-ref scrollbar-type "SmallChange"))
(define set-scrollbar-largechange! 
  (make-property-setter scrollbar-type "LargeChange"))
(define set-scrollbar-smallchange!
  (make-property-setter scrollbar-type "SmallChange"))
(define scrolleventargs-type (find-forms-type "ScrollEventArgs"))
(define scrolleventtype-type (find-forms-type "ScrollEventType"))
(define scrolleventargs-newvalue
  (make-property-ref scrolleventargs-type "NewValue"))
;; OldValue property doesn't seem to work in Mono.  :(
;;(define scrolleventargs-oldvalue
;;  (make-property-ref scrolleventargs-type "OldValue" (lambda (x) x)))
(define scrolleventargs-gettype 
  (make-property-ref scrolleventargs-type "Type" 
                     (enum-type->foreign->symbol scrolleventtype-type)))
(define evt #f)
(define (make-mnu name)
  (define (string->menu-item s)
    (let ((mi (make-menu-item)))
      (menu-item-set-text! mi (name->string s))
      mi))
  (let ((mi (string->menu-item name)))
    (msg-handler
     ((name) (name->symbol (menu-item-text mi)))
     ((mnuptr) mi)
     ((append item action) 
      (let ((mi* (cond ((string? item) (string->menu-item item))
                       (else ((item 'mnuptr))))))
        (menu-add-menu-item! mi mi*)
        (add-event-handler mi* "Click"
                           (lambda (sender args) (action))))))))
     
(define (make-wnd . args)
  (let* ((agent-ctor
          (cond ((memq 'make-agent args) => cadr)
                (else default-agent-ctor))) ;; none yet!  yikes!
         (bounds
          (cond ((memq 'bounds args) => cadr)
                (else (list 0 0 100 100))))
         (title
          (cond ((memq 'title args) => cadr)
                (else #f)))
         (agent (undefined))
         (agent-ops (undefined))
         (form-ctor (cond ((memq 'double-buffered args) 
                           make-double-buffered-form)
                          (else make-form)))
         (form (form-ctor))
         (contents-ctor (cond ((memq 'double-buffered args) 
                               make-double-buffered-control)
                              (else make-control)))
         (contents (contents-ctor))
         (core-control contents)
         (menu-stack '())
         (activate! (make-unary-method form-type "Activate"))
         (invalidate! (make-unary-method form-type "Invalidate"))
         (unhandled (lambda (method-name)
                      (lambda args
                        (display "Unhandled method ")
                        (display method-name)
                        (newline))))
         (default-impl (lambda (method-name)
                         (cond ((memq method-name agent-ops)
                                (lambda () (agent method-name)))
                               (else
                                (lambda () (unhandled method-name))))))
         (is-closed #f)
         )

    (define (add-if-supported form op-name event-name handler)
      (cond ((memq op-name agent-ops)
             (add-event-handler form event-name handler))
            (else 
             (display "No support for op ")
             (display op-name)
             (newline)
             )))

    (define (key-event-handler on-x)
      (lambda (sender e)
        (let ((alt (key-event-args-alt e))
              (ctrl (key-event-args-control e))
              (shift (key-event-args-shift e))
              ;; code enum excludes modifiers
              (code (key-event-args-keycode e))
              ;; data enum includes modifiers
              (data (key-event-args-keydata e))
              ;; original bitset 
              (value (key-event-args-keyvalue e)))
          ((agent on-x)
           wnd
           (keys-foreign->symbols code)
           `(,@(if alt '(alt) '())
             ,@(if ctrl '(ctrl) '())
             ,@(if shift '(shift) '())))
          )))
    (define (mouse-event-handler on-x)
      (lambda (sender e)
        ((agent on-x)
         wnd ;; should I check that ((wnd 'wndptr)) is eq? with sender?
         (mouse-event-args-x e)
         (mouse-event-args-y e))))
    
    (define horizontal-scrollbar? (undefined))
    (define vertical-scrollbar? (undefined))
    (define horizontal-scroll! (undefined))
    (define vertical-scroll! (undefined))

    (define horizontal-scrollbar (make-hscrollbar))
    (define vertical-scrollbar (make-vscrollbar))

    (define (set-if-present setter! tgt key lst)
      (cond ((assq key lst) => 
             (lambda (l)
               (setter! tgt (cadr l))))))
      
    (define update-scrollbars!/controls
      (let ((hh (control-height horizontal-scrollbar))
            (vw (control-width vertical-scrollbar)))
        (lambda (form)
          '(begin (display `(update-scrollbars!))
                  (newline))
          (let* ((client-size (control-client-size form))
                 (cw (size-width client-size))
                 (ch (size-height client-size)))
            
            (set-control-width! horizontal-scrollbar (- cw vw))
            (set-control-left! vertical-scrollbar (- cw vw))
            (set-control-height! vertical-scrollbar (- ch hh))
            (set-control-top! horizontal-scrollbar (- ch hh))
            (cond ((horizontal-scrollbar?) =>
                   (lambda (props)
                     (set-if-present set-scrollbar-minimum! 
                                     horizontal-scrollbar 'min props)
                     (set-if-present set-scrollbar-maximum!
                                     horizontal-scrollbar 'max props)
                     (set-if-present set-scrollbar-value!
                                     horizontal-scrollbar 'value props)
                     (set-if-present set-scrollbar-largechange!
                                     horizontal-scrollbar 'dlarge props)
                     (set-if-present set-scrollbar-smallchange!
                                     horizontal-scrollbar 'dsmall props)
                     (set-control-height! contents (- ch hh))
                     (set-control-visible! horizontal-scrollbar #t)))
                  (else
                   (set-control-height! contents ch)
                   (set-control-visible! horizontal-scrollbar #f)))
            (cond ((vertical-scrollbar?) =>
                   (lambda (props)
                     (set-if-present set-scrollbar-minimum! 
                                     vertical-scrollbar 'min props)
                     (set-if-present set-scrollbar-maximum!
                                     vertical-scrollbar 'max props)
                     (set-if-present set-scrollbar-value!
                                     vertical-scrollbar 'value props)
                     (set-if-present set-scrollbar-largechange!
                                     vertical-scrollbar 'dlarge props)
                     (set-if-present set-scrollbar-smallchange!
                                     vertical-scrollbar 'dsmall props)
                     (set-control-width! contents (- cw vw))
                     (set-control-visible! vertical-scrollbar #t)))
                  (else 
                   (set-control-width! contents cw)
                   (set-control-visible! vertical-scrollbar #f)))))))

    (define update-scrollbars! update-scrollbars!/controls)

    (define (update!)
      ;; Need to double-check this; for now this signals that a
      ;; control needs to be repainted.
      (update-scrollbars! form)
      (invalidate! core-control)
      )

    (define wnd
     (msg-handler
      ((title)    title)
      ((close)    (form-close! form))
      ((closed?)  is-closed)
      ((wndptr)  core-control) ;; for debugging; not for client code (e.g. agents)
      ((agent)    agent)
      ((width)    (control-width form))
      ((height)   (control-height form))

      ((update)   (update!))

      ((activate) (activate! form))
      ((show)     (show form))
      ((hide)     (hide form))
      ((show-dialog) (form-show-dialog form))
      ((dispose)       
       (((default-impl 'dispose)) wnd) 
       (control-dispose! form))

      ((push-menus . mnus) 
       (let ((main-menu (make-main-menu)))
         (for-each (lambda (mnu) (menu-add-menu-item! main-menu ((mnu 'mnuptr)))) mnus)
         (set! menu-stack (cons main-menu menu-stack))
         (form-set-menu! form main-menu))
       (update!))
      ((pop-menus)
       (set! menu-stack (cdr menu-stack))
       (cond ((not (null? menu-stack))
              (form-set-menu! form (car menu-stack)))
             (else
              (form-set-menu! form clr/null)))
       (update!))

      ((scroll orient magnitude)
       (case orient
         ((vertical) (vertical-scroll! magnitude))
         ((horizontal) (horizontal-scroll! magnitude))
         (else (error 'scroll ": improper orientation " orient))))
       
      ((measure-text txt-string fnt)
       (let ((g (control-creategraphics ((wnd 'wndptr)))))
         (call-with-values 
             (lambda () 
               (((graphics->gfx g) 'measure-text) txt-string fnt))
           (lambda vals
             (graphics-dispose! g)
             (apply values vals)))))

      ;; Are these really necessary in this development model?  Perhaps
      ;; for testing???  (But why not just extract the agent and call
      ;; it manually?)
      ((keydown char)  (((default-impl 'keydown)) wnd char))
      ((mousedown x y) (((default-impl 'mousedown)) wnd x y))

      
      ))
    
    (set! agent (agent-ctor wnd (list-ref bounds 2) (list-ref bounds 3)))
    (set! agent-ops ((agent 'operations)))

    (set! horizontal-scrollbar?
          (if (memq 'horizontal-scrollbar agent-ops)
              (lambda () ((agent 'horizontal-scrollbar)))
              (lambda () #f)))
    (set! vertical-scrollbar?
          (if (memq 'vertical-scrollbar agent-ops)
              (lambda () ((agent 'vertical-scrollbar)))
              (lambda () #f)))
    (set! vertical-scroll! 
          (let ((tell-agent ((default-impl 'on-vscroll))))
            (lambda (mag)
              (let ((val (+ (scrollbar-value vertical-scrollbar) mag)))
                (set-scrollbar-value! vertical-scrollbar val)
                (tell-agent wnd val 'external)))))
    (set! horizontal-scroll! 
          (let ((tell-agent ((default-impl 'on-hscroll))))
            (lambda (mag)
              (let ((val (+ (scrollbar-value horizontal-scrollbar) mag)))
                (set-scrollbar-value! horizontal-scrollbar val)
                (tell-agent wnd val 'external)))))
    
    (add-controls (control-controls form) 
                  `(,@(if (eq? contents core-control) (list contents) '())
                    ,horizontal-scrollbar 
                    ,vertical-scrollbar))
    (set-form-keypreview! form #t)
    (begin
      (display `((hscroll min: ,(scrollbar-minimum horizontal-scrollbar))
                 (hscroll max: ,(scrollbar-maximum horizontal-scrollbar))
                 (vscroll min: ,(scrollbar-minimum vertical-scrollbar))
                 (vscroll max: ,(scrollbar-maximum vertical-scrollbar))))
      (newline))
    (update-scrollbars! form)
    (cond (title (set-control-text! form title)))
    
    (add-event-handler form "FormClosed"
                       (lambda (sender e)
                         (set! is-closed #t)
                         (((default-impl 'on-close)) wnd)))

    (add-if-supported horizontal-scrollbar 'on-hscroll "Scroll"
                      (lambda (sender e)
                        ((agent 'on-hscroll) wnd 
                         (scrolleventargs-newvalue e)
                         (scrolleventargs-gettype e))))
    (add-if-supported vertical-scrollbar 'on-vscroll "Scroll"
                      (lambda (sender e)
                        ((agent 'on-vscroll) wnd 
                         (scrolleventargs-newvalue e)
                         (scrolleventargs-gettype e))))
    
    (add-if-supported core-control 'on-keydown "KeyDown"
                      (key-event-handler 'on-keydown))
    (add-if-supported core-control 'on-keyup "KeyUp"
                      (key-event-handler 'on-keyup))
    (add-if-supported core-control 'on-keypress "KeyPress"
                      (lambda (sender e)
                        ((agent 'on-keypress)
                         ;; Felix believes integer->char is safe based
                         ;; on Microsoft docs...
                         wnd
                         (integer->char
                          (clr/%foreign->int
                           (key-press-event-args-keychar e))))))
    (add-if-supported core-control 'on-mousedown "MouseDown"
                      (mouse-event-handler 'on-mousedown))
    (add-if-supported core-control 'on-mouseup "MouseUp"
                      (mouse-event-handler 'on-mouseup))
    (add-if-supported core-control 'on-mousemove "MouseMove"
                      (mouse-event-handler 'on-mousemove))
    (add-if-supported core-control 'on-mouseenter "MouseEnter"
                      (lambda (sender e) ((agent 'on-mouseenter) wnd)))
    (add-if-supported core-control 'on-mouseleave "MouseLeave"
                      (lambda (sender e) ((agent 'on-mouseleave) wnd)))
    (add-if-supported core-control 'on-mouseclick "MouseClick"
                      (mouse-event-handler 'on-mouseclick))
    (add-if-supported core-control 'on-mousedoubleclick "MouseDoubleClick"
                      (mouse-event-handler 'on-mousedoubleclick))
    
    (add-event-handler form "Resize" 
                       (cond ((memq 'on-resize agent-ops)
                              (lambda (sender e) 
                                ((agent 'on-resize) wnd)
                                ((wnd 'update))))
                             (else
                              (lambda (sender e) 
                                ((wnd 'update))))
                             ))

    (add-if-supported core-control 'on-paint "Paint"
                      (lambda (sender e)
                        (let* ((r (paint-event-args-cliprectangle e))
                               (x (rectangle-x r))
                               (y (rectangle-y r))
                               (h (rectangle-height r))
                               (w (rectangle-width r))
                               (g (paint-event-args-graphics e)))
                          ((agent 'on-paint) 
                           wnd (graphics->gfx g) x y w h))))
    
    wnd))

