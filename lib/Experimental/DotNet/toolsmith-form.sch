(clr/%load-assembly "System.Windows.Forms" "2.0.0.0" "" "b77a5c561934e089")

(define (find-forms-type name)
  (find-clr-type (string-append "System.Windows.Forms." name)))
(define (find-drawing-type name)
  (find-clr-type (string-append "System.Drawing." name)))

(define o
  (lambda funcs
    (lambda (arg)
      (let loop ((fs funcs))
        (cond
         ((null? fs) arg)
         (else ((car fs) (loop (cdr fs)))))))))

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

;; This kills the event loop.  It does not seem to kill the main REPL
;; thread, so it seems to be appropriate for flushing the event loop
;; (e.g. when a buggy callback is making life difficult.)
(define application-exit-thread!!!
  (let* ((application-type (find-forms-type "Application"))
         (meth (clr/%get-method application-type "ExitThread" '#())))
    (lambda ()
      (clr/%invoke meth clr/null '#()))))

(define toolsmith-interrupt-handler
  (lambda ()
    '(begin (display "timer interrupt was fired!") 
            (newline))
    (let* ((orig-error-handler (error-handler))
           (new-error-handler
            ;; on errors, avoid repeat interrupt (buggy callback mire)
            (lambda args
              (begin (display "interrupt had error!")
                     (newline))
              (application-exit-thread!!!)
              (apply orig-error-handler args))))
      (parameterize ((error-handler new-error-handler))
        (application-do-events!)
        (enable-interrupts (standard-timeslice))))))

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
(define mouse-event-args-button 
  (make-property-ref mouse-event-args-type "Button"
                     (enum-type->foreign->symbol 
                      (find-forms-type "MouseButtons"))))
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
         (get-name     (make-property-ref font-type "Name"))
         (get-size     (make-property-ref font-type "Size"))
         (get-italic   (make-property-ref font-type "Italic"))
         (get-bold     (make-property-ref font-type "Bold"))
         (get-uline    (make-property-ref font-type "Underline"))
         (set-italic   (make-bool-pset "Italic"))
         (set-bold     (make-bool-pset "Bold"))
        (set-uline    (make-bool-pset "Underline"))
        (set-emsize   (make-property-setter font-type "Size" 
                                            clr/%number->foreign-int32)))
    (define (construct fontptr)
      (make-root-object fnt
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
  (make-root-object col
   ((name)     (name->symbol (color-name colorptr)))
   ((alpha)    (clr/%foreign->int (color-alpha colorptr)))
   ((red)      (clr/%foreign->int (color-red   colorptr)))
   ((green)    (clr/%foreign->int (color-green colorptr)))
   ((blue)     (clr/%foreign->int (color-blue colorptr)))
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
                     (stringo (clr/%string->foreign string))
                     (fntptr ((fnt 'fntptr)))
                     (maxint30 (clr/%number->foreign-int32 (most-positive-fixnum)))
                     (sza (clr/%invoke measure-text-method 
                                       g (vector stringa fntptr maxint30 string-format)))
                     (szf (clr/%invoke measure-text-method 
                                       g (vector string* fntptr maxint30 string-format)))
                     (szo (clr/%invoke measure-text-method
                                       g (vector stringo fntptr maxint30 string-format))))
                (values (max (sizef-width szo) 
                             (- (sizef-width szf) (sizef-width sza)))
                        (sizef-height szf))))))
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
         (fill-rect-method/exact   (clr/%get-method
                                    graphics-type
                                    "FillRectangle"
                                    (vector brush-type
                                            clr-type-handle/system-int32
                                            clr-type-handle/system-int32
                                            clr-type-handle/system-int32
                                            clr-type-handle/system-int32)))
         (fill-rect-method/inexact (clr/%get-method
                                    graphics-type
                                    "FillRectangle"
                                    (vector brush-type
                                            clr-type-handle/system-single
                                            clr-type-handle/system-single
                                            clr-type-handle/system-single
                                            clr-type-handle/system-single)))
         (draw-or-fill-rect 
          (lambda (g op col x1 y1 x2 y2)
            (define colptr ((col 'colptr)))
            (define (fixnum->int32 x)
              (clr/%number->foreign-int32 x))
            (define (number->single x)
              (clr/%flonum->foreign-single (exact->inexact x)))
            (let-values (((meth ink ink-dispose! num)
                          (cond 
                           ((and (fixnum? x1) (fixnum? y1)
                                 (fixnum? x2) (fixnum? y2))
                            (case op
                              ((fill) (values fill-rect-method/exact
                                              (make-solid-brush colptr)
                                              brush-dispose!
                                              fixnum->int32))
                              ((draw) (values draw-rect-method/exact
                                              (make-pen colptr)
                                              pen-dispose!
                                              fixnum->int32))))
                           (else
                            (case op
                              ((fill) (values fill-rect-method/inexact
                                              (make-solid-brush colptr)
                                              brush-dispose!
                                              number->single))
                              ((draw) (values draw-rect-method/inexact
                                              (make-pen colptr)
                                              pen-dispose!
                                              number->single)))))))
              (let ((x (num (min x1 x2)))
                    (y (num (min y1 y2)))
                    (w (num (abs (- x2 x1))))
                    (h (num (abs (- y2 y1)))))
                (clr/%invoke meth g (vector ink x y w h))
                (ink-dispose! ink)))))
          
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
      (make-root-object gfx
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
        (draw-or-fill-rect g 'draw col x1 y1 x2 y2))
       ((fill-rect col x1 y1 x2 y2)
        (draw-or-fill-rect g 'fill col x1 y1 x2 y2))
                               
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
    (make-root-object mnu
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

    (define (add-if-supported form op-name event-name mk-handler)
      (cond ((memq op-name agent-ops)
             (add-event-handler form event-name (mk-handler (agent op-name))))
            (else 
             (display "No support for op ")
             (display op-name)
             (newline))))


    ;; A KeySym is a Symbol that is one of the enumerated values of
    ;; keys-type
    
    ;; KeySym -> [Maybe Char]
    ;; Converts ks to the corresponding character on a keyboard.
    ;; This would seem like a very silly task, since the KeyPress
    ;; events already give us the integer code that we can convert
    ;; directly into a character.  However, the fire ordering of
    ;; keyevents for KeyPress and KeyUp seems to be non-deterministic.
    (define (keysym->maybe-char ks shift)
      (case ks
        ((Escape escape)                 #\esc)
        ((Enter enter)                   #\newline)
        ((Return return)                 #\return)
        ((Space space)                   #\space)
        ((Tab tab)                       #\tab)
        ((Back back)                     #\backspace)
        ((OemPlus Oemplus oemplus)       (if shift #\+ #\=))
        ((D1 d1)                         (if shift #\! #\1))
        ((D2 d2)                         (if shift #\@ #\2))
        ((D3 d3)                         (if shift #\# #\3))
        ((D4 d4)                         (if shift #\$ #\4))
        ((D5 d5)                         (if shift #\% #\5))
        ((D6 d6)                         (if shift #\^ #\6))
        ((D7 d7)                         (if shift #\& #\7))
        ((D8 d8)                         (if shift #\* #\8))
        ((D9 d9)                         (if shift #\( #\9))
        ((D0 d0)                         (if shift #\) #\0))
        ((OemMinus Oemminus oemminus)    (if shift #\_ #\-))
        ((OemSemicolon Oemsemicolon
          oemsemicolon
          Oem1 oem1)                     (if shift #\: #\;))
        ((OemComma Oemcomma oemcomma)    (if shift #\< #\,))
        ((OemPeriod Oemperiod oemperiod) (if shift #\> #\.))
        ((OemQuestion Oemquestion 
          oemquestion 
          Oem2 oem2)                     (if shift #\? #\/))
        ((OemTilde Oemtilde oemtilde
          Oem3 oem3)                     (if shift #\~ #\`))
        ((OemOpenBrackets OemOpenbrackets
          Oemopenbrackets oemopenbrackets
          Oem4 oem4)                     (if shift #\{ #\[))
        ((OemPipe Oempipe 
          oempipe
          Oem5 oem5)                     (if shift #\| #\\))
        ((OemCloseBrackets OemClosebrackets
          Oemclosebrackets oemclosebrackets
          Oem6 oem6)                     (if shift #\} #\]))
        ((OemQuotes Oemquotes
          oemquotes
          Oem7 oem7)                     (if shift #\" #\'))
        ;; There is also OemBackslash, but (on my keyboard) 
        ;; that is handled by OemPipe, and I have not managed 
        ;; to generate a KeyEvent with OemBackslash in it...
        ;; (Is there a different keyboard type that maps this
        ;; differently?)

        ((A a) (if shift #\A #\a))
        ((B b) (if shift #\B #\b))
        ((C c) (if shift #\C #\c))
        ((D d) (if shift #\D #\d))
        ((E e) (if shift #\E #\e))
        ((F f) (if shift #\F #\f))
        ((G g) (if shift #\G #\g))
        ((H h) (if shift #\H #\h))
        ((I i) (if shift #\I #\i))
        ((J j) (if shift #\J #\j))
        ((K k) (if shift #\K #\k))
        ((L l) (if shift #\L #\l))
        ((M m) (if shift #\M #\m))
        ((N n) (if shift #\N #\n))
        ((O o) (if shift #\O #\o))
        ((P p) (if shift #\P #\p))
        ((Q q) (if shift #\Q #\q))
        ((R r) (if shift #\R #\r))
        ((S s) (if shift #\S #\s))
        ((T t) (if shift #\T #\t))
        ((U u) (if shift #\U #\u))
        ((V v) (if shift #\V #\v))
        ((W w) (if shift #\W #\w))
        ((X x) (if shift #\X #\x))
        ((Y y) (if shift #\Y #\y))
        ((Z z) (if shift #\Z #\z))

        (else #f)))

    (define (key-event-handler on-x)
      (lambda (sender e)
        (let* ((alt (key-event-args-alt e))
               (ctrl (key-event-args-control e))
               (shift (key-event-args-shift e))
               ;; code enum excludes modifiers
               (code (key-event-args-keycode e))
               ;; data enum includes modifiers
               (data (key-event-args-keydata e))
               ;; original bitset 
               (value (key-event-args-keyvalue e))
               (keysym (keys-foreign->symbols code)))
          (on-x
           (keysym->maybe-char keysym shift)
           keysym
           `(,@(if alt '(alt) '())
             ,@(if ctrl '(ctrl) '())
             ,@(if shift '(shift) '())))
          )))
    (define (mouse-event-handler on-x)
      (lambda (sender e)
        (on-x
         (mouse-event-args-x e)
         (mouse-event-args-y e))))
    (define (trivial-handler on-x)
      (lambda (sender e)
        (on-x)))
    
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
     (make-root-object wnd
      ((title)    title)
      ((close)    (form-close! form))
      ((closed?)  is-closed)
      ((wndptr)  core-control) ;; for debugging; not for client code (e.g. agents)
      ((agent)    agent)
      ((width)    (control-width contents))
      ((height)   (control-height contents))

      ((update)   (update!))

      ((activate) (activate! form))
      ((show)     (show form))
      ((hide)     (hide form))
      ((show-dialog) (form-show-dialog form))
      ((dispose)       
       (((default-impl 'dispose)))
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

      ((attempt-scroll orient magnitude)
       (define (out-of-range-handler val min max) 'ignore)
       (case orient
         ((vertical) (vertical-scroll! magnitude out-of-range-handler))
         ((horizontal) (horizontal-scroll! magnitude out-of-range-handler))
         (else (error 'scroll ": improper orientation " orient))))
      ((scroll orient magnitude)
       (define (out-of-range-handler val min max)
         (error orient ": " val " is not in range [" min "," max "]"))
       (case orient
         ((vertical) (vertical-scroll! magnitude out-of-range-handler))
         ((horizontal) (horizontal-scroll! magnitude out-of-range-handler))
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
      ((keydown char)  (((default-impl 'keydown)) char))
      ((mousedown x y) (((default-impl 'mousedown)) x y))

      
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
            (lambda (mag out-of-range-handler)
              (let ((val (+ (scrollbar-value vertical-scrollbar) mag))
                    (min (scrollbar-minimum vertical-scrollbar))
                    (max (scrollbar-maximum vertical-scrollbar)))
                (cond ((<= min val max)
                       (set-scrollbar-value! vertical-scrollbar val)
                       (tell-agent val 'external))
                      (else 
                       (out-of-range-handler val min max)))))))

    (set! horizontal-scroll! 
          (let ((tell-agent ((default-impl 'on-hscroll))))
            (lambda (mag)
              (let ((val (+ (scrollbar-value horizontal-scrollbar) mag))
                    (min (scrollbar-minimum horizontal-scrollbar))
                    (max (scrollbar-maximum horizontal-scrollbar)))
                (cond ((<= min val max)
                       (set-scrollbar-value! horizontal-scrollbar val)
                       (tell-agent val 'external))
                      (else (error 'horizontal-scroll! ": " val
                                   " is not in range [" min "," max "]")))))))
    
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
                         (((default-impl 'on-close)))))

    (add-if-supported horizontal-scrollbar 'on-hscroll "Scroll"
                      (lambda (on-hscroll)
                        (lambda (sender e)
                          (on-hscroll
                           (scrolleventargs-newvalue e)
                           (scrolleventargs-gettype e)))))
    (add-if-supported vertical-scrollbar 'on-vscroll "Scroll"
                      (lambda (on-vscroll)
                        (lambda (sender e)
                          (on-vscroll
                           (scrolleventargs-newvalue e)
                           (scrolleventargs-gettype e)))))
    
    (add-if-supported core-control 'on-keydown "KeyDown" key-event-handler)
    (add-if-supported core-control 'on-keyup "KeyUp" key-event-handler)
    (add-if-supported core-control 'on-keypress "KeyPress"
                      (lambda (on-keypress)
                        (lambda (sender e)
                          (on-keypress
                           ;; Felix believes integer->char is safe based
                           ;; on Microsoft docs...
                           (integer->char
                            (clr/%foreign->int
                             (key-press-event-args-keychar e)))))))
    (add-if-supported core-control 'on-mousedown "MouseDown" mouse-event-handler)
    (add-if-supported core-control 'on-mouseup "MouseUp" mouse-event-handler)
    (let ((add! (lambda (fcn) 
                  (add-event-handler core-control "MouseMove" fcn)))
          (has-move? (memq 'on-mousemove agent-ops))
          (has-drag? (memq 'on-mousedrag agent-ops)))
      (cond 
       ((and has-move? has-drag?)
        (let ((move-handler (mouse-event-handler (agent 'on-mousemove)))
              (drag-handler (mouse-event-handler (agent 'on-mousedrag))))
          (add! (lambda (sender e)
                  (case (mouse-event-args-button e)
                    ((none) (move-handler sender e))
                    (else   (drag-handler sender e)))))))
       (has-move?
        (let ((move-handler (mouse-event-handler (agent 'on-mousemove))))
          (add! (lambda (sender e)
                  (case (mouse-event-args-button e)
                    ((none) (move-handler sender e)))))))
       (has-drag?
        (let ((drag-handler (mouse-event-handler (agent 'on-mousedrag))))
          (add! (lambda (sender e)
                  (case (mouse-event-args-button e)
                    ((none) 'do-nothing)
                    (else   (drag-handler sender e)))))))))
    (add-if-supported core-control 'on-mouseenter "MouseEnter" trivial-handler)
    (add-if-supported core-control 'on-mouseleave "MouseLeave" trivial-handler)
    (add-if-supported core-control 'on-mouseclick "MouseClick" mouse-event-handler)
    (add-if-supported core-control 'on-mousedoubleclick "MouseDoubleClick" 
                      mouse-event-handler)
    
    (add-event-handler form "Resize" 
                       (cond ((memq 'on-resize agent-ops)
                              (let ((resize-op (agent 'on-resize))
                                    (update-op (wnd 'update)))
                                (lambda (sender e) 
                                  (resize-op)
                                  (update-op))))
                             (else
                              (let ((update-op (wnd 'update)))
                                (lambda (sender e) 
                                  (update-op))))))

    (add-if-supported core-control 'on-paint "Paint"
                      (lambda (on-paint)
                        (lambda (sender e)
                          (let* ((r (paint-event-args-cliprectangle e))
                                 (x (rectangle-x r))
                                 (y (rectangle-y r))
                                 (h (rectangle-height r))
                                 (w (rectangle-width r))
                                 (g (paint-event-args-graphics e)))
                            (on-paint
                             (graphics->gfx g) x y w h)))))

    wnd))



;; A FileChoiceFilter is a (list String String String ...)
;; interpretation:
;; A FileChoiceFilter (list description ext-1 ... ext-n) tells the file chooser to
;; only show file that end with the extension ext-i for some i.

(define file-chooser-dialog-maker 
  (let* ((file-dialog-type (find-forms-type "FileDialog"))
         (set-initial-directory! 
          (make-property-setter file-dialog-type "InitialDirectory"))
         (set-filter!
          (make-property-setter file-dialog-type "Filter"))
         (set-filter-index!
          (make-property-setter file-dialog-type "FilterIndex"))

         (filter->string
          (lambda (filter)
            (let* ((desc (car filter))
                   (head-ext (cadr filter))
                   (tail-ext (cddr filter))
                   (tail-strings 
                    (map (lambda (x) (string-append ";" "*." x)) tail-ext)))
              (apply string-append 
                     desc "|" "*." head-ext tail-strings))))
         (filters->string
          (lambda (filters)
            (let ((strings (map filter->string filters)))
              (cond 
               ((null? strings) "")
               (else
                (apply string-append
                       (car strings) (map (lambda (x) (string-append "|" x))
                                          strings))))))))
    (lambda (dialog-type)
      (let* ((make-file-dialog (type->nullary-constructor dialog-type)))
        (lambda (dir-string filter-list)
          (let ((d (make-file-dialog)))
            (set-initial-directory! d dir-string)
            (set-filter! d (filters->string filter-list))
            (set-filter-index! d 0)
            d))))))

;; show-and-post-process-dialog : Dialog -> [Maybe String]
(define (show-and-post-process-dialog d)
  (let ((dialog-result (show-dialog d))
        (file-dialog-type (find-forms-type "FileDialog"))
        (get-filename
         (make-property-ref file-dialog-type "FileName")))
    (case dialog-result
      ((OK ok) (get-filename d))
      (else #f))))

;; open-file-chooser-dialog : String [Listof FileChoiceFilter] -> [Maybe String]
(define open-file-chooser-dialog
  (let ((maker (file-chooser-dialog-maker (find-forms-type "OpenFileDialog"))))
    (lambda (dir-string filter-list)
      (let ((d (maker dir-string filter-list)))
        (show-and-post-process-dialog d)))))

;; save-file-chooser-dialog : String [Listof FileChoiceFilter] -> [Maybe String]
(define save-as-file-chooser-dialog
  (let ((maker (file-chooser-dialog-maker (find-forms-type "SaveFileDialog"))))
    (lambda (dir-string filter-list)
      (let ((d (maker dir-string filter-list)))
        (show-and-post-process-dialog d)))))

  
;; Clipboard operations...
(define clipboard-type (find-forms-type "Clipboard"))
(define clipboard-clear!
  (let ((f (make-static-method clipboard-type "Clear")))
    (lambda ()
      (f)
      (unspecified))))
(define clipboard-contains-audio?
  (let ((f (make-static-method clipboard-type "ContainsAudio")))
    (lambda ()
      (clr/foreign->bool (f)))))
(define clipboard-contains-filelist?
  (let ((f (make-static-method clipboard-type "ContainFileDropList")))
    (lambda ()
      (clr/foreign->bool (f)))))
(define clipboard-contains-image?
  (let ((f (make-static-method clipboard-type "ContainsImage")))
    (lambda () 
      (clr/foreign->bool (f)))))
(define clipboard-contains-text?
  (let ((f (make-static-method clipboard-type "ContainsText")))
    (lambda ()
      (clr/foreign->bool (f)))))
(define clipboard-get-filelist
  (let ((f (make-static-method clipboard-type "GetFileDropList")))
    (lambda ()
      (stringcollection->list (f)))))
(define clipboard-get-image
  (let ((get-image (make-static-method clipboard-type "GetImage")))
    (lambda ()
      (error 'clipboard-get-image ": unimplemented"))))
(define clipboard-get-text
  (let ((get-text (make-static-method clipboard-type "GetText")))
    (lambda ()
      (let ((t (get-text)))
        (clr/foreign->string t)))))
(define clipboard-set-filelist!
  (let ((set-files! (make-static-method 
		     clipboard-type "SetFileDropList"
		     stringcollection-type)))
    (lambda (files)
      (set-files! (list->stringcollection files))
      (unspecified))))
(define clipboard-set-image! 
  (let ((set-image (make-static-method clipboard-type "SetImage" image-type)))
    (lambda (img)
      (error 'clipboard-set-image! ": unimplemented"))))
(define clipboard-set-text!
  (let ((set-text! (make-static-method
		    clipboard-type "SetText" 
		    clr-type-handle/system-string)))
    (lambda (string)
      (set-text! (clr/string->foreign string))
      (unspecified))))


        
    
  
