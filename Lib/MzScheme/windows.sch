;;; -*- Mode: Scheme; coding: iso-8859-1 -*-

;;; A demo of using dotnet to talk to Windows.
;;;
;;; To use this code, follow these steps:
;;;
;;;  You should no longer load  dotnet-ffi.sch, it is built into the image.
;;;  You should no longer load      dotnet.sch, it is built into the image.
;;;
;;; 1.  (enable-dotnet!)
;;; 2.  Load  windows.sch  (this file)
;;; 3.  (window-demo)

;;; Note that when you load this file, and when you first call
;;; (window-demo), that Scheme must do a lot of work to build a model
;;; of the .NET types that implement Windows Forms.  It will take some
;;; time for this file to load, and several seconds for the window to
;;; appear when you first call (window-demo).

;;; WARNING WARNING WARNING

;;; The bulk of the code in this file is not an example of how to
;;; interface to windows via JavaDot notation.  The bulk of this code
;;; bypasses the JavaDot layer in order to hook into the Windows
;;; message pump at the lowest possible level.  Since every windows
;;; message will be passed through this code, an effort is made to do
;;; as little work as necessary to decode and dispatch on the windows
;;; message.

;;; The code between here and `SAMPLE CODE' below implement a generic
;;; function API to the windows message loop.  When a windows message
;;; is received, it is dispatched to an appropriately named generic
;;; function.  The generic function has a default method of simply
;;; declining to handle the method (returning false).  The sample code
;;; shows how you can add a method to the generic function to modify
;;; how windows messages are handled.

(define *system-drawing-assembly* #f)
(define *system-windows-forms-assembly* #f)

(define (system-drawing-assembly)
  (or *system-drawing-assembly*
      (begin (set! *system-drawing-assembly*
                   (System.Reflection.Assembly.LoadWithPartialName "System.Drawing"))
             ;; Preload some important classes.
             (list
              System.Drawing.ContentAlignment.class
              System.Drawing.Drawing2D.FillMode.class
              System.Drawing.Drawing2D.GraphicsPath.class
              System.Drawing.Drawing2D.RegionData.class
              ;; System.Drawing.PointF[].class
              ;; System.Drawing.Point[].class
              System.Drawing.Region.class)
             *system-drawing-assembly*)))

(define (system-windows-forms-assembly)
  (or *system-windows-forms-assembly*
      (begin (system-drawing-assembly)
             (set! *system-windows-forms-assembly*
                   (System.Reflection.Assembly.LoadWithPartialName "System.Windows.Forms"))
             ;; Preload some important classes.
             (list
              System.Windows.Forms.AccessibleEvents.class
              System.Windows.Forms.AccessibleRole.class
              System.Windows.Forms.AmbientProperties.class
              System.Windows.Forms.Application.class
              System.Windows.Forms.ApplicationContext.class
              System.Windows.Forms.BaseCollection.class
              System.Windows.Forms.Binding.class
              System.Windows.Forms.BindingContext.class
              System.Windows.Forms.BindingsCollection.class
              System.Windows.Forms.BoundsSpecified.class
              System.Windows.Forms.ContextMenu.class
              System.Windows.Forms.ControlBindingsCollection.class
              System.Windows.Forms.ControlEventArgs.class
              System.Windows.Forms.ControlEventHandler.class
              System.Windows.Forms.ControlStyles.class
              System.Windows.Forms.CreateParams.class
              System.Windows.Forms.Cursor.class
              System.Windows.Forms.DialogResult.class
              System.Windows.Forms.DragAction.class
              System.Windows.Forms.DragDropEffects.class
              System.Windows.Forms.DragEventArgs.class
              System.Windows.Forms.DragEventHandler.class
              System.Windows.Forms.Form.class
              System.Windows.Forms.FormBorderStyle.class
              System.Windows.Forms.FormStartPosition.class
              System.Windows.Forms.FormWindowState.class
              ;; System.Windows.Forms.Form[].class
              System.Windows.Forms.GiveFeedbackEventArgs.class
              System.Windows.Forms.GiveFeedbackEventHandler.class
              ;; System.Windows.Forms.GraphicsBufferManager.class
              System.Windows.Forms.HelpEventArgs.class
              System.Windows.Forms.HelpEventHandler.class
              System.Windows.Forms.HorizontalAlignment.class
              System.Windows.Forms.IButtonControl.class
              System.Windows.Forms.ICommandExecutor.class
              System.Windows.Forms.IDataObject.class
              System.Windows.Forms.InputLanguageChangedEventHandler.class
              System.Windows.Forms.InputLanguageChangingEventHandler.class
              System.Windows.Forms.InvalidateEventArgs.class
              System.Windows.Forms.InvalidateEventHandler.class
              System.Windows.Forms.KeyEventArgs.class
              System.Windows.Forms.KeyEventHandler.class
              System.Windows.Forms.KeyPressEventArgs.class
              System.Windows.Forms.KeyPressEventHandler.class
              System.Windows.Forms.LayoutEventHandler.class
              System.Windows.Forms.LeftRightAlignment.class
              System.Windows.Forms.MainMenu.class
              System.Windows.Forms.MdiClient.class
              System.Windows.Forms.MdiLayout.class
              System.Windows.Forms.Menu.class
              System.Windows.Forms.MenuItem.class
              ;; System.Windows.Forms.MenuItem[].class
              System.Windows.Forms.MenuMerge.class
              System.Windows.Forms.MouseEventArgs.class
              System.Windows.Forms.MouseEventHandler.class
              System.Windows.Forms.PaintEventArgs.class
              System.Windows.Forms.PaintEventHandler.class
              System.Windows.Forms.PropertyStore.class
              System.Windows.Forms.QueryAccessibilityHelpEventHandler.class
              System.Windows.Forms.QueryContinueDragEventArgs.class
              System.Windows.Forms.QueryContinueDragEventHandler.class
              System.Windows.Forms.RightToLeft.class
              System.Windows.Forms.Shortcut.class
              System.Windows.Forms.SizeGripStyle.class
              System.Windows.Forms.UICues.class
              System.Windows.Forms.UICuesEventArgs.class
              System.Windows.Forms.UICuesEventHandler.class)
             *system-windows-forms-assembly*)))

;; Bypass the fancy dotnet api and use the raw one.
;; Performance is critical on this.
(define clr-type-handle/system-windows-forms-message
  (clr-assembly/%get-type
   (clr-object/clr-handle (system-windows-forms-assembly))
   (clr/%string->foreign "System.Windows.Forms.Message")))

(define %message/code
  (let ((handle (clr/%get-property clr-type-handle/system-windows-forms-message "Msg" '#())))
    (lambda (message) (clr/%property-ref-int handle message '#()))))

(define %message/handle
  (let ((handle (clr/%get-property clr-type-handle/system-windows-forms-message "HWnd" '#()))
        (native-window-class System.Windows.Forms.NativeWindow.class))
    (lambda (message)
      (wrap-clr-object native-window-class (clr/%property-ref-window handle message '#())))))

(define %message/wparam
  (let ((handle (clr/%get-property clr-type-handle/system-windows-forms-message "WParam" '#())))
    (lambda (message) 0)))

(define %message/wparam-int
  (let ((handle (clr/%get-property clr-type-handle/system-windows-forms-message "WParam" '#())))
    (lambda (message)
      (clr/%property-ref-intptr-int handle message '#()))))

(define %message/lparam
  (let ((handle (clr/%get-property clr-type-handle/system-windows-forms-message "LParam" '#())))
    (lambda (message) 0)))

(define %message/lparam-int
  (let ((handle (clr/%get-property clr-type-handle/system-windows-forms-message "LParam" '#())))
    (lambda (message)
      (clr/%property-ref-intptr-int handle message '#()))))

(define-syntax lparam/xpos
  (syntax-rules ()
    ((lparam/xpos lparam) (remainder lparam #x00010000))))

(define-syntax lparam/ypos
  (syntax-rules ()
    ((lparam/ypos lparam) (quotient lparam #x00010000))))

(define missing-message-handler
  (let ((message-class System.Windows.Forms.Message.class))
    (lambda (message)
      (newline)
      (display "Missing message handler for ")
      (display (wrap-clr-object message-class message))
      ;; decline it
      #f)))

(define (ldb width position source)
  (logand (quotient source (vector-ref '#(#x00000001
                                          #x00000002
                                          #x00000004
                                          #x00000008
                                          #x00000010
                                          #x00000020
                                          #x00000040
                                          #x00000080
                                          #x00000100
                                          #x00000200
                                          #x00000400
                                          #x00000800
                                          #x00001000
                                          #x00002000
                                          #x00004000
                                          #x00008000
                                          #x00010000
                                          #x00020000
                                          #x00040000
                                          #x00080000
                                          #x00100000
                                          #x00200000
                                          #x00400000
                                          #x00800000
                                          #x01000000
                                          #x02000000
                                          #x04000000
                                          #x08000000
                                          #x10000000
                                          #x20000000
                                          #x40000000
                                          #x80000000
                                          ) position))
          (vector-ref '#(#x00000000
                         #x00000001
                         #x00000003
                         #x00000007
                         #x0000000F
                         #x0000001F
                         #x0000003F
                         #x0000007F
                         #x000000FF
                         #x000001FF
                         #x000003FF
                         #x000007FF
                         #x00000FFF
                         #x00001FFF
                         #x00003FFF
                         #x00007FFF
                         #x0000FFFF
                         #x0001FFFF
                         #x0003FFFF
                         #x0007FFFF
                         #x000FFFFF
                         #x001FFFFF
                         #x003FFFFF
                         #x007FFFFF
                         #x00FFFFFF
                         #x01FFFFFF
                         #x03FFFFFF
                         #x07FFFFFF
                         #x0FFFFFFF
                         #x1FFFFFFF
                         #x3FFFFFFF
                         #x7FFFFFFF
                         #xFFFFFFFF) width)))

(define (decode-windows-message/key generic-receiver)
  (lambda (message)
    (let ((lparam (%message/lparam-int message)))
      (generic-receiver (%message/handle message)
                        (%message/wparam-int message)
                        (zero? (ldb 1 31 lparam))
                        (zero? (ldb 1 30 lparam))
                        (zero? (ldb 1 29 lparam))
                        (zero? (ldb 1 24 lparam))
                        (ldb 8 16 lparam)
                        (remainder lparam #x00010000)))))

(define (decode-windows-message/lparam-pointer generic-receiver)
  (lambda (message)
    (generic-receiver (%message/handle message)
                      (%message/lparam message))))

(define (decode-windows-message/lparam-xy generic-receiver)
  (lambda (message)
    (let ((lparam (%message/lparam-int message)))
      (generic-receiver (%message/handle message)
                        (lparam/xpos lparam)
                        (lparam/ypos lparam)))))

(define (decode-windows-message/mouse generic-receiver)
  (lambda (message)
    (let ((lparam (%message/lparam-int message)))
      (generic-receiver (%message/handle message)
                        (%message/wparam-int message)
                        (lparam/xpos lparam)
                        (lparam/ypos lparam)))))

;; WM_NCHITTEST position codes
(define *hit-code-base* -2)
(define *hit-codes*
  '#(hterror
     httransparent
     htnowhere
     htclient
     htcaption
     htsysmenu
     htgrowbox
     htmenu
     hthscroll
     htvscroll
     htminbutton
     htmaxbutton
     htleft
     htright
     httop
     httopleft
     httopright
     htbottom
     htbottomleft
     htbottomright
     htborder
     htobject
     htclose
     hthelp))

(define (decode-windows-message/ncmouse generic-receiver)
  (lambda (message)
    (let ((lparam (%message/lparam-int message)))
      (generic-receiver (%message/handle message)
                        (vector-ref *hit-codes* (- (%message/wparam-int message) *hit-code-base*))
                        (lparam/xpos lparam)
                        (lparam/ypos lparam)))))

(define (decode-windows-message/no-arguments generic-receiver)
  (lambda (message)
    (generic-receiver (%message/handle message))))

(define (decode-windows-message/syscommand generic-receiver)
  (lambda (message)
    (let ((lparam (%message/lparam-int message)))
      (generic-receiver (%message/handle message)
                        (case (logand #xFFF0 (%message/wparam-int message))
                          ((#xF060) ':close)
                          ((#xF100) ':keymenu)
                          (else ':unrecognized))
                        (lparam/xpos lparam)
                        (lparam/ypos lparam)))))

(define (decode-windows-message/device-change generic-receiver)
  (lambda (message)
    (generic-receiver (%message/handle message)
                      (case (logand #xFFFF (%message/wparam-int message))
                        ((#x0007) ':devnodes-changed)
                        ((#x0017) ':query-change-config)
                        ((#x0018) ':config-changed)
                        ((#x0019) ':config-change-cancelled)
                        ((#x8000) ':device-arrival)
                        ((#x8001) ':device-query-remove)
                        ((#x8002) ':device-query-remove-failed)
                        ((#x8003) ':device-remove-pending)
                        ((#x8004) ':device-remove-complete)
                        ((#x8005) ':device-type-specific)
                        ((#x8006) ':custom-event)
                        ((#xFFFF) ':user-defined)
                        (else ':unrecognized)))))

;;; This table contains the handlers for the #x400 system defined
;;; windows messages.  The handlers will destructure the message and
;;; invoke the appropriate generic function.

(define *system-message-limit* #x400)
(define *windows-message-handlers*
  (let ((handlers (make-vector *system-message-limit*)))
    (let loop ((i 0))
      (if (< i *system-message-limit*)
          (begin
            (vector-set! handlers i missing-message-handler)
            (loop (+ i 1)))
          handlers))))

(define (standard-message-dispatch message)
  (let ((code (%message/code message)))
    (if (< code *system-message-limit*)
        ((vector-ref *windows-message-handlers* code) message)
        (user-message-dispatch code message))))

;;; Stub
(define (user-message-dispatch code message)
  ;; Return #f to decline the message.
  #f)

(define-syntax define-windows-message
  (syntax-rules ()
    ((define-windows-message msg-name code (arg0))
     #f)
    ((define-windows-message msg-name code (arg0) decoder)
     (define msg-name
       (let ((generic (make <generic>
                        ':arity 1
                        ':combination generic-or-combination
                        ':name 'msg-name)))
         (vector-set! *windows-message-handlers* code (decoder generic))
         (add-method generic
           (make <method>
             ':arity 1
             ':procedure (lambda (call-next-method arg0) #f)))
         generic)))
    ((define-windows-message msg-name code (arg0 arg1) decoder)
     (define msg-name
       (let ((generic (make <generic>
                        ':arity 2
                        ':combination generic-or-combination
                        ':name 'msg-name)))
         (vector-set!  *windows-message-handlers* code (decoder generic))
         (add-method generic
           (make <method>
             ':arity 2
             ':procedure (lambda (call-next-method arg0 arg1) #f)))
         generic)))
    ((define-windows-message msg-name code (arg0 arg1 arg2) decoder)
     (define msg-name
       (let ((generic (make <generic>
                        ':arity 3
                        ':combination generic-or-combination
                        ':name 'msg-name)))
         (vector-set! *windows-message-handlers* code (decoder generic))
         (add-method generic
           (make <method>
             ':arity 3
             ':procedure (lambda (call-next-method arg0 arg1 arg2) #f)))
         generic)))
    ((define-windows-message msg-name code (arg0 arg1 arg2 arg3) decoder)
     (define msg-name
       (let ((generic (make <generic>
                        ':arity 4
                        ':combination generic-or-combination
                        ':name 'msg-name)))
         (vector-set! *windows-message-handlers* code (decoder generic))
         (add-method generic
           (make <method>
             ':arity 4
             ':procedure (lambda (call-next-method arg0 arg1 arg2 arg3) #f)))
         generic)))
    ((define-windows-message msg-name code (arg0 arg1 arg2 arg3 arg4) decoder)
     (define msg-name
       (let ((generic (make <generic>
                        ':arity 5
                        ':combination generic-or-combination
                        ':name 'msg-name)))
         (vector-set! *windows-message-handlers* code (decoder generic))
         (add-method generic
           (make <method>
             ':arity 5
             ':procedure (lambda (call-next-method arg0 arg1 arg2 arg3 arg4) #f)))
         generic)))
    ((define-windows-message msg-name code (arg0 arg1 arg2 arg3 arg4 arg5) decoder)
     (define msg-name
       (let ((generic (make <generic>
                        ':arity 6
                        ':combination generic-or-combination
                        ':name 'msg-name)))
         (vector-set! *windows-message-handlers* code (decoder generic))
         (add-method generic
           (make <method>
             ':arity 6
             ':procedure (lambda (call-next-method arg0 arg1 arg2 arg3 arg4 arg5) #f)))
         generic)))
    ((define-windows-message msg-name code (arg0 arg1 arg2 arg3 arg4 arg5 arg6) decoder)
     (define msg-name
       (let ((generic (make <generic>
                        ':arity 7
                        ':combination generic-or-combination
                        ':name 'msg-name)))
         (vector-set! *windows-message-handlers* code (decoder generic))
         (add-method generic
           (make <method>
             ':arity 7
             ':procedure (lambda (call-next-method arg0 arg1 arg2 arg3 arg4 arg5 arg6) #f)))
         generic)))
    ((define-windows-message msg-name code (arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7) decoder)
     (define msg-name
       (let ((generic (make <generic>
                        ':arity 8
                        ':combination generic-or-combination
                        ':name 'msg-name)))
         (vector-set! *windows-message-handlers* code (decoder generic))
         (add-method generic
           (make <method>
             ':arity 8
             ':procedure (lambda (call-next-method arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7) #f)))
         generic)))))

(define-windows-message WM_NULL                   #x0000 (hwnd)
  decode-windows-message/no-arguments)
(define-windows-message WM_CREATE                 #x0001 (hwnd create-pointer)
  decode-windows-message/lparam-pointer)
(define-windows-message WM_DESTROY                #x0002 (hwnd)
  decode-windows-message/no-arguments)
(define-windows-message WM_MOVE                   #x0003 (hwnd xpos ypos)
  decode-windows-message/lparam-xy)
; ;
; (define-windows-message WM_SIZE                   #x0005 (hwnd ) decode-windows-message/size)
; (define-windows-message WM_ACTIVATE               #x0006 (hwnd ) decode-windows-message/activate)
; (define-windows-message WM_SETFOCUS               #x0007 (hwnd ) decode-windows-message/wparam-handle)
; (define-windows-message WM_KILLFOCUS              #x0008 (hwnd ) decode-windows-message/wparam-handle)
; ;
; (define-windows-message WM_ENABLE                 #x000A (hwnd ) decode-windows-message/wparam-bool)
; (define-windows-message WM_SETREDRAW              #x000B (hwnd ) decode-windows-message/wparam-bool)
; (define-windows-message WM_SETTEXT                #x000C (hwnd ) decode-windows-message/lparam-string)
; (define-windows-message WM_GETTEXT                #x000D (hwnd ) decode-windows-message/gettext)
(define-windows-message WM_GETTEXTLENGTH          #x000E (hwnd)
  decode-windows-message/no-arguments)
(define-windows-message WM_PAINT                  #x000F (hwnd)
  decode-windows-message/no-arguments)
(define-windows-message WM_CLOSE                  #x0010 (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_QUERYENDSESSION        #x0011 (hwnd ) decode-windows-message/query-end-session)
; ;
(define-windows-message WM_QUERYOPEN              #x0013 (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_QUIT                   #x0012 (hwnd ) decode-windows-message/wparam-int)
; (define-windows-message WM_ERASEBKGND             #x0014 (hwnd ) decode-windows-message/wparam-handle)
(define-windows-message WM_SYSCOLORCHANGE         #x0015 (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_ENDSESSION             #x0016 (hwnd ) decode-windows-message/end-session)
; ;
; (define-windows-message WM_SHOWWINDOW             #x0018 (hwnd ) decode-windows-message/show-window)
; ;
; (define-windows-message WM_WININICHANGE           #x001A (hwnd ) decode-windows-message/lparam-string)
; (define-windows-message WM_DEVMODECHANGE          #x001B (hwnd ) decode-windows-message/lparam-string)
; (define-windows-message WM_ACTIVATEAPP            #x001C (hwnd ) decode-windows-message/activate-app)
(define-windows-message WM_FONTCHANGE             #x001D (hwnd)
  decode-windows-message/no-arguments)
(define-windows-message WM_TIMECHANGE             #x001E (hwnd)
  decode-windows-message/no-arguments)
(define-windows-message WM_CANCELMODE             #x001F (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_SETCURSOR              #x0020 (hwnd ) decode-windows-message/set-cursor)
; (define-windows-message WM_MOUSEACTIVATE          #x0021 (hwnd ) decode-windows-message/mouse-hit)
(define-windows-message WM_CHILDACTIVATE          #x0022 (hwnd)
  decode-windows-message/no-arguments)
(define-windows-message WM_QUEUESYNC              #x0023 (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_GETMINMAXINFO          #x0024 (hwnd maxinfo-pointer)
;   decode-windows-message/lparam-pointer)
; ;
(define-windows-message WM_PAINTICON              #x0026 (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_ICONERASEBKGND         #x0027 (hwnd ) decode-windows-message/wparam-handle)
; (define-windows-message WM_NEXTDLGCTL             #x0028 (hwnd control)
;   decode-windows-message/nextdlgctl)
; ;
; (define-windows-message WM_SPOOLERSTATUS          #x002A (hwnd status remaining)
;   decode-windows-message/spoolerstatus)
; (define-windows-message WM_DRAWITEM               #x002B (hwnd control-id drawitem-pointer)
;   decode-windows-message/drawitem)
; (define-windows-message WM_MEASUREITEM            #x002C (hwnd item-id measureitem-pointer)
;   decode-windows-message/measureitem)
; (define-windows-message WM_DELETEITEM             #x002D (hwnd control-id deleteitem-pointer)
;   decode-windows-message/deleteitem)
; (define-windows-message WM_VKEYTOITEM             #x002E (hwnd keycode cursorpos handle)
;   decode-windows-message/keytoitem)
; (define-windows-message WM_CHARTOITEM             #x002F (hwnd keycode cursorpos handle)
;   decode-windows-message/keytoitem)
; (define-windows-message WM_SETFONT                #x0030 (hwnd ) define-windows-message/setfont)
(define-windows-message WM_GETFONT                #x0031 (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_SETHOTKEY              #x0032 (hwnd keycode modifier)
;   decode-windows-message/sethotkey)
(define-windows-message WM_GETHOTKEY              #x0033 (hwnd)
  decode-windows-message/no-arguments)
; ;
; ;
; ;
(define-windows-message WM_QUERYDRAGICON          #x0037 (hwnd)
  decode-windows-message/no-arguments)
; ;
; (define-windows-message WM_COMPAREITEM            #x0039 (hwnd control compareitem-pointer)
;   decode-windows-message/compareitem)
; ;
; ;
; ;
; (define-windows-message WM_GETOBJECT              #x003D (hwnd flags object-id)
;   decode-windows-message/getobject)
; ;
; ;
; ;
; (define-windows-message WM_COMPACTING             #x0041 (hwnd ) decode-windows-message/wparam-ratio)
; ;
; ;
; ;
; ; WM_COMMNOTIFY             #x0044  /* no longer suported */
; ;
; (define-windows-message WM_WINDOWPOSCHANGING      #x0046 (hwnd position-pointer)
;   decode-windows-message/lparam-pointer)
; (define-windows-message WM_WINDOWPOSCHANGED       #x0047 (hwnd position-pointer)
;   decode-windows-message/lparam-pointer)
; (define-windows-message WM_POWER                  #x0048 (hwnd power-event)
;   decode-windows-message/wparam-power)
; ;
; (define-windows-message WM_COPYDATA               #x004A (hwnd sender-handle data-pointer)
;   decode-windows-message/copydata)
(define-windows-message WM_CANCELJOURNAL          #x004B (hwnd)
  decode-windows-message/no-arguments)
; ;
; ;
; (define-windows-message WM_NOTIFY                 #x004E (hwnd control-id notification-pointer)
;   decode-windows-message/notify)
; ;
; ;; special (define-windows-message WM_INPUTLANGCHANGEREQUEST #x0050 () )
; (define-windows-message WM_INPUTLANGCHANGE        #x0051 (hwnd ) decode-windows-message/input-language)
; (define-windows-message WM_TCARD                  #x0052 (hwnd action info)
;   decode-windows-message/tcard)
; (define-windows-message WM_HELP                   #x0053 (hwnd helpinfo-pointer)
;   decode-windows-message/lparam-pointer)
(define-windows-message WM_USERCHANGED            #x0054 (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_NOTIFYFORMAT           #x0055 (hwnd sending-handle requery?)
;   decode-windows-message/notify-format)
;
; ;;
; (define-windows-message WM_CONTEXTMENU            #x007B (hwnd ) )
; (define-windows-message WM_STYLECHANGING          #x007C (hwnd ) decode-windows-message/style-change)
; (define-windows-message WM_STYLECHANGED           #x007D (hwnd ) decode-windows-message/style-change)
; (define-windows-message WM_DISPLAYCHANGE          #x007E (hwnd ) decode-windows-message/display-change)
; ;; special (define-windows-message WM_GETICON                #x007F () )
; (define-windows-message WM_SETICON                #x0080 (hwnd ) decode-windows-message/set-icon)
; (define-windows-message WM_NCCREATE               #x0081 (hwnd pointer)
;   decode-windows-message/lparam-pointer)
(define-windows-message WM_NCDESTROY              #x0082 (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_NCCALCSIZE             #x0083 (hwnd ) decode-windows-message/nc-calc-size)
; (define-windows-message WM_NCHITTEST              #x0084 (hwnd ) decode-windows-message/mouse-hit)
; (define-windows-message WM_NCPAINT                #x0085 (hwnd ) decode-windows-message/wparam-handle)
; (define-windows-message WM_NCACTIVATE             #x0086 (hwnd ) decode-windows-message/wparam-bool)
; (define-windows-message WM_GETDLGCODE             #x0087 (hwnd msg-pointer)
;   decode-windows-message/lparam-pointer)
(define-windows-message WM_SYNCPAINT              #x0088 (hwnd)
  decode-windows-message/no-arguments)
;
; ;; Non-client mouse
(define-windows-message WM_NCMOUSEMOVE            #x00A0 (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
(define-windows-message WM_NCLBUTTONDOWN          #x00A1 (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
(define-windows-message WM_NCLBUTTONUP            #x00A2 (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
(define-windows-message WM_NCLBUTTONDBLCLK        #x00A3 (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
(define-windows-message WM_NCRBUTTONDOWN          #x00A4 (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
(define-windows-message WM_NCRBUTTONUP            #x00A5 (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
(define-windows-message WM_NCRBUTTONDBLCLK        #x00A6 (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
(define-windows-message WM_NCMBUTTONDOWN          #x00A7 (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
(define-windows-message WM_NCMBUTTONUP            #x00A8 (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
(define-windows-message WM_NCMBUTTONDBLCLK        #x00A9 (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
;
(define-windows-message WM_NCXBUTTONDOWN          #x00AB (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
(define-windows-message WM_NCXBUTTONUP            #x00AC (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
;
(define-windows-message WM_NCXBUTTONDBLCLK        #x00AD (hwnd hit-code xpos ypos) decode-windows-message/ncmouse)
;
; ;; Input keyboard
; (define-windows-message WM_INPUT                  #x00FF (hwnd ) )
(define-windows-message WM_KEYDOWN                #x0100
  (hwnd keycode transition-state previous-state context-code extended? scan-code repeat-count)
  decode-windows-message/key)
(define-windows-message WM_KEYUP                  #x0101
  (hwnd keycode transition-state previous-state context-code extended? scan-code repeat-count)
  decode-windows-message/key)
(define-windows-message WM_CHAR                   #x0102
  (hwnd keycode transition-state previous-state context-code extended? scan-code repeat-count)
  decode-windows-message/key)
(define-windows-message WM_DEADCHAR               #x0103
  (hwnd keycode transition-state previous-state context-code extended? scan-code repeat-count)
  decode-windows-message/key)
(define-windows-message WM_SYSKEYDOWN             #x0104
  (hwnd keycode transition-state previous-state context-code extended? scan-code repeat-count)
  decode-windows-message/key)
(define-windows-message WM_SYSKEYUP               #x0105
  (hwnd keycode transition-state previous-state context-code extended? scan-code repeat-count)
  decode-windows-message/key)
; (define-windows-message WM_SYSCHAR                #x0106 (hwnd ) )
(define-windows-message WM_SYSDEADCHAR            #x0107
  (hwnd keycode transition-state previous-state context-code extended? scan-code repeat-count)
  decode-windows-message/key)
(define-windows-message WM_UNICHAR                #x0109
  (hwnd keycode transition-state previous-state context-code extended? scan-code repeat-count)
  decode-windows-message/key)
; ;
; ;
; ;
; (define-windows-message WM_IME_STARTCOMPOSITION   #x010D (hwnd ) )
; (define-windows-message WM_IME_ENDCOMPOSITION     #x010E (hwnd ) )
; (define-windows-message WM_IME_COMPOSITION        #x010F (hwnd ) )
; (define-windows-message WM_INITDIALOG             #x0110 (hwnd control-handle extra-info)
;   decode-windows-message/initdialog)
; (define-windows-message WM_COMMAND                #x0111 (hwnd ) )
(define-windows-message WM_SYSCOMMAND             #x0112 (hwnd command-type xpos ypos)
  decode-windows-message/syscommand)
; (define-windows-message WM_TIMER                  #x0113 (hwnd ) )
; (define-windows-message WM_HSCROLL                #x0114 (hwnd ) )
; (define-windows-message WM_VSCROLL                #x0115 (hwnd ) )
; (define-windows-message WM_INITMENU               #x0116 (hwnd ) )
; (define-windows-message WM_INITMENUPOPUP          #x0117 (hwnd ) )
;; Undocumented, but well-known message
(define-windows-message WM_SYSTIMER               #x0118 (hwnd)
  decode-windows-message/no-arguments)
; ;
; ;
; ;
; ;
; ;
; ;
; ;
; (define-windows-message WM_MENUSELECT             #x011F (hwnd ) )
; (define-windows-message WM_MENUCHAR               #x0120 (hwnd ) )
; (define-windows-message WM_ENTERIDLE              #x0121 (hwnd idle-reason handle)
;   decode-windows-message/enter-idle)
; (define-windows-message WM_MENURBUTTONUP          #x0122 (hwnd ) )
; (define-windows-message WM_MENUDRAG               #x0123 (hwnd ) )
; (define-windows-message WM_MENUGETOBJECT          #x0124 (hwnd ) )
; (define-windows-message WM_UNINITMENUPOPUP        #x0125 (hwnd ) )
; (define-windows-message WM_MENUCOMMAND            #x0126 (hwnd ) )
; (define-windows-message WM_CHANGEUISTATE          #x0127 (hwnd ) )
; (define-windows-message WM_UPDATEUISTATE          #x0128 (hwnd ) )
; (define-windows-message WM_QUERYUISTATE           #x0129 (hwnd ) )
; ;
; ;
; ;
; ;
; ;
; ;
; ;
; ;
; (define-windows-message WM_CTLCOLORMSGBOX         #x0132 (hwnd ) )
; (define-windows-message WM_CTLCOLOREDIT           #x0133 (hwnd ) )
; (define-windows-message WM_CTLCOLORLISTBOX        #x0134 (hwnd device-context listbox)
;   decode-windows-message/ctlcolorlistbox)
; (define-windows-message WM_CTLCOLORBTN            #x0135 (hwnd ) )
; (define-windows-message WM_CTLCOLORDLG            #x0136 (hwnd device-context dialog-box)
;   decode-windows-message/ctlcolordlg)
; (define-windows-message WM_CTLCOLORSCROLLBAR      #x0137 (hwnd ) )
; (define-windows-message WM_CTLCOLORSTATIC         #x0138 (hwnd ) )
;
; ;; Mouse
(define-windows-message WM_MOUSEMOVE              #x0200 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_LBUTTONDOWN            #x0201 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_LBUTTONUP              #x0202 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_LBUTTONDBLCLK          #x0203 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_RBUTTONDOWN            #x0204 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_RBUTTONUP              #x0205 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_RBUTTONDBLCLK          #x0206 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_MBUTTONDOWN            #x0207 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_MBUTTONUP              #x0208 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_MBUTTONDBLCLK          #x0209 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_MOUSEWHEEL             #x020A (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_XBUTTONDOWN            #x020B (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_XBUTTONUP              #x020C (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_XBUTTONDBLCLK          #x020D (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
; ;
; ;
; (define-windows-message WM_PARENTNOTIFY           #x0210 (hwnd ) decode-windows-message/parent-notify)
; (define-windows-message WM_ENTERMENULOOP          #x0211 (hwnd ) )
; (define-windows-message WM_EXITMENULOOP           #x0212 (hwnd ) )
; (define-windows-message WM_NEXTMENU               #x0213 (hwnd ) )
; (define-windows-message WM_SIZING                 #x0214 (hwnd ) decode-windows-message/sizing)
; (define-windows-message WM_CAPTURECHANGED         #x0215 (hwnd ) decode-windows-message/lparam-handle)
; (define-windows-message WM_MOVING                 #x0216 (hwnd info)
;   decode-windows-message/lparam-pointer)
;
; ;; Hardware oriented
; (define-windows-message WM_POWERBROADCAST         #x0218 (hwnd ) )
(define-windows-message WM_DEVICECHANGE           #x0219 (hwnd devmsg) decode-windows-message/device-change)

;
; ;; MDI
; (define-windows-message WM_MDICREATE              #x0220 (hwnd ) )
; (define-windows-message WM_MDIDESTROY             #x0221 (hwnd ) )
; (define-windows-message WM_MDIACTIVATE            #x0222 (hwnd ) )
; (define-windows-message WM_MDIRESTORE             #x0223 (hwnd ) )
; (define-windows-message WM_MDINEXT                #x0224 (hwnd ) )
; (define-windows-message WM_MDIMAXIMIZE            #x0225 (hwnd ) )
; (define-windows-message WM_MDITILE                #x0226 (hwnd ) )
; (define-windows-message WM_MDICASCADE             #x0227 (hwnd ) )
; (define-windows-message WM_MDIICONARRANGE         #x0228 (hwnd ) )
; (define-windows-message WM_MDIGETACTIVE           #x0229 (hwnd ) )
; (define-windows-message WM_MDISETMENU             #x0230 (hwnd ) )
(define-windows-message WM_ENTERSIZEMOVE          #x0231 (hwnd)
  decode-windows-message/no-arguments)
(define-windows-message WM_EXITSIZEMOVE           #x0232 (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_DROPFILES              #x0233 (hwnd hwnd drophandle)
;   decode-windows-message/wparam-handle)
; (define-windows-message WM_MDIREFRESHMENU         #x0234 (hwnd ) )
;
; ;; IME
; (define-windows-message WM_IME_SETCONTEXT         #x0281 (hwnd ) )
; (define-windows-message WM_IME_NOTIFY             #x0282 (hwnd ) )
; (define-windows-message WM_IME_CONTROL            #x0283 (hwnd ) )
; (define-windows-message WM_IME_COMPOSITIONFULL    #x0284 (hwnd ) )
; (define-windows-message WM_IME_SELECT             #x0285 (hwnd ) )
; (define-windows-message WM_IME_CHAR               #x0286 (hwnd ) )
; ;
; (define-windows-message WM_IME_REQUEST            #x0288 (hwnd ) )
; ;
; ;
; ;
; ;
; ;
; ;
; ;
; (define-windows-message WM_IME_KEYDOWN            #x0290 (hwnd ) )
; (define-windows-message WM_IME_KEYUP              #x0291 (hwnd ) )
;
; ;;
(define-windows-message WM_NCMOUSEHOVER           #x02A0 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_MOUSEHOVER             #x02A1 (hwnd keycode xpos ypos)
  decode-windows-message/mouse)
(define-windows-message WM_NCMOUSELEAVE           #x02A2 (hwnd)
  decode-windows-message/no-arguments)
(define-windows-message WM_MOUSELEAVE             #x02A3 (hwnd)
  decode-windows-message/no-arguments)
;
; ;;
; (define-windows-message WM_WTSSESSION_CHANGE      #x02B1 (hwnd ) )
;
; ;; Tablet group #x02c0 - #x02df
;
; ;; Cut, Copy, and Paste
; (define-windows-message WM_CUT                    #x0300 (hwnd ) )
; (define-windows-message WM_COPY                   #x0301 (hwnd ) )
; (define-windows-message WM_PASTE                  #x0302 (hwnd ) )
; (define-windows-message WM_CLEAR                  #x0303 (hwnd ) )
; (define-windows-message WM_UNDO                   #x0304 (hwnd ) )
; (define-windows-message WM_RENDERFORMAT           #x0305 (hwnd ) )
; (define-windows-message WM_RENDERALLFORMATS       #x0306 (hwnd ) )
; (define-windows-message WM_DESTROYCLIPBOARD       #x0307 (hwnd ) )
; (define-windows-message WM_DRAWCLIPBOARD          #x0308 (hwnd ) )
; (define-windows-message WM_PAINTCLIPBOARD         #x0309 (hwnd ) )
; (define-windows-message WM_VSCROLLCLIPBOARD       #x030A (hwnd ) )
; (define-windows-message WM_SIZECLIPBOARD          #x030B (hwnd ) )
; (define-windows-message WM_ASKCBFORMATNAME        #x030C (hwnd ) )
; (define-windows-message WM_CHANGECBCHAIN          #x030D (hwnd ) )
; (define-windows-message WM_HSCROLLCLIPBOARD       #x030E (hwnd ) )
(define-windows-message WM_QUERYNEWPALETTE        #x030F (hwnd)
  decode-windows-message/no-arguments)
; (define-windows-message WM_PALETTEISCHANGING      #x0310 (hwnd ) decode-windows-message/wparam-handle)
; (define-windows-message WM_PALETTECHANGED         #x0311 (hwnd ) decode-windows-message/wparam-handle)
; (define-windows-message WM_HOTKEY                 #x0312 (hwnd ) decode-windows-message/hotkey)
; ;
; ;
; ;
; ;
; (define-windows-message WM_PRINT                  #x0317 (hwnd ) decode-windows-message/print)
; (define-windows-message WM_PRINTCLIENT            #x0318 (hwnd ) decode-windows-message/print)
; (define-windows-message WM_APPCOMMAND             #x0319 (hwnd ) decode-windows-message/app-command)
(define-windows-message WM_THEMECHANGED           #x031A (hwnd)
  decode-windows-message/no-arguments)
;
; ;; Handheld group #x0358 - #x035F
; (define-windows-message WM_HANDHELDFIRST          #x0358 (hwnd ) )
; (define-windows-message WM_HANDHELDLAST           #x035F (hwnd ) )
;
; ;; AFX group #x0360 - #x037F
;
; ;; Pen group #x0380 - #x038F
;
;;; This procedure is intendend to be hooked into the windows forms
;;; message pump at the lowest level.  It gets the first crack at
;;; handling a message.  Since *every* message comes through here, we
;;; need this to be as efficient as possible.


(define standard-message-filter
  (clr-object->clr-instance
   (clr/%procedure->message-filter standard-message-dispatch)))


;;; Low-Level message pump code ends here

;;; WARNING WARNING WARNING
;;; The code above this line is very magic.  See the top of the file for details.
;;; The sample code below is an example of using JavaDot notation to
;;; talk to Windows forms.

;;; SAMPLE CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample code.  Creates a window and hooks the mouse-leave events.

(define (window-demo)
  (let* ((form (System.Windows.Forms.form.))
         (context (System.Windows.Forms.ApplicationContext. form)))
    (System.Windows.Forms.Application.AddMessageFilter standard-message-filter)
    (System.Windows.Forms.Application.Run context)
    (System.Windows.Forms.Application.RemoveMessageFilter standard-message-filter)))

;; For typos
(define windows-demo window-demo)

(define (preload)
  (system-windows-forms-assembly)
  #f)

(preload)

;; Add a method to the wm_mouseleave and wm_ncmouseleave generics.
;; This will be called whenever those messages are posted.

(define *exit-count* 0)

(let ((method
       (make <method>
         :arity 1
         :procedure (lambda (call-next-method hwnd)
                       (newline)
                       (display "Elvis has left the building. ")
                       (display *exit-count*)
                       (set! *exit-count* (+ *exit-count* 1))
                       #f))))
  (add-method WM_MOUSELEAVE method)
  (add-method WM_NCMOUSELEAVE method))

