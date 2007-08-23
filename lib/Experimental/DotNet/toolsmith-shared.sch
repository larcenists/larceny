;; :Basic:make-trap.sch is Mac specific
;; :Basic:mac-type is Mac specific 
;; :Basic:general is FFI stuff that we can do ourselves
;; :Basic:blt is also very lowlevel FFI stuff 
;; :Basic:event event handling protocols; want to do this ourselves
;; :Basic:tasks interrupt & event based multi-tasker
;; :Standard:data&traps seems mostly mac specific, but it might be
;;    good to know what state it defines...
;;    - *also*, at least some of the functions in here are directly
;;      used by user agents (at least in WillsEditor;
;;      e.g. invertrect), and therefore it might represent (a superset
;;      of?) basic graphics functionality that all need to support.
;;    - *also*, watch out for case [in]sensitivity issues; code may
;;      reference TEInsert which is defined as teinsert.
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
;; ((menu 'append) item action)            ; item : String, action : () -> ()
;; ((menu 'append) item action enableproc) ; enableproc () -> Bool
;;   Special chars in item: #\- #\/ #\< #\! #\^
;; ((menu 'addresources) rtype raction)
;; ((menu 'update))
;;   enables and disables all its menuitems according to the enableproc's
;; ((menu 'selectitem) itemnumber)
;; (pushmenubar)
;; (popmenubar)
;; ((menu 'id))
;; ((menu 'menuhandle))
;; (make-window) (make-window 'make-agent make-window-agent 'text 'bounds (left top right bottom) 'title string 'nogoaway 'nosizebox)
;;    (make-window 'text) is equivalent to (make-window 'make-agent make-editor)
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
;;   to set the insertion point, use equiv start and end.
;;   (really?  why overload this way?)
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
;;   scrolls text vertically so that a particular position in text is
;;   visible
;; ((scroller 'set) editor)
;;   sets scroll bar indicators so that they indicate correct position
;;   of currently displayed text

(define-syntax msg-handler
  (syntax-rules ()
    ((msg-handler ((OP-NAME . ARGS) BODY ...) ...)
     (lambda (op)
       ;; (display `(handling msg ,op)) (newline)
       (case op
         ((OP-NAME) (lambda ARGS BODY ...))
         ...
         ((operations) (lambda () '(OP-NAME ... operations)))
         (else (error 'msg-handler 
                      ": unhandled object message " op)))))))

;; An agent can choose whether or not it handles the paint event (by
;; including paint in its operations list).
;; If it does handle paint, then the window will dynamically dispatch
;; to the agent every time it needs to be painted.
;; If it does not handle paint, then the window will keep image state
;; to be rendered and the agent will imperatively modify that.

;; Note that all agent operations are optional, except (of course) the
;; 'operations operation.
;; 
;; AGENT: (make-noisy-agent) ; client def's ctors, perhaps via msg-handler form
;;  (on-close wnd) 
;;  (on-keypress wnd char) (on-keydown wnd sym mods) (on-keyup sym mods)
;;  (on-mousedown x y) (on-mouseup x y) (on-mousemove x y) 
;;  (on-mouseclick x y) (on-mousedoubleclick x y)
;;  (on-mouseenter) (on-mouseleave)
;;  (on-paint gfx x y w h) (dispose)
;;  (vertical-scrollbar) (horizontal-scrollbar) ; scrollbar exposure+properties
;;  (on-vscroll wnd new-val event-type)
;;  (on-hscroll wnd new-val event-type)
;;  (on-resize wnd)

;; Agents are client written entities; the objects below are provided
;; by the runtime system.

;; FNT: (make-fnt name em-size) (available-fontnames) (monospace-fontname) (sans-serif-fontname) (serif-fontname)
;;  (clone ['italic] ['bold] ['underline] ['em-size r])
;;  (name) (em-size) (italic?) (bold?) (underline?) (fntptr)
;; GFX: ;; no public constructors (received via agent's on-paint op)
;;  (measure-text txt-string font-obj) 
;;  (draw-text txt-string font-obj x y col) 
;;  (draw-line col x1 y1 x2 y2) (draw-image img-obj x y) 
;;  (draw-rect col x1 y1 x2 y2) (gfxptr)
;; COL: (make-col alpha red green blue) (name->col name) (available-colornames)
;;  (name) (alpha) (red) (green) (blue) (colptr)
;; WND: (make-wnd ['make-agent agent-ctor] 
;;                ['bounds (x y w h)] 
;;                ['double-buffered] 
;;                ['title string])
;;  (show) (show-dialog) (title) (close) (dispose) (update)
;;  (width) (height)
;;  (push-menus mnu ...) (pop-menus)
;; MNU: (make-mnu name)
;;  (append item action) ;; item is string or mnu; action is nullary procedure
;;  (append item action enabled?) ;; enabled? is nullary predicate
;;  (items) (mnuptr) (name)

(define (make-noisy-agent width height)
  (define (displayln x) (display x) (newline))
  (msg-handler
   ((on-close wnd)                (displayln `(on-close)))
   ((on-keydown wnd int mods)     (displayln `(on-keydown ,int ,mods)))
   ((on-keyup wnd int mods)       (displayln `(on-keyup ,int ,mods)))
   ((on-keypress wnd char)        (displayln `(on-keypress ,char)))
   ((on-mousedown wnd x y)        (displayln `(on-mousedown ,x ,y)))
   ((on-mouseup   wnd x y)        (displayln `(on-mouseup ,x ,y)))
   ((on-mousemove wnd x y)        (displayln `(on-mousemove ,x ,y)))
   ((on-mouseclick wnd x y)       (displayln `(on-mouseclick ,x ,y)))
   ((on-mousedoubleclick wnd x y) (displayln `(on-mousedoubleclick ,x ,y)))
   ((on-mouseenter wnd)           (displayln `(on-mouseenter)))
   ((on-mouseleave wnd)           (displayln `(on-mouseleave)))
   ((on-paint wnd g x y w h)      (displayln `(on-paint ,g ,x ,y ,w ,h)))
   ((on-dispose wnd)              (displayln `(on-dispose)))
   ))

(define (make-rectangle-drawing-agent width height)
  (let* ((last-x #f)
         (last-y #f)
         (temp-rect #f)
         (save! (lambda (x y) (set! last-x x) (set! last-y y)))
         (temp! (lambda (x y) 
                  (cond ((and last-x last-y)
                         (set! temp-rect (list last-x last-y x y))))))
         (forget! (lambda () 
                    (set! last-x #f) (set! last-y #f) (set! temp-rect #f)))
         (rect-list '()))
    (msg-handler 
     ((on-mousedown wnd x y) (save! x y))
     ((on-mousemove wnd x y) 
      (cond ((and last-x last-y)
             (temp! x y) ((wnd 'update)))))
     ((on-mouseup wnd x y) 
      (cond ((and last-x last-y)
             (set! rect-list 
                   (cons (list last-x last-y x y) rect-list))
             (forget!)
             ((wnd 'update))
             )))
     ((on-mouseleave wnd) (forget!) ((wnd 'update)))
     ((on-paint wnd g x y w h)
      (for-each
       (lambda (rect) (apply (g 'draw-rect) (name->col "Black") rect))
       rect-list)
      ;;(begin (display `(on-paint temp-rect: ,temp-rect)) (newline))
      (cond (temp-rect (apply (g 'draw-rect) (name->col "Red") temp-rect)))
      ))))

(define (make-code-editor-agent width height)
  ;; XXX don't spend too much time writing code oriented around this
  ;; representation of the text contents; we would be better off
  ;; adopting something like Will's buffer abstraction (see text.sch
  ;; in his editor code)
  (let* ((rlines-before-view '("line B" "line A"))
         (rlines-before-cursor '("line D" "line C"))
         (lines-after  '("line Y" "line Z"))
         (prefix '())
         (suffix '())
         (em-size 10)
         (fnt (make-fnt (monospace-fontname) em-size))
         (col (name->col (string->symbol "Black"))))

    (define (take lst n)
      (cond ((zero? n) '())
            ((null? lst) '())
            (else (cons (car lst) 
                        (take (cdr lst) (- n 1))))))
    (define (drop lst n)
      (cond ((zero? n) lst)
            ((null? lst) '())
            (else (drop (cdr lst) (- n 1)))))

    (define (max-visible-lines)
      (quotient height em-size))

    (define (count-visible-lines)
      (min (+ (length rlines-before-cursor)
              1
              (length lines-after))
           (max-visible-lines)))

    (define (count-lines)
      (+ (length rlines-before-view)
         (length rlines-before-cursor)
         1
         (length lines-after)))

    (define (cursor-line)
      (length rlines-before-cursor))


    (msg-handler
     ((text) 
      (apply string-append
             (append (reverse rlines-before-view)
                     (reverse rlines-before-cursor)
                     (list (list->string (append (reverse prefix) suffix)))
                     lines-after)))
     ((on-keydown wnd sym mods)
      '(begin (write `(keydown ,((wnd 'title)) ,sym ,mods)) (newline))
      )
     ((on-keyup   wnd sym mods)
      '(begin (write `(keyup ,((wnd 'title)) ,sym ,mods)) (newline))
      (case sym
        ((back delete) 
         (cond ((not (null? prefix))
                (set! prefix (cdr prefix)))
               ((not (null? rlines-before-cursor))
                (set! prefix (reverse (string->list 
                                       (car rlines-before-cursor))))
                (set! rlines-before-cursor (cdr rlines-before-cursor)))
               ((null? rlines-before-view)
                'do-nothing)
               (else
                ((wnd 'scroll) 'vertical -1)
                (assert (not (null? rlines-before-cursor)))
                (set! prefix (reverse (string->list 
                                       (car rlines-before-cursor))))
                (set! rlines-before-cursor (cdr rlines-before-cursor)))))
        ((enter) 
         (set! rlines-before-cursor
               (cons (list->string (reverse prefix)) rlines-before-cursor))
         (set! prefix '()))
        ((up)
         (let ((move-up! 
                (lambda ()
                  (assert (not (null? rlines-before-cursor)))
                  (let ((i (length prefix)))
                    (set! lines-after 
                          (cons (list->string (append (reverse prefix) suffix))
                                lines-after))
                    (set! prefix
                          (reverse 
                           (take (string->list (car rlines-before-cursor)) i)))
                    (set! suffix 
                          (drop (string->list (car rlines-before-cursor)) i))
                    (set! rlines-before-cursor (cdr rlines-before-cursor))))))
           (cond ((not (null? rlines-before-cursor)) 
                  (move-up!))
                 ((null? rlines-before-view)         
                  'do-nothing)
                 (else
                  ((wnd 'scroll) 'vertical -1)
                  (move-up!)))))
        ((down)
         (cond ((null? lines-after) 'do-nothing)
               (else
                (let ((i (length prefix)))
                  (set! rlines-before-cursor
                        (cons (list->string (append (reverse prefix) suffix))
                              rlines-before-cursor))
                  (set! prefix (reverse (take (string->list (car lines-after)) i)))
                  (set! suffix (drop (string->list (car lines-after)) i))
                  (set! lines-after (cdr lines-after))
                  (cond ((> (+ (length rlines-before-cursor) 1)
                            (count-visible-lines))
                         ((wnd 'scroll) 'vertical 1)))))))
        ((left) 
         (let ((move-left! 
                (lambda ()
                  (assert (not (null? rlines-before-cursor)))
                  (set! lines-after
                        (cons (list->string (append (reverse prefix) suffix))
                              lines-after))
                  (set! suffix '())
                  (set! prefix (reverse (string->list
                                         (car rlines-before-cursor))))
                  (set! rlines-before-cursor (cdr rlines-before-cursor)))))
           (cond ((not (null? prefix))
                  (set! suffix (cons (car prefix) suffix))
                  (set! prefix (cdr prefix)))
                 ((not (null? rlines-before-cursor))
                  (move-left!))
                 ((not (null? rlines-before-view))
                  ((wnd 'scroll) 'vertical 1)
                  (move-left!)))))
        ((right)
         (cond ((not (null? suffix))
                (set! prefix (cons (car suffix) prefix))
                (set! suffix (cdr suffix)))
               ((not (null? lines-after))
                (set! rlines-before-cursor
                      (cons (list->string (append (reverse prefix) suffix))
                            rlines-before-cursor))
                (set! prefix '())
                (set! suffix (string->list (car lines-after)))
                (set! lines-after (cdr lines-after))
                (cond ((> (+ (length rlines-before-cursor) 1)
                          (count-visible-lines))
                       ((wnd 'scroll) 'vertical 1))))
               (else
                'do-nothing)))
        (else 
         (begin (write `(keyup ,((wnd 'title)) ,sym ,mods)) (newline)))
        )
      ((wnd 'update)))
     ((on-resize wnd)
      (set! width  ((wnd 'width)))
      (set! height ((wnd 'height))))
     ((on-keypress wnd char) 
      (cond ((or (char-alphabetic? char)
                 (char-numeric? char))
             (set! prefix (cons char prefix)))
            (else
             (case char
               ((#\backspace #\return #\esc #\tab) 'do-nothing)
               (else
                '(begin (write `(on-keypress wnd ,char)) (newline))
                (set! prefix (cons char prefix))))))
      ((wnd 'update)))
     ((horizontal-scrollbar) #f)
     ((vertical-scrollbar)
      ;; my hope is that these do not have to be in pixels to be meaningful
      `((min ,0) 
        (max ,(count-lines))
        (value ,(length rlines-before-view))
        (dsmall ,1)
        (dlarge ,(quotient (max-visible-lines) 2))))
     ((on-hscroll wnd new-int event-type)
      (begin (write `(on-hscroll wnd ,new-int ,event-type))
             (newline)))
     ((on-vscroll wnd new-int event-type)
      (begin (write `(on-vscroll wnd ,new-int ,event-type))
             (newline))
      (let ((lines-before (reverse (append rlines-before-cursor
                                           rlines-before-view))))
        (set! rlines-before-cursor (reverse (drop lines-before new-int)))
        (set! rlines-before-view (reverse (take lines-before new-int))))
      ((wnd 'update)))
     ((on-paint wnd g rx ry rw rh)
      '(begin (write `(on-paint wnd g ,rx ,ry ,rw ,rh))
              (newline))
      (let* ((measure-height
              (lambda (s) 
                (call-with-values (lambda () ((g 'measure-text) s fnt))
                  (lambda (w h) h))))
             (h (let loop ((h 0) (lines (reverse rlines-before-cursor)))
                  (cond ((not (null? lines))
                         ((g 'draw-text) (car lines) fnt 0 h col)
                         (loop (+ h (measure-height (car lines))) (cdr lines)))
                        (else h))))
             (pre (list->string (reverse prefix)))
             (suf (list->string suffix))
             (h (call-with-values (lambda () ((g 'measure-text) pre fnt))
                  (lambda (pw ph) 
                    ((g 'draw-text) pre fnt 0  h col)
                    ((g 'draw-line) col     pw h pw (+ h ph))
                    ((g 'draw-text) suf fnt pw h col)
                    (+ h ph)))))
        (let loop ((h h) (lines lines-after))
          (cond ((not (null? lines))
                 ((g 'draw-text) (car lines) fnt 0 h col)
                 (loop (+ h (measure-height (car lines))) (cdr lines)))))))
     )))

    
