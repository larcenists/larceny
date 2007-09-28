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

;; Following code requires use of ProtoObj object system.

;; An agent can choose whether or not it handles the paint event (by
;; including paint in its operations list).
;; If it does handle paint, then the window will dynamically dispatch
;; to the agent every time it needs to be painted.
;; If it does not handle paint, then the window will keep image state
;; to be rendered and the agent will imperatively modify that.

;; Note that all agent operations are optional, except (of course) the
;; 'operations operation.
;; 
;; AGENT: (make-noisy-agent wnd width height) ; client def's ctors, perhaps via msg-handler form
;;  (on-close) 
;;  (on-keypress char) (on-keydown mchar sym mods) (on-keyup mchar sym mods)
;;  (on-mousedown x y) (on-mouseup x y) (on-mousemove x y) (on-mousedrag x y)
;;  (on-mouseclick x y) (on-mousedoubleclick x y)
;;  (on-mouseenter) (on-mouseleave)
;;  (on-paint gfx x y w h) (dispose)
;;  (vertical-scrollbar) (horizontal-scrollbar) ; scrollbar exposure+properties
;;  (on-vscroll new-val event-type)
;;  (on-hscroll new-val event-type)
;;  (on-resize)

;; Agents are client written entities; the objects below are provided
;; by the runtime system.

;; FNT: (make-fnt name em-size) (available-fontnames) (monospace-fontname) (sans-serif-fontname) (serif-fontname)
;;  (clone ['italic] ['bold] ['underline] ['em-size r])
;;  (name) (em-size) (italic?) (bold?) (underline?) (fntptr)
;; GFX: ;; no public constructors (received via agent's on-paint op)
;;  (measure-text txt-string font-obj) 
;;  (draw-text txt-string font-obj x y col) 
;;  (draw-line col x1 y1 x2 y2) (draw-image img-obj x y) 
;;  (draw-rect col x1 y1 x2 y2) 
;;  (fill-rect col x1 y1 x2 y2) 
;;  (gfxptr)
;; COL: (make-col alpha red green blue) (name->col name) (available-colornames)
;;  (name) (alpha) (red) (green) (blue) (colptr)
;; WND: (make-wnd ['make-agent agent-ctor] 
;;                ['bounds (x y w h)] 
;;                ['double-buffered] 
;;                ['title string])
;;  (show) (hide) (show-dialog) (title) (close) (dispose) (update)
;;  (width) (height)
;;  (measure-text txt-string font-obj) 
;;  (push-menus mnu ...) (pop-menus)
;; MNU: (make-mnu name)
;;  (append item action) ;; item is string or mnu; action is nullary procedure
;;  (append item action enabled?) ;; enabled? is nullary predicate
;;  (items) (mnuptr) (name)

(define (make-noisy-agent wnd width height)
  (define (displayln x) (write x) (newline))
  (make-root-object noisy-agent
   ((on-close)                  (displayln `(on-close)))
   ((on-keydown mchar sym mods) (displayln `(on-keydown ,mchar ,sym ,mods)))
   ((on-keyup mchar sym mods)   (displayln `(on-keyup ,mchar ,sym ,mods)))
   ((on-keypress char)          (displayln `(on-keypress ,char)))
   ((on-mousedown x y)          (displayln `(on-mousedown ,x ,y)))
   ((on-mouseup   x y)          (displayln `(on-mouseup ,x ,y)))
   ((on-mousemove x y)          (displayln `(on-mousemove ,x ,y)))
   ((on-mouseclick x y)         (displayln `(on-mouseclick ,x ,y)))
   ((on-mousedoubleclick x y)   (displayln `(on-mousedoubleclick ,x ,y)))
   ((on-mouseenter)             (displayln `(on-mouseenter)))
   ((on-mouseleave)             (displayln `(on-mouseleave)))
   ((on-paint g x y w h)        (displayln `(on-paint ,g ,x ,y ,w ,h)))
   ((on-dispose)                (displayln `(on-dispose)))
   ))

(define (make-rectangle-drawing-agent wnd width height)
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
    (make-root-object rectangle-drawing-agent
     ((on-mousedown x y) (save! x y))
     ((on-mousedrag x y) 
      (cond ((and last-x last-y)
             (temp! x y) ((wnd 'update)))))
     ((on-mouseup x y) 
      (cond ((and last-x last-y)
             (set! rect-list 
                   (cons (list last-x last-y x y) rect-list))
             (forget!)
             ((wnd 'update))
             )))
     ((on-mouseleave) (forget!) ((wnd 'update)))
     ((on-paint g x y w h)
      (for-each
       (lambda (rect) (apply (g 'draw-rect) (name->col "Black") rect))
       rect-list)
      ;;(begin (display `(on-paint temp-rect: ,temp-rect)) (newline))
      (cond (temp-rect (apply (g 'draw-rect) (name->col "Red") temp-rect)))
      ))))

(define (invert-col col)
  (define (inv num) (- 255 num))
  (make-col ((col 'alpha))
            (inv ((col 'red))) 
            (inv ((col 'green)))
            (inv ((col 'blue)))))

(define (make-textview-agent wnd width height)
  ;; Invariant: mytext must always end with a #\newline character.
  (define mytext "\n") ;; revist when we add IMG objects.
  (define selection 0) ;; [Oneof Nat (cons Nat Nat)]
  (define preferred-cursor-col #f) ;; used when moving cursor up and down
  (define (cursor-line)
    (let ((pos (cond ((number? selection) selection)
                     (else (car selection)))))
      (do ((i 0 (+ i 1))
           (j 0 (+ j (if (char=? #\newline (string-ref mytext i)) 1 0))))
          ((= i pos)
           j))))
                    
  (define mouse-down #f)
  (define mouse-up #f)
  (define mouse-drag #f)
  (define em-size 10)
  (define fnt (make-fnt (monospace-fontname) em-size))
  (define selection-col (name->col (string->symbol "LightBlue")))
  
  ;; A ColRange is a (list [Maybe Pos] [Maybe Pos] Col)
  ;; interpretation:
  ;; ( m  n c) maps the number range [m, n) to c
  ;; (#f  n c) maps the number range < n to c
  ;; ( m #f c) maps the number range >= m to c  
  
  ;; Pos ColRange -> [Maybe Col]
  (define (lookup-col pos col-ranges)
    (let loop ((ranges col-ranges))
      (cond
       ((null? ranges) #f)
       (else 
        (let ((range (car ranges)))
          (cond ((and (or (not (car range)) (<= (car range) pos))
                      (or (not (cadr range)) (< pos (cadr range))))
                 (caddr range))
                (else
                 (loop (cdr ranges)))))))))

  ;; [Listof ColRange]
  (define stable-foreground-col-ranges '())
  (define stable-background-col-ranges '())
  (define transient-foreground-col-ranges '())
  (define transient-background-col-ranges '())
  (define selection-foreground-col-ranges 
    (list
     (list #f #f (name->col (string->symbol "Black")))))
  (define selection-background-col-ranges
    (list 
     (list #f #f (name->col (string->symbol "LightBlue")))))
  
  (define default-foreground-col (name->col (string->symbol "Black")))
  (define default-background-col (name->col (string->symbol "White")))
  (define (default-selection-foreground-col) (invert-col default-foreground-col))
  (define (default-selection-background-col) (invert-col default-background-col))

  (define (clear-transient-state!) 
    (set! transient-background-col-ranges '())
    (set! transient-foreground-col-ranges '()))
  (define (clear-stable-state!)
    (set! stable-background-col-ranges '())
    (set! stable-foreground-col-ranges '()))
  (define (update-stable-ranges! change-start-incl change-finis-excl remain-delta)
    (define (shift-entry start finis col)
      (list (+ start remain-delta)
            (+ finis remain-delta)
            col))
    (define (updated ranges)
      (let loop ((ranges ranges))
        (cond
         ((null? ranges) '())
         (else
          (let* ((entry (car ranges))
                 (start-incl (car entry))
                 (finis-excl (cadr entry))
                 (col   (caddr entry)))
            (cond ((<= finis-excl change-start-incl) 
                   ;; Leave early entries unchanged
                   (cons entry (loop (cdr ranges))))
                  ((and (< start-incl change-finis-excl)
                        (< change-start-incl finis-excl))
                   ;; Drop overlapping entries
                   (loop (cdr ranges)))
                  ((<= change-finis-excl start-incl)
                   ;; Shift late entries
                   (cons (shift-entry start-incl finis-excl col)
                         (loop (cdr ranges))))
                  (else
                   (error 'update-stable-ranges!
                          ": this should be dead code."))))))))
    (set! stable-background-col-ranges (updated stable-background-col-ranges))
    (set! stable-foreground-col-ranges (updated stable-foreground-col-ranges)))
  
  (define backing-agent #f)
  (define (count-visible-lines)
    (inexact->exact
     (ceiling (/ ((wnd 'height)) 
                 (call-with-values (lambda () ((wnd 'measure-text) "" fnt))
                   (lambda (w h) h))))))
    
  (define (cursor-left!)
    (clear-transient-state!)
    (set! preferred-cursor-col #f)
    (cond ((number? selection) 
           (set! selection (max 0 (- selection 1))))
          (else
           (set! selection (car selection)))))
  (define (cursor-right!)
    (clear-transient-state!)
    (set! preferred-cursor-col #f)
    (cond ((number? selection) 
           (set! selection (min (string-length mytext) (+ selection 1))))
          (else 
           (set! selection (cdr selection)))))
  (define (cursor-vertical! dir)
    (define pos (cond ((number? selection) selection)
                      (else (car selection))))
    (clear-transient-state!)
    (let ((start-of-line-idx
           (do ((idx (- pos 1) (- idx 1)))
               ((or (< idx 0) (char=? #\newline (string-ref mytext idx)))
                (+ idx 1)))))
      (cond ((not preferred-cursor-col)
             (set! preferred-cursor-col (- pos start-of-line-idx))))
      (let* ((start-of-target-line 
              (case dir
                ((backward)
                 (do ((idx (max -1 (- start-of-line-idx 2)) (- idx 1)))
                     ((or (< idx 0) (char=? #\newline (string-ref mytext idx)))
                      (+ idx 1))))
                ((forward)
                 (let loop ((idx (+ pos 1)))
                   (cond 
                    ((>= idx (string-length mytext))
                     ;; We reached the end of mytext without passing over
                     ;; a newline; therefore pos is on the final line.
                     ;; To preserve no-op semantics, return start of that line
                     start-of-line-idx)
                    ((char=? #\newline (string-ref mytext (- idx 1)))
                     idx)
                    (else
                     (loop (+ idx 1))))))))
             (target-idx
              (do ((idx start-of-target-line (+ idx 1))
                   (col preferred-cursor-col (- col 1)))
                  ((or (= col 0) 
                       (char=? #\newline (string-ref mytext idx)))
                   idx))))
        (set! selection target-idx))))
  (define (cursor-up!)
    (clear-transient-state!)
    (if (= (cursor-line) 0)
        ((wnd 'attempt-scroll) 'vertical -1))
    (cursor-vertical! 'backward))
  (define (cursor-down!)
    (clear-transient-state!)
    (if (= (cursor-line) (- (count-visible-lines) 1))
        ((wnd 'attempt-scroll) 'vertical  1))
    (cursor-vertical! 'forward))
  (define (selection-start-pos)
    (cond ((number? selection) selection)
          (else (car selection))))
  (define (selection-finis-pos)
    (cond ((number? selection) (+ selection 1))
          (else (cdr selection))))
  (define (point-in-range? char pt x y w h)
    ;; #\newline has indefinite right extent
    (or (and (char=? #\newline char)
             (<= x (car pt))
             (<= y (cdr pt) (+ y h)))
        (and (<= x (car pt) (+ x w))
             (<= y (cdr pt) (+ y h)))))

  ;; A CharPosHandler is a  (Char Pos X Y Width Height LineNo ColNo -> void)

  ;; for-each-charpos : Gfx CharPosHandler -> void
  (define (for-each-charpos g proc)
    (let* ((measure-height
            (lambda (s) 
              (call-with-values (lambda () ((g 'measure-text) s fnt))
                (lambda (w h) h))))
           (cursor-height #f)
           (initial-height (measure-height "A")))
      (let loop ((x 0)
                 (y 0)
                 (max-height-on-line initial-height)
                 (line-num 0)
                 (col-num 0)
                 (curr-pos 0))
        (cond
         ((>= curr-pos (string-length mytext))
          (unspecified))

         (else
          (let* ((char (string-ref mytext curr-pos))
                 (char-text (string char)))
            (call-with-values (lambda () ((g 'measure-text) char-text fnt))
              (lambda (char-w char-h)
                (proc char curr-pos x y char-w char-h line-num col-num)
                (cond 
                 ((char=? char #\newline)
                  (loop 0
                        (+ y max-height-on-line) 
                        initial-height
                        (+ line-num 1) 
                        0
                        (+ curr-pos 1)))
                 (else
                  (loop (+ x char-w)
                        y
                        (max max-height-on-line char-h)
                        line-num
                        (+ col-num 1)
                        (+ curr-pos 1))))))))))))
  
  (define (delegate msg . args)
    (cond ((and backing-agent
                (memq msg ((backing-agent 'operations))))
           (apply (backing-agent msg) args))
          (else 
           #f)))
  
  (define (insert-char-at-point! char)
    (define len (string-length mytext))
    (define on-last-line
      (and (char=? char #\newline)
            (>= (cursor-line) (- (count-visible-lines) 2))))
    (set! preferred-cursor-col #f)
    (call-with-values 
        (lambda () (cond ((number? selection) 
                          (values (substring mytext 0 selection)
                                  (substring mytext selection len)
                                  selection
                                  (+ selection 1)
                                  ))
                         (else
                          (values (substring mytext 0 (car selection))
                                  (substring mytext (cdr selection) len)
                                  (car selection)
                                  (cdr selection)
                                  ))))
      (lambda (prefix suffix pos end)
        (clear-transient-state!)
        (update-stable-ranges! pos end (- end pos))
        (set! mytext (string-append prefix (string char) suffix))
        (set! selection (+ pos 1))))
    ;; We delay the scroll until after the text change has been made...
    (cond (on-last-line
           ((wnd 'attempt-scroll) 'vertical 1))))

  (define (delete-char-at-point!)
    (define len (string-length mytext))
    (clear-transient-state!)
    (set! preferred-cursor-col #f)
    (call-with-values 
        (lambda () (cond ((number? selection) 
                          (let ((pos (max 0 (- selection 1))))
                            (values (substring mytext 0 pos)
                                    (substring mytext selection len)
                                    pos
                                    selection
                                    (substring mytext pos selection)
                                    )))
                         (else
                          (values (substring mytext 0 (car selection))
                                  (substring mytext (cdr selection) len)
                                  (car selection)
                                  (cdr selection)
                                  (substring mytext (car selection) (cdr selection))
                                  ))))
      (lambda (prefix suffix pos end deleted-text)
        (clear-transient-state!)
        (update-stable-ranges! pos end (- pos end))
        (set! mytext (string-append prefix suffix))
        (set! selection pos)
        deleted-text)))

  (define (cursor-repositioned)
    (delegate 'on-cursor-reposition))

  (define (call-with-wnd-update thunk)
    (call-with-values thunk
      (lambda vals 
        (cursor-repositioned)
        ((wnd 'update)) 
        (apply values vals))))
  
  (make-root-object textview-agent
   ((textstring) mytext)
   ((set-textstring! string) 
    (clear-stable-state!)
    (cond ((and (not (zero? (string-length string)))
                (char=? #\newline 
                        (string-ref string (- (string-length string) 1))))
           (set! mytext string))
          (else
           (begin (display "textview set-textstring! ")
                  (display "warning: appending newline")
                  (newline))
           (set! mytext (string-append string "\n")))))
   ((selection) 
    (cond ((number? selection)
           (values selection selection))
          (else
           (values (car selection) (cdr selection)))))
   ((set-selection! start-pos-incl end-pos-excl)
    (set! selection 
          (if (= start-pos-incl end-pos-excl)
              start-pos-incl
              (cons start-pos-incl end-pos-excl))))

   ((cursor-left!)               (call-with-wnd-update cursor-left!))
   ((cursor-right!)              (call-with-wnd-update cursor-right!))
   ((cursor-up!)                 (call-with-wnd-update cursor-up!))
   ((cursor-down!)               (call-with-wnd-update cursor-down!))
   ((insert-char-at-point! char) (call-with-wnd-update 
                                  (lambda () (insert-char-at-point! char))))
   ((delete-char-at-point!)      (call-with-wnd-update delete-char-at-point!))

   ((color-foreground-transiently! start-incl finis-excl col)
    (set! transient-foreground-col-ranges
          (cons (list start-incl finis-excl col) transient-foreground-col-ranges)))
   ((color-background-transiently! start-incl finis-excl col)
    (set! transient-background-col-ranges
          (cons (list start-incl finis-excl col) transient-background-col-ranges)))
   ((color-foreground-stably! start-incl finis-excl col)
    (set! stable-foreground-col-ranges
          (cons (list start-incl finis-excl col) stable-foreground-col-ranges)))
   ((color-background-stably! start-incl finis-excl col)
    (set! stable-background-col-ranges
          (cons (list start-incl finis-excl col) stable-background-col-ranges)))

   ((foreground-colors) (list transient-foreground-col-ranges
                              stable-foreground-col-ranges
                              default-foreground-col))
   ((background-colors) (list transient-background-col-ranges
                              stable-background-col-ranges
                              default-background-col))
    

   ((on-keydown mchar sym mods)  (delegate 'on-keydown mchar sym mods))
   ((on-keyup   mchar sym mods)  (delegate 'on-keyup   mchar sym mods))
   ((on-keypress char)     (delegate 'on-keypress char))
   ((on-resize)            (delegate 'on-resize))
   ((on-hscroll new-int event-type)  (delegate 'on-hscroll new-int event-type))
   ((on-vscroll new-int event-type)  (delegate 'on-vscroll new-int event-type))
   ((horizontal-scrollbar) (delegate 'horizontal-scrollbar))
   ((vertical-scrollbar)   (delegate 'vertical-scrollbar))

   ((count-visible-lines)  (count-visible-lines))

   ;; XXX not well defined for non-fixed width fonts...
   ;; ((count-visible-columns) (quotient ((wnd 'width)) ((fnt 'em-width))))

   ((on-mousedown mx my)
    (clear-transient-state!)
    (set! mouse-down (cons mx my))
    (set! mouse-drag (cons mx my))
    (set! mouse-up #f)
    ((wnd 'update)))
   ((on-mouseup mx my)
    (clear-transient-state!)
    (set! mouse-drag #f)
    (set! mouse-up (cons mx my))
    ((wnd 'update)))
   ((on-mousedrag mx my)
    (clear-transient-state!)
    (cond (mouse-drag
           (set-car! mouse-drag mx)
           (set-cdr! mouse-drag my)))
    ((wnd 'update)))
   ((on-paint g rx ry rw rh)
    (let ((pos-1 #f) 
          (pos-2 #f))
      (cond 
       ((and mouse-down mouse-drag)
        (for-each-charpos 
         g (lambda (char pos pixel-x pixel-y
                    char-pixel-width char-pixel-height 
                    line column)
             (cond ((point-in-range? 
                     char 
                     mouse-down pixel-x pixel-y
                     char-pixel-width char-pixel-height)
                    (set! pos-1 pos)))
             (cond ((point-in-range? 
                     char 
                     mouse-drag pixel-x pixel-y
                     char-pixel-width char-pixel-height)
                    (set! pos-2 pos)))
             )))
       ((and mouse-down mouse-up)
        (for-each-charpos 
         g (lambda (char pos pixel-x pixel-y
                    char-pixel-width char-pixel-height 
                    line column)
             (cond ((point-in-range? 
                     char 
                     mouse-down pixel-x pixel-y
                     char-pixel-width char-pixel-height)
                    (set! pos-1 pos)))
             (cond ((point-in-range? 
                     char 
                     mouse-up   pixel-x pixel-y
                     char-pixel-width char-pixel-height)
                    (set! pos-2 pos)))))
        (set! mouse-down #f)
        (set! mouse-up #f)))
      (cond ((and pos-1 pos-2)
             (set! selection
                   (cond ((= pos-1 pos-2)
                          pos-1)
                         (else (cons (min pos-1 pos-2)
                                     (max pos-1 pos-2)))))
             (cursor-repositioned)))
      )
    
    ((g 'fill-rect) default-background-col rx ry (+ rx rw) (+ ry rh))
    (call-with-values (lambda () ((g 'measure-text) "A" fnt))
      (lambda (a-char-w a-char-h)
        (for-each-charpos 
         g (lambda (char pos x y w h line column)
             (let ((fg-col (or (lookup-col pos transient-foreground-col-ranges)
                               (lookup-col pos stable-foreground-col-ranges)
                               default-foreground-col))
                   (bg-col (or (lookup-col pos transient-background-col-ranges)
                               (lookup-col pos stable-background-col-ranges)
                               default-background-col))
                   (sel-fg-col (or (lookup-col pos selection-foreground-col-ranges)
                                   (default-selection-foreground-col)))
                   (sel-bg-col (or (lookup-col pos selection-background-col-ranges)
                                   (default-selection-background-col))))
               (cond
                ((and (number? selection) 
                      (= selection pos)
                      (char=? char #\newline))
                 ((g 'fill-rect) sel-bg-col x y (+ x a-char-w) (+ y a-char-h))
                 )
                ((and (<= (selection-start-pos) pos)
                      (< pos (selection-finis-pos)))
                 ((g 'fill-rect) sel-bg-col x y (+ x w) (+ y h))
                 ((g 'draw-text) (string char) fnt x y sel-fg-col))
                (else
                 ((g 'fill-rect) bg-col x y (+ x w) (+ y h))
                 ((g 'draw-text) (string char) fnt x y fg-col)))))))))
   ;; Note this method is not meant for use outside of editor-agent-maker
   ((set-backing-agent! agent) (set! backing-agent agent))
   ;; This method is not really meant to be used except for debugging.
   ((backing-agent) backing-agent)

   ;; These methods *might* be sane.  Or they might be a sign of a
   ;; serious design mistake.  Or they might need to be factored out
   ;; into an object extension/subclass/mixin/whatever...
   ((load-file-cmd)    (delegate 'load-file-cmd))
   ((save-file-cmd)    (delegate 'save-file-cmd))
   ((save-file-as-cmd) (delegate 'save-file-as-cmd))
   ))

(define (make-simplest-backing-agent wnd editor-agent)
  (make-root-object simplest-backing-agent
   ((on-keyup   sym mods)
    (case sym
      ((enter)       ((editor-agent 'insert-char-at-point!) #\newline))
      ((left)        ((editor-agent 'cursor-left!)))
      ((right)       ((editor-agent 'cursor-right!)))
      ((up)          ((editor-agent 'cursor-up!)))
      ((down)        ((editor-agent 'cursor-down!)))
      ((back delete) ((editor-agent 'delete-char-at-point!)))))
   ((on-keypress char)
    (case char
      ((#\backspace #\return #\esc #\tab) 'do-nothing)
      (else 
       ((editor-agent 'insert-char-at-point!) char))))))

(define (make-auto-indenting-agent wnd editor-agent)
  (define (count-newlines-in string)
    (length (filter (lambda (x) (char=? x #\newline)) (string->list string))))
  ;; String Nat -> [Maybe Nat]
  (define (index-after-line-count text line-count)
    (let loop ((i 0) (j 0))
      (cond ((= i (string-length text))
             #f) ;; if we return #f, then there are not line-count lines in text
            ((= j line-count)
             i)
            (else
             (loop (+ i 1)
                   (+ j (if (char=? #\newline (string-ref text i)) 1 0)))))))
  (define (line-count)
    (let ((prefix-lines (count-newlines-in prefix))
          (text-lines   (count-newlines-in ((editor-agent 'textstring))))
          (suffix-lines (count-newlines-in suffix)))
      '(begin (display `(line-count ((prefix-lines ,prefix-lines)
                                    (text-lines   ,text-lines)
                                    (suffix-lines ,suffix-lines))))
             (newline))
      (+ prefix-lines text-lines suffix-lines)))
     
     
  (define first-line-idx 0)
  ;; Bad rep; scrolling op's take O(n) time (where n is the size of
  ;; the entirety of the text).  A doubly linked list of lines may be
  ;; better.  Or even a pair of singly linked list (where the first is
  ;; kept in reverse order).  But this is simple prototype code.
  (define prefix "")
  (define suffix "")

  (define current-filename #f)

  (require "Experimental/scheme-source")

  (make-root-object auto-indenting-agent
   ((on-keydown mchar  sym mods)
    (case sym
      ((enter)       
       (let* ((ea editor-agent)
              (prefixstr (call-with-values (lambda () ((ea 'selection))) 
                           (lambda (beg end) 
                             (substring ((ea 'textstring)) 0 beg))))
              (indent
               (suggest-indentation 
                (open-input-string 
                 (list->string (reverse (string->list prefixstr)))))))
         ((editor-agent 'insert-char-at-point!) #\newline)
         (do ((i indent (- i 1)))
             ((zero? i))
           ((editor-agent 'insert-char-at-point!) #\space))))
      ((tab)         
       (let* ((ea editor-agent)
              (text ((ea 'textstring))))
         (call-with-values (lambda () ((ea 'selection)))
           (lambda (beg end) 
             ;; 1. if beg = end, then we want to indent cursor's line
             ;; 2. if beg < end, then we want to indent selected region 
             ;;               (I think)
             ;; For now, just assume beg = end; I can add support for 
             ;; the case (2) later.
             
             (let* (;; search backward for the newline
                    (line-start (do ((i beg (- i 1)))
                                    ((or (= i 0)
                                         (char=? #\newline (string-ref text (- i 1))))
                                     i)))
                    ;; search forward for non-whitespace; don't go past a newline
                    (content-start (do ((i line-start (+ i 1)))
                                       ((or (= i (string-length text))
                                            (char=? #\newline (string-ref text i))
                                            (not (char-whitespace? (string-ref text i))))
                                        i)))
                    ;; find suggested indentation
                    (prefixstr (substring text 0 line-start))
                    (indent
                     (suggest-indentation
                      (open-input-string
                       (list->string (reverse (string->list prefixstr))))))
                    (delta (- (- content-start line-start) indent))
                    )
               
               ;; replace all that white space with spaces for suggested indent
               ((ea 'set-textstring!) (string-append (substring text 0 line-start)
                                                     (make-string indent #\space)
                                                     (substring text content-start 
                                                                (string-length text))))

               ;; fix the cursor's position
               ((ea 'set-selection!) (- beg delta) (- end delta))
               
               ((wnd 'update))

               (begin (display "saw tab!") (newline))
               )))))
      ((left)        ((editor-agent 'cursor-left!)))
      ((right)       ((editor-agent 'cursor-right!)))
      ((up)          ((editor-agent 'cursor-up!)))
      ((down)        ((editor-agent 'cursor-down!)))
      ((back delete) 
       (let* (;; 1. Delete the text
              (deleted-text ((editor-agent 'delete-char-at-point!)))
              ;; 2. Analyze deleted text to see if we need to append more text
              (lines (count-newlines-in deleted-text)))
         (cond 
          ((not (= lines 0))
           ;;    3. Okay, append more text.
           (let* ((text ((editor-agent 'textstring)))
                  (idx  (index-after-line-count suffix lines))
                  (suflen (string-length suffix))
                  (append-text (if idx (substring suffix 0 idx) suffix))
                  (new-text (string-append text append-text))
                  (new-suffix (if idx (substring suffix idx suflen) "")))
             ((editor-agent 'set-textstring!) new-text)
             (set! suffix new-suffix))))))
      ))

   ((on-keypress char)
    (case char
      ((#\backspace #\return #\esc #\tab) 'do-nothing)
      (else 
       ((editor-agent 'insert-char-at-point!) char))))
   ((on-cursor-reposition)
    (let* ((matched-col (name->col "Orange"))
           (unmatch-col (name->col "Red"))
           (ea editor-agent)
           (text ((ea 'textstring)))
           (cursor-pos (call-with-values (lambda () ((ea 'selection)))
                         (lambda (beg end) 
                           end)))
           (preceding-char (and (<= 1 cursor-pos (string-length text))
                                (string-ref text (- cursor-pos 1)))))
      (cond 
       ((and preceding-char 
             (case preceding-char ((#\] #\)) #t) (else #f)))
        (cond 
         ((let ((prefixstr (substring text 0 (max 0 (- cursor-pos 1)))))
            (count-until-unmatched-open-paren
             (open-input-string 
              (list->string
               (reverse (string->list prefixstr))))))
          => (lambda (count-until-match)
               (let* ((e (- cursor-pos 1))
                      (s (- cursor-pos (+ 2 count-until-match))))
                 ((ea 'color-background-transiently!) s (+ s 1) matched-col)
                 ((ea 'color-background-transiently!) e (+ e 1) matched-col))))
         (else
          (let ((e cursor-pos))
            ((ea 'color-background-transiently!) e (+ e 1) unmatch-col))))))))
   ((vertical-scrollbar)
    ;; These do not have to be in pixels to be meaningful; we as the
    ;; client select our own unit of measurement, and are then
    ;; responsible for using it consistently.  In this case, we are
    ;; using a line as the measurement grain, (and I suppose that
    ;; the horizontal scrollbars will use a column as the grain).
    ;; When image support is added, these grains might not remain
    ;; appropriate.
    (let* ((my-line-count (line-count))
           (visible-lines ((editor-agent 'count-visible-lines)))
           (max-val (max 0 (- my-line-count visible-lines))) ;; XXX this is buggy
           (max-val my-line-count)) ;; This is questionable but easier to work with
      (cond ((not (<= 0 first-line-idx max-val))
             (display `(want: (<= 0 ,first-line-idx ,max-val)))
             (newline)))
      (assert (<= 0 first-line-idx max-val))
      `((min ,0)
        (max ,max-val)
        (value ,first-line-idx)
        (dsmall ,1)
        (dlarge ,(if (> (* 2 visible-lines) my-line-count)
                     1
                     (quotient visible-lines 2)
                     )))))
   ((on-vscroll new-int event-type)

    (let* ((visible-line-count ((editor-agent 'count-visible-lines)))
           (old-int first-line-idx)
           ;; 1. Extract existing text + cursor info from the textview.
           (text ((editor-agent 'textstring)))
           (cursor-info (call-with-values (lambda () ((editor-agent 'selection))) 
                          list))
           ;; 2. Merge textview's text with our own text.
           (text (string-append prefix text suffix))
           ;; 3. Extract the appropriate substring from the total text.
           (subtext-start-idx (index-after-line-count text new-int))
           (subtext-finis-idx (index-after-line-count text (+ new-int 
                                                              visible-line-count)))
           ;; 4. Calculate change to cursor-info
           (cursor-delta (- (string-length prefix) subtext-start-idx))
           (new-cursor-info (map (lambda (x) (+ x cursor-delta)) cursor-info)))
      (let* ((prefix* (substring text 0 (or subtext-start-idx 0)))
             (start   (or subtext-start-idx 0))
             (finis   (or subtext-finis-idx (string-length text)))
             (view*   (substring text start finis))
             (suffix* (substring text finis (string-length text))))
        '(begin (display `(repartition-and-install ,text 
                                                  (,0 ,subtext-start-idx
                                                      ,subtext-finis-idx
                                                      ,(string-length text))
                                                  ,prefix*
                                                  ,view*
                                                  ,suffix*
                                                  ))
               (newline))
        ;;    5. Repartition and install text
        (set! prefix prefix*)
        ((editor-agent 'set-textstring!) view*)
        (set! suffix suffix*)
        (apply (editor-agent 'set-selection!) new-cursor-info))
      (set! first-line-idx new-int)
      ((wnd 'update))
      ))
   
   ((load-file-cmd)
    (cond ((open-file-chooser-dialog (current-directory)
                                     (list (list "Scheme Files" "sch" "scm" "ss")
                                           (list "All Files" "*")))
           => (lambda (name)
                ((auto-indenting-agent 'load-file) name)))))
   ((save-file-cmd)
    (cond (current-filename ((auto-indenting-agent 'save-file-as) current-filename))
          (else ((auto-indenting-agent 'save-file-as-cmd)))))
   ((save-file-as-cmd) 
    (cond ((save-as-file-chooser-dialog (current-directory)
                                        (list (list "Scheme Files" "sch" "scm" "ss")
                                              (list "All Files" "*")))
           => (lambda (name)
                ((auto-indenting-agent 'save-file-as) name)))))

   ((load-file filename)   
    (set! current-filename filename)
    (call-with-input-file filename
      (lambda (filein)
        (let* ((chars (do ((c (read-char filein) (read-char filein))
                           (l '() (cons c l)))
                          ((eof-object? c) (reverse l))))
               (text (list->string chars))
               (visible-line-count ((editor-agent 'count-visible-lines)))
               (line-idx 0)
               (subtext-start-idx (index-after-line-count text line-idx))
               (subtext-finis-idx (index-after-line-count 
                                   text (+ line-idx visible-line-count)))
               (prefix* (substring text 0 line-idx))
               (start (or subtext-start-idx 0))
               (finis (or subtext-finis-idx (string-length text)))
               (view* (substring text start finis))
               (suffix* (substring text finis (string-length text))))
          (set! prefix prefix*)
          ((editor-agent 'set-textstring!) view*)
          (set! suffix suffix*)
          ((editor-agent 'set-selection!) 0 0)
          (set! first-line-idx line-idx)
          ((wnd 'update))
          ))))
   ((save-file-as filename) 
    (set! current-filename filename)
    (call-with-output-file filename
      (lambda (fileout)
        (let ((text (string-append prefix ((editor-agent 'textstring)) suffix)))
          (do ((i 0 (+ i 1)))
              ((= i (string-length text)))
            (write-char (string-ref text i) fileout))))))
   ))

(define (editor-agent-maker make-backing-agent)
  (lambda (wnd width height)
    (let* ((textview-agent (make-textview-agent wnd width height))
           (backing-agent (make-backing-agent wnd textview-agent)))
      ((textview-agent 'set-backing-agent!) backing-agent)
      textview-agent)))

(define (make-editor-agent wnd width height)
  ((editor-agent-maker make-simplest-backing-agent) wnd width height))

(define (make-read-eval-print-loop-agent wnd editor-agent)
  (define prompt-idx 0)
  (define (bump-prompt! count) 
    '(begin (display "  ")
           (write `(bump-prompt! ,count prompt-idx: ,prompt-idx))
           (newline))
    (set! prompt-idx (+ prompt-idx count)))
  
  ;; A MaybeReadResult is one of:
  ;; - (list 'evaluate  Sexp Nat)
  ;; - (list 'eof       Char)
  ;; - (list 'error     Any)
  
  ;; maybe-read : TextualPort Char -> MaybeReadResult
  (define (maybe-read port input-char)
    (call-with-current-continuation
     (lambda (escape)
       (let* ((idx 0)
              (custom-binary-port
               ;; This is so broken.  Its a total hack to try to
               ;; convert a string into a binary port.  But Larceny
               ;; does not support constructing custom string ports
               ;; yet.  Other options include making a custom reader
               ;; for this purpose, but that's also unappealing.
               (let ((read! 
                      (lambda (bytevec start count)
                        (let ((c (get-char port)))
                          (cond 
                           ((eof-object? c)
                            (escape `(eof ,input-char)))
                           (else 
                            (set! idx (+ idx 1))
                            (bytevector-set! bytevec start (char->integer c))
                            1))))))
                 (make-custom-binary-input-port "REPL PORT" read! #f #f #f)))
              (custom-port
               ;; See note above; once Larceny has custom text ports,
               ;; then transcoding won't be necessary here.
               (transcoded-port custom-binary-port (native-transcoder)))
              (read-result
               (call-with-error-handler
                (lambda args (escape `(error ,args)))
                (lambda () (read custom-port)))))
         `(evaluate ,read-result ,idx)))))

  (define (insert-string-at-point! editor-agent string)
    (do ((i 0 (+ i 1)))
        ((= i (string-length string)))
      ((editor-agent 'insert-char-at-point!) (string-ref string i))))
  
  (define (insert-string-at-point/bump! editor-agent string)
    '(begin (write `(insert-string-at-point/bump! editor-agent ,string))
           (newline))
    (insert-string-at-point! editor-agent string)
    (bump-prompt! (string-length string)))
  
  ;; Maybe I should abstract over this above...
  (define my-own-env (environment-copy (interaction-environment)))

  (make-root-object repl-agent
   ((on-keydown mchar sym mods)
    (case sym
      ((enter) 
       ;; If user hits enter *and* we're at the end of the foremost
       ;; S-exp, then evaluate it.  I'm going to hack this up by
       ;; making a custom port for reading from the text.  If the
       ;; reader ever requests to read past the end of the input text,
       ;; then we abandon the read attempt (via an escape
       ;; continuation) and just insert the #\newline.
       ;; TODO: add support for catching errors during the read,
       ;; printing the error message in the GUI, and resetting REPL.
       ;; TODO: figure out how this is going to integrate with Image
       ;; support.  (Perhaps even at this level, images will be
       ;; rendered to ASCII and the text view will be reponsible for
       ;; interpreting the code sequences.
       
       '(begin (write `((on-keydown ,sym)
                       (prompt-idx: ,prompt-idx)
                       (text: ,((editor-agent 'textstring)))
                       (len: ,(string-length ((editor-agent 'textstring))))
                       ))
              (newline))

       (let* ((ea editor-agent)
              (text ((ea 'textstring)))
              (subtext (substring text prompt-idx (string-length text)))
              ;; Add whitespace to end of text, so that this will work independently of
              ;; whether (ea 'textstring) returns text ending with newline
              (orig-port (open-string-input-port subtext))
              (mrr (maybe-read orig-port #\newline)))

         '(begin (write `((prompt-idx: ,prompt-idx)
                         (subtext: ,subtext)
                         (mrr: ,mrr)))
                (newline))

         (case (car mrr)
           ((evaluate) 
            ;; First provide user feedback by actually printing char
            (cond (mchar
                   (insert-string-at-point/bump! ea (string mchar))))

            ;; XXX during evaluation, should override
            ;; current-input-port and current-output-port here so that
            ;; user code will side-effect the REPL window or something
            ;; similar.  (DrScheme's approach is very nice IMHO.)
            ;; XXX during evaluation, we also should catch errors
            ;; and print them to the REPL window.
            (let* ((sexp (cadr mrr))
                   (count-chars-read (caddr mrr))
                   (val (eval sexp my-own-env))
                   (valstr 
                    (call-with-output-string
                     (lambda (strport)
                       (call-with-current-continuation 
                        (lambda (escape)
                          (call-with-error-handler 
                           (lambda (who . args)
                             (display "Error during printing.  ")
                             (display "NOT reverting to ur-printer tho'.")
                             (newline)
                             (escape "print-error"))
                           (lambda () ((repl-printer) val strport))))))))
                   (promptstr
                    (call-with-output-string
                     (lambda (strport)
                       ((repl-prompt) (repl-level) strport))))
                   (ignore 
                    '(begin (write `((count-chars-read: ,count-chars-read)
                                    (subtext len: ,(string-length subtext))
                                    (subtext: ,subtext)))
                           (newline)))
                   (remaining-input
                    (substring subtext 
                               count-chars-read (string-length subtext)))
                   ;; with data like () or "foo", remaining-input will
                   ;; include a newline.  for data that requires a
                   ;; delimiter (like a number or symbol),
                   ;; remaining-input will not include the newline
                   ;; Either way, *we* don't want it.
                   (remaining-input
                    (let ((len (string-length remaining-input)))
                      (cond 
                       ((= len 0) remaining-input)
                       ((char=? (string-ref remaining-input (- len 1))
                                #\newline)
                        (substring remaining-input 0 (- len 1)))
                       (else
                        remaining-input)))))

              ;; Now that we've read it the user's text, we should
              ;; bump the prompt over it.
              '(begin (write `(manually bumping ,(- (string-length subtext) 1) for ,subtext))
                     (newline))
              (bump-prompt! (- (string-length subtext) 1))

              ;; Write rendered value to textview.
              (insert-string-at-point/bump! ea valstr)

              ;; Print new prompt
              (insert-string-at-point/bump! ea promptstr)
              
              ;; XXX if there is still data on orig-port, then we
              ;; should probably propagate it down past the new
              ;; prompt.  (This does not match DrScheme's behavior
              ;; though; I believe it evaluates greedily until there
              ;; aren't any S-exps left; another option for us.)
              '(begin (write `(propagating remaining-input: ,remaining-input))
                     (newline))
              (insert-string-at-point! ea remaining-input)
              ))
           ((eof)      
            (insert-string-at-point/bump! ea (string (cadr mrr))))
           ((error)
            (let ((errstr (call-with-output-string
                           (lambda (p)
                             (decode-error (cadr mrr) p))))
                  (promptstr (call-with-output-string
                              (lambda (strport)
                                ((repl-prompt) (repl-level) strport)))))
              (bump-prompt! (string-length subtext))
              (insert-string-at-point/bump! ea errstr)
              (insert-string-at-point/bump! ea promptstr)
              ))
           (else (error 'repl-agent..on-keydown ": oops.")))))
      (else
       (cond 
        (mchar 
         (let* ((ea editor-agent))
           ;; move cursor to end of buffer
           (call-with-values (lambda () ((ea 'selection)))
             (lambda (beg end) ((ea 'set-selection!) end end)))
           ;; This doesn't bump the prompt-idx, because it is
           ;; part of the user input that we're still waiting
           ;; to process.
           (insert-string-at-point! ea (string mchar))
           ))))))
   ((prompt!) 
    (let* ((ea editor-agent))
      (call-with-values (lambda () ((ea 'selection)))
        (lambda (beg end) ((ea 'set-selection!) end end)))
      (let ((str (call-with-output-string 
                  (lambda (strport)
                    ((repl-prompt) (repl-level) strport)))))
        (insert-string-at-point/bump! ea str))))
   ))
