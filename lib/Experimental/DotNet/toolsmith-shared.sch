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
;; (make-window) 
;; (make-window 'make-agent make-window-agent 'text 
;;              'bounds (left top right bottom) 'title string
;;              'nogoaway 'nosizebox)
;;    (make-window 'text) is equivalent to 
;;    (make-window 'make-agent make-editor)
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
;; Note that for useful agents, client developer defines appropriate
;; constructors (perhaps via msg-handler form) and passes a
;; constructor to make-wnd via 'make-agent keyword argument
;;
;; AGENT: (make-noisy-agent wnd width height)
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

;; FNT: (make-fnt name em-size) 
;;  (clone ['italic] ['bold] ['underline] ['em-size r])
;;  (name) (em-size) (italic?) (bold?) (underline?) (fntptr)
;; Font names are availble via the procedures:
;;  (available-fontnames)
;;  (monospace-fontname) (sans-serif-fontname) (serif-fontname)
;; 
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
;; IMG: (make-blank-img w h) (make-img/bmp w h bv) (xpm->img s) (file->img f)
;;  (sourcefile) => [Maybe PathString]
;;  (bitmap)     => Bytevector

;; TODO consider adopting something like the IDataObject interface for
;; data stored on clipboard.
;; 
;; Clipboard related operations (global procedures):
;; (clipboard-clear!)
;; (clipboard-contains-audio?)
;; (clipboard-contains-filelist?)
;; (clipboard-contains-image?)
;; (clipboard-contains-text?)
;; (clipboard-get-audio)     => [Oneof InputPort ByteVector]
;; (clipboard-get-filelist)  => [Listof PathString]
;; (clipboard-get-image)     => img
;; (clipboard-get-text)      => string
;; (clipboard-set-audio! aud)    where aud in [Oneof OutputPort ByteVector]
;; (clipboard-set-filelist! lst) where lst in [Listof PathString]
;; (clipboard-set-image! img)
;; (clipboard-set-text! string)

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

;; TextModel has methods
;;    textstring set-textstring!                         
;;    selection set-selection!                           
;;    on-cursor-reposition                               
;;    cursor-left! cursor-right! cursor-up! cursor-down! 
;;    insert-char-at-point! delete-char-at-point!        

;; [Rendered T]  <: T has on-paint wnd font-ranges
;; [Keyed T]     <: T has on-keydown on-keyup on-keypress
;; [Moused T]    <: T has on-mousedown on-mouseup on-mousedrag
;; [Scrolled T]  <: T has on-hscroll on-vscroll 
;;                        horizontal-scrollbar vertical-scrollbar
;; [Colorable T] <: T has color-foreground-transiently!
;;                        color-background-transiently!
;;                        color-foreground-stably!
;;                        color-background-stably!                
;; make-textmodel : -> TextModel
(define (make-textmodel)
  (define mytext "") ;; revisit when we add IMG objects.
  (define selection 0) ;; [Oneof Nat (cons Nat Nat)]
  (define preferred-cursor-col #f) ;; used when moving cursor up and down

  (define (cursor-left!)
    (set! preferred-cursor-col #f)
    (cond ((number? selection) 
           (set! selection (max 0 (- selection 1))))
          (else
           (set! selection (car selection)))))
  (define (cursor-right!)
    (set! preferred-cursor-col #f)
    (cond ((number? selection) 
           (set! selection (min (string-length mytext) (+ selection 1))))
          (else 
           (set! selection (cdr selection)))))
  (define (cursor-vertical! dir)
    (define pos (cond ((number? selection) selection)
                      (else (car selection))))
    (let ((start-of-line-idx ;; search backward for start of line cursor is on
           (do ((c-idx (- pos 1) (- c-idx 1)))
               ((or (< c-idx 0) (char=? #\newline (string-ref mytext c-idx)))
                (+ c-idx 1)))))
      (cond ((not preferred-cursor-col) ;; save current cursor column if needed
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
                    ((> idx (string-length mytext)) ;; DOC for CHANGE BELOW
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
		       ;; This change and the one above fix a problem
		       ;; where hitting down on the last line with
		       ;; content did not move the cursor to the
		       ;; implicit newline.
		       (= idx (string-length mytext))
		       (char=? #\newline (string-ref mytext idx)))
		   idx))))
        (set! selection target-idx))))
  (define (do-cursor-up!)
    (cursor-vertical! 'backward))
  (define (do-cursor-down!)
    (cursor-vertical! 'forward))

  (define (insert-char-at-point! char)
    (define len (string-length mytext))
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
        (set! mytext (string-append prefix (string char) suffix))
        (set! selection (+ pos 1)))))

  (define (delete-char-at-point!)
    (define len (string-length mytext))
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
                                  (substring mytext 
                                             (car selection) (cdr selection))
                                  ))))
      (lambda (prefix suffix pos end deleted-text)
        (set! mytext (string-append prefix suffix))
        (set! selection pos)
        deleted-text)))

  (define (call-with-wnd-update self thunk)
    (call-with-values thunk
      (lambda vals 
        ((self 'on-cursor-reposition))
        (apply values vals))))

  (make-root-object textmodel
   ((textstring) "=> string holding model's text." mytext)
   ((set-textstring! string) "Sets model's text to string parameter."
    (set! mytext string))
   ((selection) "=> (values b e) where [b,e) is selected.  
 If b = e, then no text is selected, and the cursor resides at b."
    (cond ((number? selection)
           (values selection selection))
          (else
           (values (car selection) (cdr selection)))))

   ((on-cursor-reposition) "event hook" 'default-hook-does-nothing)
   ((set-selection! start-pos-incl end-pos-excl) "Sets model's selection to 
 [start-pos-incl,end-pos-excl)."
    (call-with-wnd-update
     textmodel 
     (lambda () (set! selection 
                      (if (= start-pos-incl end-pos-excl)
                          start-pos-incl
                          (cons start-pos-incl end-pos-excl))))))
   ((cursor-left!)  "Moves cursor one column left (wrapping
 to end of previous line) if possible." 
    (call-with-wnd-update textmodel cursor-left!))
   ((cursor-right!) "Moves cursor one column right (wrapping
 to start of succeeding line) if possible."
    (call-with-wnd-update textmodel cursor-right!))
   ((cursor-up!)    "Moves cursor one line up, if possible."
    (call-with-wnd-update textmodel do-cursor-up!))
   ((cursor-down!)  "Moves cursor one line down, if possible."
    (call-with-wnd-update textmodel do-cursor-down!))
   ((insert-char-at-point! char) "Inserts char after cursor or replaces 
 selected text with char."
    (let ((insert! (lambda () (insert-char-at-point! char))))
      (call-with-wnd-update textmodel insert!)))
   ((delete-char-at-point!) "Deletes selected text or char preceding 
 cursor, if any.
 => deleted string."
    (call-with-wnd-update textmodel delete-char-at-point!))
   ))

;; A CharPosHandler is a  (Char Pos X Y Width Height LineNo ColNo -> void)
;; A [CharPosCont X] is a (X Y Height LineNo ColNo Pos -> X)

;; for-each-charpos : String Nat Gfx Fnt CharPosHandler [CharPosCont X] -> X
(define (for-each-charpos textmodel g fnt proc at-end)
  (let* ((mytext ((textmodel 'textstring)))
         (start-pos ((textmodel 'visible-offset)))
         (measure-height 
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
               (curr-pos start-pos)) 
      (cond
       ((>= curr-pos (string-length mytext))
        (at-end x y max-height-on-line line-num col-num curr-pos))
       
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

;; A TextElemHandler is a 
;;       (TextElem CharPos CharSpan X Y Width Height LineNo ColNo -> void)
;; A [TextElemCont X] is a (X Y Height LineNo ColNo Pos -> X)
;; A TextElem is one of:
;; - String (which has no newlines)
;; - #\newline

;; for-each-textelem : 
;;     [Rendered TextModel] Gfx Fnt TextElemHandler [TextElemCont X] -> X
(define (for-each-textelem textmodel g fnt proc at-end)
  (define (char->textelem char)
    (if (char=? char #\newline) #\newline (string char)))
  (for-each-charpos textmodel 
                    g 
                    fnt 
                    (lambda (char pos x y w h line-num col-num)
                      (proc (char->textelem char) 
                            pos 1 x y w h line-num col-num))
                    at-end))

(define (for-each-textelem/old textmodel g fnt proc at-end)
  ;; XXX this should also subdivide based on changes in font...
  (let* ((mytext ((textmodel 'textstring)))
         (start-pos ((textmodel 'visible-offset)))
         (sel-start (call-with-values (textmodel 'selection)
                      (lambda (s e) s)))
         (sel-finis (call-with-values (textmodel 'selection)
                      (lambda (s e) e)))
         (measure-height
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
               (curr-pos start-pos))
      (cond
       ((>= curr-pos (string-length mytext))
        (at-end x y max-height-on-line line-num col-num curr-pos))
       
       (else
        (let* ((<&<= (lambda (x y z) (and (< x y) (<= y z))))
               (<=&< (lambda (x y z) (and (<= x y) (< y z))))
               (telem 
                (cond
                 ((char=? #\newline (string-ref mytext curr-pos))
                  #\newline)
                 (else
                  (let char-scan ((chars '())
                                  (pos curr-pos))
                    (cond
                     ((or (>= pos (string-length mytext))
                          (<&<= curr-pos sel-start pos)
                          (<=&< curr-pos sel-start pos)
                          (<&<= curr-pos sel-finis pos)
                          (<=&< curr-pos sel-finis pos)
                          (char=? #\newline (string-ref mytext pos)))
                      (list->string (reverse chars)))
                     (else
                      (char-scan (cons (string-ref mytext pos) chars)
                                 (+ pos 1)))))))))
          (cond 
           ((eqv? telem #\newline)
            (call-with-values 
                (lambda () ((g 'measure-text) (string telem) fnt))
              (lambda (char-w char-h)
                (proc telem curr-pos 1 x y char-w char-h line-num col-num)))
            (let ((y* (+ y max-height-on-line))
                  (line-num* (+ line-num 1))
                  (curr-pos* (+ curr-pos 1)))
              (loop 0 y* initial-height line-num* 0 curr-pos*)))
           (else
            (call-with-values (lambda () ((g 'measure-text) telem fnt))
              (lambda (text-w text-h)
                (let ((span (string-length telem)))
                  (proc telem curr-pos span x y 
                        text-w text-h ;; XXX
                        line-num col-num)
                  (loop (+ x text-w)
                        y
                        (max max-height-on-line text-h)
                        line-num
                        (+ col-num 1)
                        (+ curr-pos span)))))))))))))

;; rendering-extender : Wnd -> T -> [Rendered T] where T <: TextModel
(define (rendering-extender wnd)
  (define em-size 10)
  (define fnt (make-fnt (monospace-fontname) em-size))
  (define default-foreground-col (name->col (string->symbol "Black")))
  (define default-background-col (name->col (string->symbol "White")))
  
  (define (count-visible-lines/lower-bound)
    (inexact->exact (floor (count-visible-lines/fractional))))
  (define (count-visible-lines/upper-bound)
    (inexact->exact (ceiling (count-visible-lines/fractional))))
  (define (count-visible-lines/fractional)
    (/ ((wnd 'height)) 
       (call-with-values (lambda () ((wnd 'measure-text) "" fnt))
	 (lambda (w h) h))))
  (define (selection-start-pos self)
    (call-with-values (lambda () ((self 'selection)))
      (lambda (s e) s)))
  (define (selection-finis-pos self)
    (call-with-values (lambda () ((self 'selection)))
      (lambda (s e) (if (= s e) (+ e 1) e))))
  
  (lambda (textmodel)
    (extend-object textmodel renderable-textmodel
     ((on-cursor-reposition) 
      ((wnd 'update))
      ((delegate textmodel 'on-cursor-reposition renderable-textmodel)))
     ((wnd) wnd)
     ((font-ranges) "=> list of (fnt s e) where fnt is used on
 text in range [s,e)."
      ;; Making this so flexible may have been a mistake...
      ;; 1. Ambiguities as to what happens when ranges overlap
      ;; 2. If disallow overlaps, perhaps require returned list is sorted.
      (list (list fnt 0 (string-length ((renderable-textmodel 'textstring))))))
     ((on-resize) "handler for window resize event." #f)
     ((count-visible-lines)  "=> number of lines visible in buffer."
      (count-visible-lines/fractional))
     ;; XXX adding this as a hook that can be potentially overridden, 
     ;; but I'm not sure it is a good idea.
     ((visible-offset) "=> integer offset where the visible part of str begins.
 Override to change view behavior."
      0)
     ((foreground-color idx) default-foreground-col)
     ((background-color idx) default-background-col)
     ((on-paint g rx ry rw rh) "handler for window paint event."
      ((g 'fill-rect) default-background-col rx ry (+ rx rw) (+ ry rh))
      (call-with-values (lambda () ((g 'measure-text) "A" fnt))
        (lambda (a-char-w a-char-h)
	  (define max-lines (count-visible-lines/upper-bound))
	  (let* ((self renderable-textmodel)
		 (text ((self 'textstring)))
		 (visible-offset ((self 'visible-offset)))
		 (foreground (self 'foreground-color))
		 (background (self 'background-color))
		 (selection  (self 'selection))
		 (fill-rect (g 'fill-rect))
		 (draw-text (g 'draw-text)))
	    ;; XXX this change fixes a bug where abandoning in the
	    ;; middle of the background render would cause *none* of
	    ;; the foreground to be rendered.
	    (call-with-current-continuation
	     (lambda (abandon)
               ;; Draw background
               (for-each-textelem
                self
                g fnt
                (lambda (telem pos span x y w h line column)
		  '(begin (write `(background ,telem ,pos ,x ,y ,line ,column))
			 (newline))                  (cond ((> line max-lines)
                         (abandon line)))
                  (let* ((bg-col (background pos))
                         (sel-bg-col (invert-col bg-col)))
                    (cond
                     ((and (call-with-values selection (lambda (s e) 
                                                         (= s e pos)))
                           (eqv? telem #\newline))
                      (fill-rect sel-bg-col x y (+ x a-char-w) (+ y a-char-h)))
                     ((and (<= (selection-start-pos self) pos)
                           (< pos (selection-finis-pos self)))
                      (fill-rect sel-bg-col x y (+ x w) (+ y h)))
                     (else
                      (fill-rect bg-col x y (+ x w) (+ y h))))))
                (lambda (x y height line-num col-num pos)
                  (unspecified)))))
	    ;; DOC for CHANGE above
	    (call-with-current-continuation
	     (lambda (abandon)
               ;; Draw foreground
               (for-each-textelem 
                self
                g fnt
                (lambda (telem pos span x y w h line column)
		  '(begin (write `(foreground ,telem ,pos ,x ,y ,line ,column))
			 (newline))
                  (cond ((> line max-lines)
                         (abandon line)))
                  (let* ((fg-col (foreground pos))
                         (sel-fg-col (invert-col fg-col)))
                    (cond 
                     ((and (string? telem)
                           (<= (selection-start-pos self) pos)
                           (< pos (selection-finis-pos self)))
                      (draw-text telem fnt x y sel-fg-col))
                     ((string? telem)
                      (draw-text telem fnt x y fg-col)))))
                (lambda (x y height line-num col-num pos)
                  (cond 
                   ((call-with-values selection (lambda (s e) (= s e pos)))
                    (fill-rect (invert-col (background (- pos 1)))
                               x y (+ x a-char-w) (+ y a-char-h))))))))
             )))))
    ))
  
;; extend-with-keystroke-handling : T -> [Keyed T] where T <: TextModel
(define (extend-with-keystroke-handling textmodel)
  (extend-object textmodel keystroke-handling-textmodel
   ((on-keydown mchar  sym mods) "handler for keydown of keystroke sym
 with modifiers mods.  
 If sym and mods correspond to char c, then mchar is c; otherwise mchar is #f."
    (let ((self keystroke-handling-textmodel))
      (case sym
        ((enter)       ((self 'insert-char-at-point!) #\newline))
        ((left)        ((self 'cursor-left!)))
        ((right)       ((self 'cursor-right!)))
        ((up)          ((self 'cursor-up!)))
        ((down)        ((self 'cursor-down!)))
        ((back delete) ((self 'delete-char-at-point!)))
        (else
         (cond (mchar
                (case mchar
                  ((#\backspace #\return #\esc #\tab) 'do-nothing)
                  (else ((self 'insert-char-at-point!) mchar)))))))))
   ((on-keyup mchar  sym mods) "handler for keyup of keystroke sym 
 with modifiers mods.
 If sym and mods correspond to char c, then mchar is c; otherwise mchar is #f."
    #f)
   ((on-keypress char) "deprecated method.
 handler for keypress of char."
    #f)
   ))

;; extend-with-mousehandling : T -> [Moused T] where T <: [Rendered TextModel]
(define (extend-with-mouse-handling textmodel)
  (define mouse-down #f)
  (define mouse-up #f)
  (define mouse-drag #f)
  (define wnd ((textmodel 'wnd)))
  
  (define (point-in-range? telem pt x y w h)
    ;; #\newline has indefinite right extent
    (or (and (eqv? #\newline telem)
             (<= x (car pt))
             (<= y (cdr pt) (+ y h)))
        (and (<= x (car pt) (+ x w))
             (<= y (cdr pt) (+ y h)))))

  (extend-object textmodel mouse-handling-textmodel
   ((on-mousedown mx my) "handler for mousedown event."
    (set! mouse-down (cons mx my))
    (set! mouse-drag (cons mx my))
    (set! mouse-up #f)
    ((wnd 'update)))
   ((on-mouseup mx my) "handler for mouseup event."
    (set! mouse-drag #f)
    (set! mouse-up (cons mx my))
    ((wnd 'update)))
   ((on-mousedrag mx my) "handler for mousedrag event."
    (cond (mouse-drag
           (set-car! mouse-drag mx)
           (set-cdr! mouse-drag my)))
    ((wnd 'update)))
   ((on-paint g rx ry rw rh)
    (let ((pos-1 #f) 
          (pos-2 #f)
          (self mouse-handling-textmodel))
      (cond 
       ((and mouse-down mouse-drag)
        (for-each-charpos
         self
         g (caar ((self 'font-ranges))) 
         (lambda (char pos pixel-x pixel-y
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
           )
         (lambda (x y h l c p) (unspecified))))
       ((and mouse-down mouse-up)
        (for-each-charpos
         self
         g (caar ((self 'font-ranges)))
         (lambda (char pos pixel-x pixel-y
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
                  (set! pos-2 pos))))
         (lambda (x y h l c p) (unspecified)))
        (set! mouse-down #f)
        (set! mouse-up #f)))
      (cond ((and pos-1 pos-2)
             ((mouse-handling-textmodel 'set-selection!) 
              (min pos-1 pos-2)
              (max pos-1 pos-2)))))
    ((delegate textmodel 'on-paint mouse-handling-textmodel) g rx ry rw rh))))

;; extend-with-scrolling : T -> [Scrolled T] where T <: [Rendered TextModel]
(define (extend-with-scrolling textmodel)
  (define wnd ((textmodel 'wnd)))
  (define (count-newlines-in string)
    (length (filter (lambda (x) (char=? x #\newline)) (string->list string))))
  ;; String Nat -> [Maybe Nat]
  (define (index-after-line-count text line-count)
    (let loop ((i 0) (j 0))
      (cond ((= i (string-length text))
             #f) ;; if we return #f, then there aren't line-count lines in text
            ((= j line-count)
             i)
            (else
             (loop (+ i 1)
                   (+ j (if (char=? #\newline (string-ref text i)) 1 0)))))))
  (define (line-count self)
    (count-newlines-in ((self 'textstring))))

  (define (cursor-line self)
    (let ((mytext ((self 'textstring)))
          (pos (call-with-values (self 'selection) (lambda (s e) s))))
      (do ((i 0 (+ i 1))
           (j 0 (+ j (if (char=? #\newline (string-ref mytext i)) 1 0))))
          ((= i pos)
           j))))
  
  (define first-line-idx 0)

  ;; Bad rep; scrolling op's take O(n) time (where n is the size of
  ;; the entirety of the text).  A doubly linked list of lines may be
  ;; better.  Or even a pair of singly linked list (where the first is
  ;; kept in reverse order).  But this is simple prototype code.

  (extend-object textmodel scrollable-textmodel
   ((first-line-index) "=> line offset according to vertical scrollbar."
    first-line-idx)
   ((set-textstring! string)
    (set! first-line-idx (min first-line-idx (count-newlines-in string)))
    ((delegate textmodel 'set-textstring! scrollable-textmodel) string))
   ((visible-offset)
    (let* ((text ((scrollable-textmodel 'textstring)))
           (nlines (inexact->exact (ceiling ((scrollable-textmodel 'count-visible-lines)))))
           (start (index-after-line-count text first-line-idx))
           (start (or start (string-length text)))
           (finis (index-after-line-count text (+ first-line-idx nlines)))
           (finis (or finis (string-length text))))
      '(begin (display `('visible-offset nlines: ,nlines
					start: ,start
					finis: ,finis))
	     (newline))
      start))
   ((on-cursor-reposition)
    (let* ((text ((scrollable-textmodel 'textstring)))
           (vlinesf ((scrollable-textmodel 'count-visible-lines)))
	   (vlines-fire (inexact->exact (floor vlinesf)))
	   (vlines-measure (inexact->exact (ceiling vlinesf)))
           (start (index-after-line-count text first-line-idx))
           (start (or start (string-length text)))
           (nlines (min vlines-fire
			(count-newlines-in
			 (substring text start (string-length text)))))
           (finis (index-after-line-count text (+ first-line-idx nlines)))
           (finis (or finis (string-length text))))
      (call-with-values (lambda () ((scrollable-textmodel 'selection)))
        (lambda (s e)
          ;; This is simpler than the prefactored code (which overrode
          ;; all of the cursor related operations with advise on when
          ;; and how to scroll).  It's also an improvement, because
          ;; when the cursor goes offscreen, it attempts to scroll to
          ;; center it (rather than going up or down by one line).
          ;; But it still isn't quite right; should handle selections
          ;; as well (that will require some serious redesign though).
          (cond ((and (= s e)
                      (< s start))
                 (let* ((cursor-lines 
                         (count-newlines-in (substring text s start)))
                        (cursor-lines
                         (+ ; cursor-lines
			    (min nlines (quotient vlines-measure 2)))))
                   '(begin (format #t "Attempting to scroll ~a lines back"
                                  cursor-lines)
                          (newline))
                   ((wnd 'attempt-scroll) 'vertical (- cursor-lines))))
                ((and (= s e)
                      (<= s (string-length text))
                      (<= finis s)
                      (= nlines vlines-fire))
                 (let* ((cursor-lines
                         (count-newlines-in (substring text finis s)))
                        (cursor-lines
                         (+ cursor-lines (quotient vlines-measure 2))))
		   '(begin (format #t "Attempting to scroll ~a lines forward"
                                  cursor-lines)
                          (newline))
                   ((wnd 'attempt-scroll) 'vertical cursor-lines)))))))
    ((delegate textmodel 'on-cursor-reposition scrollable-textmodel)))
   ((on-hscroll new-idx event-type) "handler horizontal scroll to new-idx." 
    #f)
   ((on-vscroll new-idx event-type) "handler vertical scroll to new-idx."
    (set! first-line-idx new-idx)
    ((wnd 'update)))
   ((horizontal-scrollbar) #f)
   ((vertical-scrollbar)
    ;; These do not have to be in pixels to be meaningful; we as the
    ;; client select our own unit of measurement, and are then
    ;; responsible for using it consistently.  In this case, we are
    ;; using a line as the measurement grain, (and I suppose that
    ;; the horizontal scrollbars will use a column as the grain).
    ;; When image support is added, these grains might not remain
    ;; appropriate.
    (let* ((my-line-count (line-count scrollable-textmodel))
           (visible-lines 
	    (inexact->exact (floor ((scrollable-textmodel 'count-visible-lines)))))
           (max-val (max 0 (- my-line-count visible-lines))) ;; XXX (buggy)
           (max-val my-line-count)) ;; questionable but easier to work with
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
   ))

;; extend-with-text-coloring 
;;                    : T -> [Colorable T] where T <: [Rendered TextModel]
(define (extend-with-text-coloring textmodel)
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

  (define (clear-transient-state!) 
    (set! transient-background-col-ranges '())
    (set! transient-foreground-col-ranges '()))
  (define (clear-stable-state!)
    (set! stable-background-col-ranges '())
    (set! stable-foreground-col-ranges '()))
  (define (update-stable-ranges! change-start-incl 
                                 change-finis-excl 
                                 remain-delta)
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



  (extend-object textmodel colorable-textmodel
   ((foreground-color pos) "=> col for text at index pos."
    (or (lookup-col pos transient-foreground-col-ranges)
        (lookup-col pos stable-foreground-col-ranges)
        ((delegate textmodel 'foreground-color colorable-textmodel) pos)))
   ((background-color pos) "=> col for background at index pos."
    (or (lookup-col pos transient-background-col-ranges)
        (lookup-col pos stable-background-col-ranges)
        ((delegate textmodel 'background-color colorable-textmodel) pos)))
   ((on-cursor-reposition)
    ((delegate textmodel 'on-cursor-reposition colorable-textmodel))
    (clear-transient-state!))
   ((set-textstring! string)
    (clear-stable-state!)
    ((delegate textmodel 'set-textstring! colorable-textmodel) string))
   ((insert-char-at-point! char)
    (clear-transient-state!)
    (call-with-values (colorable-textmodel 'selection)
      (lambda (pos end) 
        (let ((end (if (= pos end) (+ pos 1) end)))
          (update-stable-ranges! pos end (- end pos)))))
    ((delegate textmodel 'insert-char-at-point! colorable-textmodel) char))
   ((delete-char-at-point!)
    (call-with-values (colorable-textmodel 'selection)
      (lambda (pos end) 
        (let ((pos (if (= pos end) (max 0 (- end 1)) pos)))
          (update-stable-ranges! pos end (- pos end)))))
    ((delegate textmodel 'delete-char-at-point! colorable-textmodel)))
   ((color-foreground-transiently! start-incl finis-excl col) "Colors text
 range [start-incl,finis-excl) using color col.
 Meant to be used for hints of color that will not last beyond next editor
 state chage (cursor movements, text edits)."
    (set! transient-foreground-col-ranges
          (cons (list start-incl finis-excl col) 
                transient-foreground-col-ranges)))
   ((color-background-transiently! start-incl finis-excl col) "Colors behind
 text range [start-incl,finis-excl) using color col.
 Meant to be used for hints of color that will not last beyond next editor
 state chage (cursor movements, text edits)."
    (set! transient-background-col-ranges
          (cons (list start-incl finis-excl col) 
                transient-background-col-ranges)))
   ((color-foreground-stably! start-incl finis-excl col) "Colors text 
 range [start-incl,finis-excl) using color col.
 Meant to be used for colors that should persist through cursor and 
 changes to unrelated pieces of text."
    (set! stable-foreground-col-ranges
          (cons (list start-incl finis-excl col) 
                stable-foreground-col-ranges)))
   ((color-background-stably! start-incl finis-excl col) "Colors behind text
 range [start-incl,finis-excl) using color col.
 Meant to be used for colors that should persist through cursor and 
 changes to unrelated pieces of text." 
    (set! stable-background-col-ranges
          (cons (list start-incl finis-excl col) 
                stable-background-col-ranges)))

   ((foreground-colors) "debugging method.  Scheduled for deletion."
    (list transient-foreground-col-ranges
          stable-foreground-col-ranges
          default-foreground-col))
   ((background-colors) "debugging method.  Scheduled for deletion."
    (list transient-background-col-ranges
          stable-background-col-ranges
          default-background-col))
   ))

;; extend-with-auto-indentation : T -> T where T <: [Keyed T]
;; This extension only advises existing methods; it introduces no new methods.
(define (extend-with-auto-indentation textmodel)
  (define (reindent-according-to-suggestion self)
    (require "Experimental/scheme-source")
    (let ((text ((self 'textstring))))
      (call-with-values (lambda () ((self 'selection)))
        (lambda (beg end) 
          ;; 1. if beg = end, then we want to indent cursor's line
          ;; 2. if beg < end, then we want to indent selected region 
          ;;               (I think)
          ;; For now, just assume beg = end; I can add support for 
          ;; the case (2) later.
          
          (let* (;; search backward for the newline
                 (line-start
                  (do ((i beg (- i 1)))
                      ((or (= i 0)
                           (char=? #\newline (string-ref text (- i 1))))
                       i)))
                 ;; search forward for non-whitespace; don't go past a newline
                 (content-start 
                  (do ((i line-start (+ i 1)))
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
            ;; XXX the behavior set-textstring! method in all the subclasses 
            ;; XXX is not as well fleshed out as the cursor manipulation 
            ;; XXX methods; perhaps consider reexpressing this in terms of 
            ;; XXX local edits to the text via the cursor.
            ((self 'set-textstring!) 
             (string-append (substring text 0 line-start)
                            (make-string indent #\space)
                            (substring text content-start 
                                       (string-length text))))
            
            ;; fix the cursor's position
            ((self 'set-selection!) (- beg delta) (- end delta))
            
            )))))
    
  (extend-object textmodel auto-indenting-textmodel
   ((on-keydown mchar  sym mods)
    ((delegate textmodel 'on-keydown auto-indenting-textmodel) mchar sym mods)
    (case sym
      ((enter) 
       (reindent-according-to-suggestion auto-indenting-textmodel))
      ((tab) 
       (reindent-according-to-suggestion auto-indenting-textmodel))
      ))))

;; extend-with-paren-matching : T -> T where T <: [Colorable T]
;; This extension only advises existing methods; it introduces no new methods.
(define (extend-with-paren-matching textmodel)
  (require "Experimental/scheme-source")

  (extend-object textmodel paren-matching-textmodel
   ((on-cursor-reposition)
    ((delegate textmodel 'on-cursor-reposition paren-matching-textmodel))
    (let* ((matched-col (name->col "Orange"))
           (unmatch-col (name->col "Red"))
           (ea paren-matching-textmodel)
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
            ((ea 'color-background-transiently!) e (+ e 1) unmatch-col)))))))
    ((((paren-matching-textmodel 'wnd)) 'update))
    )))

(define (extend-with-file-handling textmodel)
  (define current-filename #f)

  (extend-object textmodel file-handling-textmodel
   ((load-file-cmd)
    (cond ((open-file-chooser-dialog (current-directory)
                                     (list (list "Scheme Files" 
                                                 "sch" "scm" "ss")
                                           (list "All Files" "*")))
           => (lambda (name)
                ((file-handling-textmodel 'load-file) name)))))
   ((save-file-cmd)
    (cond (current-filename 
           ((file-handling-textmodel 'save-file-as) current-filename))
          (else ((file-handling-textmodel 'save-file-as-cmd)))))
   ((save-file-as-cmd) 
    (cond ((save-as-file-chooser-dialog (current-directory)
                                        (list (list "Scheme Files"
                                                    "sch" "scm" "ss")
                                              (list "All Files" "*")))
           => (lambda (name)
                ((file-handling-textmodel 'save-file-as) name)))))
   ((load-file filename)   
    (set! current-filename filename)
    (call-with-input-file filename
      (lambda (filein)
        (let* ((chars (do ((c (read-char filein) (read-char filein))
                           (l '() (cons c l)))
                          ((eof-object? c) (reverse l))))
               (text (list->string chars))
               (self file-handling-textmodel))
          ((self 'set-textstring!) text)
          ((self 'set-selection!) 0 0)
          ))))
   ((save-file-as filename) 
    (set! current-filename filename)
    (call-with-output-file filename
      (lambda (fileout)
        (let* ((self file-handling-textmodel)
               (text ((self 'textstring))))
          (do ((i 0 (+ i 1)))
              ((= i (string-length text)))
            (write-char (string-ref text i) fileout))))))
   ))

(define (make-simplest-backing-agent wnd textmodel)
  (extend-object textmodel simplest-backing-agent
   ((on-keyup mchar  sym mods)
    (let ((self simplest-backing-agent))
      (case sym
        ((enter)       ((self 'insert-char-at-point!) #\newline))
        ((left)        ((self 'cursor-left!)))
        ((right)       ((self 'cursor-right!)))
        ((up)          ((self 'cursor-up!)))
        ((down)        ((self 'cursor-down!)))
        ((back delete) ((self 'delete-char-at-point!))))))
   ((on-keypress char)
    (case char
      ((#\backspace #\return #\esc #\tab) 'do-nothing)
      (else 
       ((textmodel 'insert-char-at-point!) char)))))) ;; XXX should be self

(define (make-auto-indenting-agent wnd textmodel)
  (extend-with-paren-matching (extend-with-auto-indentation textmodel)))

(define (make-scheme-editor-agent wnd textmodel)
  (extend-with-file-handling 
   (extend-with-paren-matching
    (extend-with-auto-indentation textmodel))))


(define (make-textview wnd)
  (define (o . fcns)
    (lambda (x)
      (if (null? fcns) 
          x
          ((car fcns) ((apply o (cdr fcns)) x)))))
  (define extend-with-rendering (rendering-extender wnd))
  
  ((o extend-with-text-coloring
      extend-with-scrolling
      extend-with-mouse-handling
      extend-with-keystroke-handling
      extend-with-rendering)
   (make-textmodel)))

(define (editor-agent-maker wnd*textmodel->agent)
  (lambda (wnd width height)
    (let* ((textview (make-textview wnd))
           (agent (wnd*textmodel->agent wnd textview)))
      agent)))

(define (make-editor-agent wnd width height)
  ((editor-agent-maker (lambda (w tm) tm)) wnd width height))

(define (extend-with-repl textmodel)
  ;; prompt-idx marks the start of the text that we will use as input
  ;; to the reader for the REPL.
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
               ;; does not support constructing custom textual ports
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

  (define (evaluate-first-sexp-after-prompt alternative-behavior)
    ;; I'm going to hack this up by making a custom port for reading
    ;; from the text.  If the reader ever requests to read past the
    ;; end of the input text, then we abandon the read attempt (via an
    ;; escape continuation) and just insert the #\newline.
    ;; TODO: add support for catching errors during the read, printing
    ;; the error message in the GUI, and resetting REPL.
    ;; TODO: figure out how this is going to integrate with Image
    ;; support.  (Perhaps even at this level, images will be rendered
    ;; to ASCII and the text view will be reponsible for interpreting
    ;; the code sequences.
    (let* ((ea textmodel)
           (text ((ea 'textstring)))
           (subtext (substring text prompt-idx (string-length text)))
           ;; TODO: Add whitespace to end of text, so that this will
           ;; work independently of whether (ea 'textstring) returns
           ;; text ending with newline
           (orig-port (open-string-input-port subtext))
           
           (repl-binary-output-port 
            (let ((write! 
                   (lambda (bv start count)
                     (if (= count 0)
                         0
                         (let* ((b (bytevector-ref bv start))
                                (c (integer->char b))
                                (s (string c)))
                           (insert-string-at-point/bump! ea s)
                           1)))))
              (make-custom-binary-output-port 
               "REPL OUTPUT" write! #f #f #f)))
           (repl-output-port 
            (transcoded-port repl-binary-output-port (native-transcoder)))
           
           (mrr (maybe-read orig-port #\newline)))
      
      '(begin (write `((prompt-idx: ,prompt-idx)
                       (subtext: ,subtext)
                       (mrr: ,mrr)))
              (newline))
      
      (case (car mrr)
        ((evaluate) 

         ;; Now that we've read it the user's text, we should
         ;; bump the prompt over it.
         (bump-prompt! (- (string-length subtext) 1))

         ;; XXX during evaluation, should override current-input-port
         ;; and current-output-port here so that user code will
         ;; side-effect the REPL window or something similar.
         ;; (DrScheme's approach to this is very nice IMHO.)
         ;; XXX during evaluation, we also should catch errors and
         ;; print them to the REPL window.
         (let* ((sexp (cadr mrr))
                (count-chars-read (caddr mrr)))
           
           (call-with-current-continuation 
            (lambda (escape-from-eval)
              (let* ((orig-error-handler (error-handler))
                     (repl-error-handler 
                      (lambda args
                        (parameterize ((error-handler orig-error-handler))
                          (decode-error args repl-output-port)
                          (escape-from-eval 'ignore-me))))
                     (val (parameterize ((error-handler repl-error-handler))
                            (eval sexp my-own-env)))
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
                             (lambda () ((repl-printer) val strport)))))))))

                ;; Write rendered value to textview, bumping prompt over it.
                (insert-string-at-point/bump! ea valstr))))
           
           (let* ((promptstr
                   (call-with-output-string
                    (lambda (strport)
                      ((repl-prompt) (repl-level) strport))))
                  (ignore 
                   '(begin (write `((count-chars-read: ,count-chars-read)
                                    (subtext len: ,(string-length subtext))
                                    (subtext: ,subtext)))
                           (newline))))
             
             ;; Print new prompt
             (insert-string-at-point/bump! ea promptstr))
           
           (let* ((remaining-input
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
             
             ;; XXX if there is still data on orig-port, then we should
             ;; probably propagate it down past the new prompt.  (This
             ;; does not match DrScheme's behavior though; I believe it
             ;; evaluates greedily until there aren't any S-exps left;
             ;; which is another option for us.)
             '(begin (write `(propagating remaining-input: ,remaining-input))
                     (newline))
             (insert-string-at-point! ea remaining-input))
           ))
        ((eof)      
         (alternative-behavior))
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
  
  (extend-object textmodel repl-agent
   ((on-keydown mchar sym mods)
    (case sym
      ((enter) 
       ;; If user hits enter and we're at the end of the foremost
       ;; S-exp, then evaluate it.  
       ;; If user hits enter and we're selecting text before the
       ;; prompt, then copy the text after the prompt and then attempt
       ;; to evaluate it.
       ;; XXX what should REPL do if user hits enter while in middle
       ;; of text post prompt?
       ;; XXX what shoudl REPL do if user hits enter while selecting
       ;; text post prompt?
       '(begin (write `((on-keydown ,sym)
                       (prompt-idx: ,prompt-idx)
                       (text: ,((repl-agent 'textstring)))
                       (len: ,(string-length ((repl-agent 'textstring))))
                       ))
              (newline))
       (let* ((ea repl-agent)
              (default-behavior-thunk
                (lambda () 
                  ((delegate textmodel 'on-keydown ea) mchar sym mods))))
         (call-with-values (lambda () ((ea 'selection)))
           (lambda (beg end)
             (cond 
              ((<= beg prompt-idx)
               (let* ((text ((ea 'textstring)))
                      (subtext (substring text beg end))
                      (end (string-length text)))
                 ;; Move prompt to end of buffer
                 ((ea 'set-selection!) (- end 1) (- end 1))
                 (insert-string-at-point! ea subtext)))
              (else
               (let* ((text ((ea 'textstring)))
                      (text-end (string-length text)))
		 ;; XXX yet another fencepost-fix that was necessary
		 ;; after I got rid of the "every textmodel ends with
		 ;; newline" invariant.  (Though the reality is that
		 ;; this probably should not require that end is at
		 ;; the text-end, but rather that there is no
		 ;; non-whitespace content in between end and
		 ;; text-end...)
                 (cond ((= end text-end)
			(default-behavior-thunk)
			(evaluate-first-sexp-after-prompt 
			 (lambda () 'no-alternative-behavior)))
		       (else 
			(default-behavior-thunk))))))))))
      (else
       ;; Pass the buck to the underlying agent.
       ((delegate textmodel 'on-keydown repl-agent) mchar sym mods))))

   ;; The REPL will silently drop attempts to edit text before the
   ;; prompt.
   ;; 
   ;; Text edits that *do* take effect do not affect the prompt-idx,
   ;; because such edits are guaranteed to be part of the user input
   ;; that we're still waiting to process, and therefore we can leave
   ;; prompt-idx where it is.
   ((insert-char-at-point! char) 
    (cond ((call-with-values (lambda () ((repl-agent 'selection)))
             (lambda (beg end) (<= prompt-idx beg)))
           ((delegate textmodel 'insert-char-at-point! repl-agent) char))))
   ((delete-char-at-point!) 
    (cond ((call-with-values (lambda () ((repl-agent 'selection)))
             (lambda (beg end) (<= prompt-idx beg)))
           ((delegate textmodel 'delete-char-at-point! repl-agent)))))

   ((prompt!) "Prints a prompt at the end of the text buffer."
    (let* ((ea repl-agent))
      (call-with-values (lambda () ((ea 'selection)))
        (lambda (beg end) ((ea 'set-selection!) end end)))
      (let ((str (call-with-output-string 
                  (lambda (strport)
                    ((repl-prompt) (repl-level) strport)))))
        (insert-string-at-point/bump! ea str))))
   ))

(define (make-read-eval-print-loop-agent wnd textmodel)
  (extend-with-repl 
   (extend-with-paren-matching
    (extend-with-auto-indentation textmodel))))
