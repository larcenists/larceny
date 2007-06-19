;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; $Id$
;
; Default parameter settings for all heap images.
; Loaded by *-heap.sch files and called just before dumping a heap.
;
; FIXME:  This shouldn't be necessary.
; When a *-heap.fasl file is loaded, the load procedure
; temporarily changes the reader mode.  Currently, the
; altered reader mode is still in effect when the heap
; is dumped.  That should change eventually, but putting
; the default settings in a common file is a good thing
; regardless.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-parameter-defaults-for-a-standard-heap!)

  ; compiler switches
  ;
  ; FIXME: bound? isn't available in all heaps

  ;(if (bound? compiler-switches)
  ;    (compiler-switches 'fast-safe))

  ; reader mode

  (case-sensitive? #f)
  (read-r6rs-flags? #t)
  (read-larceny-weirdness? #t)
  (read-traditional-weirdness? #f)
  (read-mzscheme-weirdness? #f)

  (recognize-keywords? #f)
  (recognize-javadot-symbols? #f)

  ; FIXME: deprecated flags

  (read-square-bracket-as-paren #t))
