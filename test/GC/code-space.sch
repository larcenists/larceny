; Collecting the code space (native systems).
;
; Loads a compiled file many times while allocating, keeping each
; procedure it defines for a while, so code is installed, referenced
; from data that gets promoted, and then becomes garbage.  Run it with
;
;     larceny -rrof -size1 1M -annoy-user < code-space.sch
;
; (and under the other collectors).  It prints "code-space: ok"; with
; -annoy-user the collector reports each code collection ("Code space:
; collected by scanning ...", with the pointers to code it cleared in
; garbage and the bytes of code live), which should keep happening and
; stay bounded.  Writes code-space-tmp.sch and .fasl in the current
; directory.

(call-with-output-file "code-space-tmp.sch"
  (lambda (out)
    (write '(define (code-space-f x) (+ x 1)) out)
    (newline out)))
(compile-file "code-space-tmp.sch" "code-space-tmp.fasl")

(define code-space-live         ; permanent data, so there are regions
  (let loop ((i 0) (l '()))
    (if (= i 4000)
        l
        (loop (+ i 1) (cons (make-vector 1000 i) l)))))

(define code-space-keep '())
(define code-space-junk '())

(define (code-space-step i)
  (load "code-space-tmp.fasl")
  (set! code-space-keep (cons code-space-f code-space-keep))
  (set! code-space-junk (cons (make-vector 200 i) code-space-junk))
  (if (= 0 (remainder i 500))
      (begin (set! code-space-keep '())
             (set! code-space-junk '()))))

(do ((i 1 (+ i 1)))
    ((> i 100000))
  (code-space-step i))

(display "code-space: ")
(display (if (and (= (code-space-f 1) 2)
                  (= (length code-space-live) 4000))
             "ok"
             "FAIL"))
(newline)
(exit)
