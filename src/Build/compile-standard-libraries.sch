(define files-to-compile 
  '(
    "applyhook.sch"
    "apropos.sch"
    "array.sch"
    "array-util.sch"
    ;; "assert.sch"   ;; has syntax
    ;; "autoload.sch" ;; has syntax
    "barrier-sync.sch"     ;; uses non-std syntax (without-interrupts, define-record)
    "box.sch"
    "combinatory.sch"
    "comment.sch"
    ;; "common-syntax.sch" ;; has syntax
    "control.sch"
    "coroutine.sch"
    "debugger.sch"
    ;; "define-record.sch" ;; has syntax
    ;; "define-values.sch" ;; has syntax
    ;; "defmacro.sch"      ;; has syntax
    "docextract.sch"
    ;; "dotimes.sch"       ;; has syntax
    "exec-comment.sch"
    "file-system.sch"
    "file-utils.sch"
    ;; "fluid.sch"         ;; has syntax
    "format.sch"
    "fortune.sch"
    "fqueue.sch"
    "generator.sch"
    "glob.sch"
    "infix-expr.sch"
    "io.sch"
    "list.sch"
    "list-set.sch"
    "match.sch"
    ;; "match-syntax.sch"  ;; has syntax (defmacro)
    ;; "md5.sch"        ;; has syntax
    ;; "monitor.sch"    ;; has syntax
    ;; "mutex.sch"               ;; uses non-std syntax (without-interrupts, define-record)
    ;; "nonblocking-console.sch" ;; uses non-std syntax (cond-expand)
    "number.sch"
    ;; "poll.sch"                ;; uses non-std syntax (cond-expand)
    "pretty.sch"
    ;; "queue.sch"               ;; uses non-std syntax (assert)
    "random.sch"
    "record.sch"
    "regexp.sch"
    "require0.sch"
    "require.sch"
    "sharp-dot.sch"
    ;; "shivers-syntax.sch"   ;; has syntax
    ;; "socket.sch"           ;; uses non-std syntax (cond-expand, define-record)
    "stats.sch"
    "std-autoloads.sch"
    "string.sch"
    "symbol.sch"
    ;; "tasking.sch"          ;; has syntax
    ;; "tasking-with-io.sch"  ;; uses non-std syntax (tasks/without-interrupts)
    ;; "time.sch"             ;; uses non-std syntax (cond-expand)
    "trie.sch"
    "unify.sch"
    "unix.sch"
    "uuencode.sch"
    ;; "word.sch"             ;; has syntax
    ))

(for-each (lambda (file)
            (begin (display `(compiling: ,file)) 
                   (newline))
            (compile-file (string-append "lib/Standard/" file)))
          files-to-compile)
