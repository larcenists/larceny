;; A CompileEntry is one of:
;; - Filename
;; - (list Filename RequireSpec ...) 
;; where a single file is a file that can be compiled without
;; importing syntax, and a list is the file to compile, F, followed by
;; the libraries to load syntax from before compiling F.

;; files-to-compile : [Listof CompileEntry]

(define standard-lib-files-to-compile 
  '(
    "applyhook.sch"
    "apropos.sch"
    "array.sch"
    "array-util.sch"
    ;; "assert.sch"   ;; defines syntax
    ;; "autoload.sch" ;; defines syntax
    ("barrier-sync.sch" tasking define-record)
    "box.sch"
    "combinatory.sch"
    "comment.sch"
    ;; "common-syntax.sch" ;; defines syntax
    "control.sch"
    "coroutine.sch"
    "debugger.sch"
    ;; "define-record.sch" ;; defines syntax
    ;; "define-values.sch" ;; defines syntax
    ;; "defmacro.sch"      ;; defines syntax
    "docextract.sch"
    ;; "dotimes.sch"       ;; defines syntax
    "exec-comment.sch"
    ("file-system.sch" srfi-0 foreign-ctools)
    "file-utils.sch"
    ;; "fluid.sch"         ;; defines syntax
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
    ;; "match-syntax.sch"  ;; defines syntax (via defmacro)
    ;; "md5.sch"        ;; defines syntax
    ;; "monitor.sch"    ;; defines syntax
    ("mutex.sch" tasking define-record)
    ("nonblocking-console.sch" srfi-0)
    "number.sch"
    ("poll.sch" srfi-0)
    "pretty.sch"
    ("queue.sch" assert)
    "random.sch"
    "record.sch"
    "regexp.sch"
    "require0.sch"
    "require.sch"
    "sharp-dot.sch"
    ;; "shivers-syntax.sch"   ;; defines syntax
    ("socket.sch" srfi-0 define-record)
    "stats.sch"
    "std-autoloads.sch"
    "string.sch"
    "symbol.sch"
    ;; "tasking.sch"          ;; defines syntax
    ("tasking-with-io.sch" tasking)
    ("time.sch" srfi-0)
    "trie.sch"
    "unify.sch"
    "unix.sch"
    "uuencode.sch"
    ;; "word.sch"             ;; defines syntax
    ))

(load "src/Build/compile-tools.sch")

(define (compile-libraries-in-dir lib-dir files-to-compile)
  (for-each (lambda (entry)
              (begin (display `(compiling: ,entry)) 
                     (newline))
              (cond 
               ((string? entry) (compile-file (string-append lib-dir entry)))
               ((pair? entry) (compile-file/with-syntax 
                               (string-append lib-dir (car entry))
                               (map (current-library-resolver) (cdr entry))))))
            files-to-compile))

(compile-libraries-in-dir "lib/Standard/" standard-lib-files-to-compile)

(define experimental-lib-files-to-compile
  '(
    ("socket.sch"          srfi-0 common-syntax foreign-ctools)
    ("unix.sch"            srfi-0 foreign-ctools)
    ("unix-descriptor.sch" define-record))) 

(compile-libraries-in-dir "lib/Experimental/"
                          experimental-lib-files-to-compile)

(define ffi-lib-files-to-compile
  '(
    "ffi-i386.sch"
    "ffi-linux-x86.sch"
    "ffi-load.sch"
    "ffi-lower.sch"
    "ffi-macosx.sch"
    "ffi-sparc.sch"
    "ffi-sunos4.sch"
    "ffi-sunos5.sch"
    "ffi-upper.sch"
    "ffi-util.sch"
    "ffi-win32.sch"
    "memory.sch"
    "tramp.sch"))

(compile-libraries-in-dir "lib/Ffi/" ffi-lib-files-to-compile)
