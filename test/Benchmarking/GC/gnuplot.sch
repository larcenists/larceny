(require "Experimental/temp-files")

;; Notes on gnuplot syntax
;; # comments rest of line; see also "set datafile commentschars"

;; <cmd> ::= <set-coord> [<system>] <x>, [<system>] <y> [, [<system>] <z>]
;; <system> ::= first | second | graph | screen | character
;; <set-coord> ::= set arrow | set key | set label | set object

;; meaning of <system>: places coord in system defined by:
;; - first  : left+bottom axes [default for <x>; (y's inherited from <x>)]
;; - second : top+right axes
;; - graph  : area within axes; 0,0 is bottom left, 1,1 top right
;; - screen : area on screen; 0,0 bottom left, 1,1 top right
;; - character : font chars widths/heights from bottom left of screen (0,0)

;; (There are other details on absolute/relative, logarithmic axis, timeseries)

;; ----

;; There's something about "Datastrings" which apparently are just delimited
;; strings appearing as entries in the data file... the example given in the
;; manual does not have clear semantics, and I cannot tell if the fink 
;; gnuplot is even configured to support "Datastrings"

;; ---- 

;; There are Environment Variables of potential interest.
;; Most do not seem of interest to me at the moment.

;; ----

;; "In general, any mathematical expression accepted by C, FORTRAN,
;; Pascal, or BASIC is valid."

;; Complex constants are expressed as {<real>,<imag>}; 
;; (handled for non-zero imaginary parts in fmt-x below)

;; There are "simple" infix operations on strings (concatenation via
;; "." operator and equality comparison via "eq" operator).  One can
;; probably hack in such expressions via string->symbol, but is there
;; any reason to do so?

;; Unary functions:
(define gnuplot-unary-function-names
  '(abs acos acosh arg asin asinh atan atanh besj0 besj1 besy0 besy1 
    ceil cos cosh erf erfc exp floor gamma inverf imag invnorm int
    iambertw lgamma log log10 norm rand real sgn sign sinh sqrt 
    tan tanh strlen system words 
    column ; defined (deprecated)
    exists stringcolumn timecolumn
    tm_hour tm_mday tm_min tm_mon tm_sec tm_wday tm_yday tm_year 
    valid))

;; Other arity functions:
;; atan2(y,x) ibeta(p,q,x) igamma(a,x) 
;; gprintf("format",x) sprintf("format",x,...)
;; strstrt("string","key") substr("string",beg,end) word("string",n)

(define gnuplot-unary-prefix-operator-names '(- + ~ ! $))
(define gnuplot-unary-postfix-operator-names '(!))
(define gnuplot-binary-operator-names 
  '(** * / % + - == != < <= > >= & ^ \| && \|\| \. eq ne))
(define gnuplot-ternary-operator-names
  '((? :)))

'(gnuplot '(( f(x) = 0 <= x && x < 1 ? 
                     sin(x) : 1 <= x && x < 2 ? 
                     1 / x : 1 / 0 )
            ( plot f(x) )))

;; There are some built-in variables: pi, MOUSE_* GPVAL_* FIT_*

;; cmd ::= ... | 
;;         <func-name>( <dummy1> [,<dummy2>] ... [,<dummy5>] ) = <expression>
;; cmd ::= ... | <variable-name> = <constant-expression>

;; ----

;; gnuplot terminology (taken from Section 14 of the gnu plot manual).
;; A "page" or "screen" is entire addressable area, and is made up of plots
;; A "plot" is defined by an abscissa and an ordinate (?), plus margins + text
;;          and contains one graph
;; A "graph" is defined by an abscissa and an ordinate (?), made up of lines
;; A "line" is a single function or data set.  (It is also a plotting style.)
;; A "title" is either a plot title, line title, or key title.
;; A 2-d graph has <= 4 named axes "x" (bot), "y" (lft), "x2" (top), "y2" (rgt)
;; A 3-d graph has <= 3 named axes "x", "y", "z"
;; A "record" is a single line from a data set
;; A "point" is datum extracted from a record
;; A "datablock" is a set of points from consecutive records

;; ----

;; linetype lt rgb palette cbrange linecolor lc 
;; set style line linestyle ls

;; ----

;; bind mouse MOUSE_X MOUSE_Y MOUSE_X2 MOUSE_Y2 
;; MOUSE_BUTTON MOUSE_SHIFT MOUSE_ALT MOUSE_CTRL

;; ----

;; plot (2d) splot (3d) replot (append)
;; set polar, using 
;; axes, set xlabel, set logscale
;; set isosamples, set contour, set cntrparam

;; ----

;; (we won't use .gnuplot since we can pass our own "initialization"
;;  file explicitly)

;; ----

;; String-producing expressions are "usually" acceptable in contexts where
;;   string constants appear.
;; Integers are promoted to strings when fed to string concat operator "."
;; "." "eq" "ne"
;; gprintf sprintf 
;; substrings formed via [begin:end] operator; first character has index 1,
;; can omit begin or end (or put "*" in their place) to get actual begin/end

;; ----

;; Substitution 

;; Occurrences of `pgm args ...` on a command line be replaced with
;; the output from the host OS executing pgm on args ...

;; (I could consider supporting this by changing the meaning of lists
;; when their head is the symbol quasiquote, but do not want to do so
;; yet...)

;; set macros enables some other feature involving dynamic generation
;; of gnuplot command lines from string variables via @ character.

;; ----

;; Syntax

;; options and parameters are separated by spaces
;; lists and coordinates are separated by commas
;; ranges are separated by colons and enclosed in square brackets
;; text and file names are enclosed in quotes
;; "a few miscellaneous things" are enclosed in parentheses
;; - sets of explicit tics
;; - computations in using filter of plotting and fit commands
;; curly braces "are used for a few special purposes"
;; - text to be specially processed by some terminals (postscript)
;; - complex numbers {3,2} = 3 + 2i
;; semicolons are used to separate commands given on a single command line

;; one can use single quotes to delimit strings; embedded single quotes
;; in such a string are achieved by writing two single quote marks.
;; (should not be relevant for my script generation)

;; ----

;; Time/Data

;; set xdata time, set ydata time, set timefmt 
;; show xrange, set format

#|
;; Here is a terrible expression I came up with, after playing with
;; the manual's (terrible) example.  (Terribleness was inherited.)
(gnuplot
 (lambda (files) `((set xdata time) 
                   (set timefmt "%m/%d/%y")
                   ;; Note hack of using #\[ #\] to encode square brackets
                   (set xrange #\[ "03/21/95" : "03/22/95" #\]) 
                   (set format x "%m/%d") 
                   (set timefmt "%m/%d/%y %H:%M") 
                   (plot ,files using ,(string->symbol "1:3")))) 
 (map (lambda (e) (map string->symbol e)) 
      '(("03/21/95" "10:00" "6.02e23") 
        ("03/22/95" "09:00" "6.10e23"))))
|#

;; ----

;; Commands

(define gnuplot-command-names
  '(cd call clear exit fit help history if load lower
    pause plot print pwd quit raise replot reread reset
    save set show shell splot system test unset update))

;; cd '<directory-name>'                     	changes working directory
;; call "<input-file>" <param-0> .. <param-9>	loads file, subst's $N arg
;; clear                                     	erases current output
;; exit [gnuplot]                            	ends curr. input stream
;; fit [rngs] <fcn> '<dat>' [mods] via <vars>	fits (open) fcn to data (SSR)
;; help [topic]                              	displays on-line help
;; history [count] ["output"]                	lists past history
;; history ?<token-or-string>                	history w/ search filter
;; history !<token-or-string>                	executes last found entry
;; if (<cond>) <cmd>                         	predicated execution
;; if (<cond>) <cmd> ; else <cmd>            	branched execution
;; load "<input-file>"                       	loads file
;; lower [plot_window_number]                	puts window at bottom of stack
;; pause <time> ["<string>"]                 	display text and then wait
;; pause mouse [<endcond>, ...] ["<string>"] 	display text and wait for click
;; plot [<rngs>] [<fcn> | "<dat>" [mods]] ...	2d plotting of function / data
;; plot '<dat>' binary <binary list>
;; plot '<dat>' matrix
;; plot '<dat>' index <index list>
;; plot '<dat>' every <every list>
;; plot '<dat>' thru <thru exp
;; plot '<dat>' using <using list>
;; plot '<dat>' smooth <option>

;; ----

;; THE CODE

;; fmt-x : Port ElemSexp -> unspecified
;; Renders x to p in a manner compatible with gnuplot's parser.
;; 
;; A number is printed according to what gnuplot's doc says it can handle.
;; A list is delimited by parentheses (except for when it appears as a 
;;     vector element) and its elements are space separated.
;; A vector is undelimited and its elements are comma separated
;; A symbol is displayed (e.g. can sneak in tokens like 2:3 via string->symbol)
;; A string is written (its content is delimited by quotation marks)
;; 
;; These components combine to make for a reasonable way to write gnuplot
;; scripts as s-exps.
(define (fmt-x p x)
  (cond ((string? x) 
         (format p "~s " x))
        ((and (complex? x) (not (zero? (imag-part x))))
         (format p "{~a,~a} " (real-part x) (imag-part x)))
        ((and (rational? x) (not (integer? x)))
         (format p "~a " (exact->inexact x)))

        ;; Idea: interpolate commas for all vector arguments in
        ;; input (so that I can use true s-exp syntax at high-level and
        ;; get isomorphic corresponding gnuplot script.)

        ((vector? x)
         (case (vector-length x)
           ((0) "")
           (else (let loop ((i 0))
                   (cond ((= i (- (vector-length x) 1))
                          (fmt-x/unwrapped p (vector-ref x i)))
                         (else 
                          (fmt-x/unwrapped p (vector-ref x i))
                          (format p ", ")
                          (loop (+ i 1))))))))
        ((list? x) 
         (format p "( ")
         (for-each (lambda (e) (fmt-x p e)) x)
         (format p ")"))
        (else
         (format p "~a " x))))

;; fmt-x/unwrapped : Port Sexp -> unspecified
;; Renders x to p, unwrapping the *first* level of list structure
;; (assumes the caller is already delimiting the list in some
;;  other manner, such as via surrounding commas or line breaks)
(define (fmt-x/unwrapped p x)
  (cond
   ((list? x) 
    (for-each (lambda (e) (fmt-x p e)) x))
   (else
    (fmt-x p x))))

;; fmt-string : Sexp -> String
;; Allows interactive experimentation with fmt.
(define (fmt-string x)
  (call-with-output-string (lambda (p) (fmt-x p x))))

;; A Dataset is a Listof[DataRecord]

;; A DataRecord is an Sexp
;; - a Number is just a single value
;; - a list is a record of its contents
;;   as a special case, an empty list rendered as a blank line

;; gnuplot : (Vectorof[String] -> Listof[PlotCmd]) DataSet ... -> unspecified
;; gnuplot : Dataset ... -> unspecified

;; gnuplot/interactive like above, but brings up gnuplot repl as well

(define (gnuplot             . args) (apply gnuplot/core #f args))
(define (gnuplot/interactive . args) (apply gnuplot/core #t args))

(define (gnuplot/core interact? . args)
  (let* ((data-files->plot-cmds (if (procedure? (car args))
                                    (car args)
                                    (lambda (files) `((plot ,files)))))
         (args (if (procedure? (car args)) (cdr args) args))
         (plot-file (make-temporary-file "go~a.plot")))
    (define (build-file cmds port)
      (let ((p (lambda (x) (fmt-x/unwrapped port x) (newline port))))
        (for-each p cmds)))
    (define (run-gnuplot . files)
      (system (apply string-append
                     "gnuplot"
                     (append (map (lambda (f) (string-append " " f))
                                  files)
                             (if interact? '( " - " ) '())
                     ))))
    (define (cleanup-file f)
      (delete-file f))

    (let ((data-files
           (map (lambda (data)
                  (let ((this-data (make-temporary-file "data~a.dat")))
                    (call-with-output-file this-data
                      (lambda (data-port)
                        (build-file data data-port)))
                    this-data))
                args)))

      (call-with-output-file plot-file
        (lambda (plot-port) 
          (build-file (data-files->plot-cmds (list->vector data-files))
                      plot-port)))
      (let ((result (run-gnuplot plot-file)))
        (cond ((zero? result)
               (for-each cleanup-file data-files)
               (cleanup-file plot-file)
               (values))
              (else 
               (cons plot-file data-files)))))))


#|
;; SAMPLE USAGES:
(gnuplot '((1 2) (3 4)) '((10 20) (30 40)))

(gnuplot (map (lambda (x) (list x (* x x))) (iota 20)) 
         (map (lambda (x) (list x (* 2 x))) (iota 10)))

(gnuplot (lambda (files) 
           `((plot x**2 + x*2 + 1) (replot cos(x)) (replot ,files))) 
         '((1 2) (3 4)) '((10 20) (30 40)))
|#
