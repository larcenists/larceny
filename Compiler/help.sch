; Simple help system for Larceny

(define (help . rest)
  (if (null? rest)
      (begin (writeln "Help is available on the following topics: ")
	     (display-topics)
	     (writeln "For help on any topic, type (help '<topic>)"))
      (display-help-for (car rest))))

(define *topiclines*
  '((compiling 
     "  compiling          Compiling and assembling Scheme programs.")
    (compiler-switches
     "  compiler-switches  Info about a slew of compiler switches.")
    (general
     "  general            General information about file types etc.")
    (heaps
     "  heaps              Dumping heap images.")))

(define *topics*
  '((compiling 
     "(compile-file <schemefile> &opt <outputfile>)"
     "  Compile the Scheme source file and produce a .fasl file."
     "  This command is context-sensitive: if the compiler switches"
     "  are set to produce code for a non-generational collector, the"
     "  output file has extension .efasl by default."
     ""
     "(compile313 <schemefile> &opt <outputfile>)"
     "  Compile the Scheme source file and produce a .lap file."
     ""
     "(assemble313 <lapfile> &opt <outputfile>)"
     "  Assemble a .lap or .mal source file and produce a .lop file."
     ""
     "(make-fasl <lopfile> &opt <outputfile>)"
     "  Create a fast-load file (.fasl) from the object file."
     "  This command is context sensitive in the same way compile-file is."
     )
    (heaps
     "A bootstrap heap image is created with the dump-heap procedure:"
     ""
     "  (dump-heap <heapname> <object-file> ...)"
     ""
     "This creates a bootstrap heap image containing all the given objects."
     "However, it is not usually called directly; the following two procedures"
     "are typically more convenient:"
     ""
     "  (make-larceny-heap)"
     "    Create a bootstrap heap image for generational collectors,"
     "    using the default makefile. The heap is named \"larceny.heap\"."
     "  (make-larceny-eheap)"
     "    Create a bootstrap heap image for non-generational collectors,"
     "    using the default makefile. The heap is names \"larceny.eheap\"."
     ""
     "Beware that neither of these commands cleans up .lop files compiled"
     "for a different collector type; you should first 'make lopclean' from"
     "the Unix shell if you are building a heap of a kind different from"
     "the one you built last."
     ""
     "For details, see the file Lib/makefile.sch."
     )
    (compiler-switches
     "There are a number of compiler and assembler switches, and a bit of"
     "chaos in how they are used. The command (compiler-switches) lists"
     "all switches and their current settings. The switches are:"
     ""
     ; @@ Will (the next three lines were updated)
     "  (integrate-usual-procedures)  Generate in-line code for primitives."
     "  (local-optimizations)    Suppress redundant instructions."
     "  (benchmark-mode)         Makes global self-recursion faster."
     "  unsafe-mode              Turn off type checking for many primitives."
     "  inline-cons              Allocate pairs in-line."
     "  listify?                 Produce listing of MAL code (obsolete)."
;     "  fast-pop                 Do not check for stack underflow." ; @@ Will
     "  inline-assignment        Generate in-line generation check."
     "  register-transactions-for-side-effects   Generate code for gen. gc."
     "  enable-peephole?         Perform peephole optimization."
     "  enable-singlestep?       Insert singlestepping support code (slow)."
     "  assume-short-distance-to-call  Manual branch optimization..."
     "  emit-undef-check?        Check references to global variables."
     "  generate-global-symbols? Let each global cell contain its name."
     ""
     ; @@ Will (the next paragraph was updated)
     "All switches can be set to #t to enable the effect explained above,"
     "or to #f to disable the effect.  If the switch is a variable, its"
     "value can be set by an assignment.  If the switch is a procedure,"
     "it can be set by passing an argument as in (benchmark-mode #f)."
     "Please note that assume-short-distance-to-call is not always safe"
     "due to assembler bugs.  It is probably ok for benchmarks, but not"
     "for the libraries."
     )
    (general
     "Scheme source files have extension .sch or .scm; these extensions"
     "are treated specially by the compiler. You may compile source files"
     "with extensions other than these, but the compiler will not replace"
     "them with an object file extension, only append the object file"
     "extension to the file name."
     ""
     "Compiled files in tokenized form have extension .lap, for \"Lisp"
     "Assembly Program\". Again, you can use others at the risk of having"
     "to type a lot."
     ""
     "MacScheme assembly language files in symbolic form have extension"
     ".mal (for MacScheme Assembly Language). These files are mostly"
     "obsolete, as their format requires that 'eval' is available."
     ""
     "Assembled files containing machine code segments have extension"
     ".lop (for \"Lisp Object Program\"."
     ""
     "Fast-load files containing machine code have extension .fasl, for"
     "\"Fast-Load\". These files contains special syntax which allow"
     "them to be read by Larceny's reader like any other file; there is"
     "no magic about the extension."
     ""
     "Heap files usually have the extension .heap."
     )
    ))

(define (display-help-for topic)
  (let ((probe (assq topic *topics*)))
    (if probe
	(for-each writeln (cdr probe))
	(writeln "There is no topic " topic))))
		    
(define (display-topics)
  (for-each (lambda (topic)
	      (writeln (cadr topic)))
	    *topiclines*))

(define (writeln . x)
  (for-each display x)
  (newline))
