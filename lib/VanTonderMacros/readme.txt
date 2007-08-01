
;;;===============================================================================
;;;
;;; R6RS Macros and R6RS libraries:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at srfi.schemers.org/srfi-process.html
;;;
;;;===============================================================================
 
----------------------------------------------------------------------
TO RUN : execute examples.scm (after loading appropriate compatibility
         file in expander.scm)
----------------------------------------------------------------------

Tested on: MzScheme     (compat file provided)
           Petite Chez  (compat file provided)
           Larceny      (compat file provided)

Uses r5rs plus:

  * SRFI-9
  * parameterize
  * let-values
  * pretty-print
  * file-exists? and delete-file 
  * the ability to generate a token that uniquely identifies the run  

R6RS COMPLIANCE NOTES:
======================

SYNTAX-CASE: 
------------   

 FULLY R5.97RS-COMPLIANT, WITH THE FOLLOWING OPTIONAL OR RECOMMENDED FEATURES:
  
   * Expansion algorithm (r6rs optional):
   
       We use a renaming algorithm instead of the mark-antimark
       algorithm described in the draft.  The difference will not 
       matter for r6rs-conforming macros.

   * Wrapped = Unwrapped (r6rs optional):
   
       A wrapped syntax object is the same as an unwrapped syntax 
       object, and can be directly manipulated using car, cdr, ... 
       without syntax-case deconstruction.  
       This should make porting of legacy low-level macros,
       e.g., explicit renaming or define-macro, easier.
       Macros depending on this feature wil not be 
       portable to all r6rs implementations.  
                 
   * Lexical scoping violation detection (r6rs recommended):
   
       We treat a violation of the following restriction in chapter 8
       as a syntax violation:
       
       A definition in the sequence of forms must not define any 
       identifier whose binding is used to determine the meaning of the
       undeferred portions of the definition or any definition that 
       precedes it in the sequence of forms.       

LIBRARIES:
----------

 FULLY R5.97RS-COMPLIANT WITH ONE EXCEPTION (SEE BELOW) WITH THE FOLLOWING OPTIONAL 
 AND ADDITIONAL FEATURES: 
   
   * Phase semantics (r6rs optional):
   
       - A syntax violation is raised at expand-time if a binding is
         referenced outside its declared levels.  We do this for 
         both variable references and macro references.  As we are 
         entitled to do by R5.97RS, we also check levels for some
         references that occur when free-identifier=? is called.
         Specifically: 
          
         A syntax error is raised if free-identifier=? succeeds 
         but either argument is outside its declared level.  This is sufficient 
         to ensure that literals such as ... in syntax-case are used at the 
         correct level.  See the examples file for more discussion, and for 
         an explanation of why the levels should not be checked if the 
         comparison fails.  
         
         Note: This choice raises a syntax violation when MzScheme and Chez
               would disagree, thus ensuring that a program blessed by this
               expander will be maximally portable.  
                
       - We start each expansion of a library form by removing visits of libraries 
         in any phase and instances of libraries in phases above 0. 
         We start each execution of a toplevel program by removing instances of
         libraries in phase 0.
                
       - While expanding a library form, we use an instance/visit at any phase as 
         an instance/visit at any other phase.     
                   
       - We may create instances/visits of a library at more phases than 
         strictly required to satisfy references, but only in the case - of 
         which the semantics is unspecified in r6rs - where a library is imported 
         for some phase but none of its bindings is used in that phase.  
         Specifically:
                
         During expansion of a library or program, each library imported 
         at phase n > 0 is instantiated once, and each library imported 
         at phase n >= 0 is visited once.   
                
         During execution of a program, each library imported 
         at phase n = 0 is instantiated once.
 
   * Separate and joint organization and expansion (r6rs unspecified):
   
       Libraries and programs may be in separate files or in the same file,
       and may be separately or jointly expanded. 
       See example scripts in examples.scm.
                                
   * Repl (r6rs unspecified):
   
       A REPL is provided that is integrated with the r6rs library system.  
       Libraries and programs can be defined at, imported into and run in the REPL toplevel.  
       See examples.scm.
       
    * Reflection (not in r6rs):
    
       A library (rnrs eval reflection (6)) is provided.  
       It exports the procedure (environment-bindings <environment>).
       Displays for each binding whether it is a variable or macro, its
       source library, and its levels.
       Very useful for library development and debugging. 
       See examples of use in examples.scm.  
       
 KNOWN R6RS-NONCOMPLIANCE:       
       
   * Version name and reference are syntax-checked, but otherwise ignored. 
 
 
CHANGELOG:
========== 

CHANGES SINCE VERSION OF JULY 12, 2007:
--------------------------------------

- Immutability checks:
  It is now a syntax violation if an explicitly exported variable appears on the 
  left-hand side of a set! expression, either in the exporting or importing libraries.
  It is also a syntax violation if a variable appears on the left-hand side of a set!
  expression in any code produced by an exported macro outside of the library in which 
  the variable is defined. 
  It is also a syntax violation if a reference to an assigned variable appears in any 
  code produced by an exported macro outside of the library in which the variable is 
  defined.
- Added library (rnrs eval reflection (6)) for reflection facilities:
  (environment-bindings <environment>) lists the bindings in an environment
  Very useful for development and dsbugging of libraries
  See the examples in examples.scm file
- replaced implementation of QUASIQUOTE with optimised Dybvig portable syntax-case version
- an unbound identifier at toplevel will now match unbound literal in library
- improved some error messages to state source libraries of bindings
- Larceny compat updated to v0.94
- corrected free occurrence of FIRST in ex:unify-imports that prevented an error message
- removed caching of imported macros
- added missing version (6) to library (rnrs mutable-strings)
- only, except, prefix, and rename now enforce the constraints stated in the r6rs draft

CHANGES SINCE VERSION OF JULY 10, 2007:
--------------------------------------

 - Fixed bug causing internal error when a literal appeared followed by
   ellipses, as in (syntax-rules (x) ((m x ...) #t))

CHANGES SINCE VERSION OF JUNE 22, 2007:
--------------------------------------

 - simplified expansion of bodies - "wraps"
 - changed "mark" to "color" everywhere to avoid confusion with Dybvig algorithm
 - changed unify-imports so it runs much faster
 - reorganized standard libraries so more common names near front of envs
 - simplified environment model significantly
 - fixed bug preventing nonpair, nonvector, nonsymbol values in syntax
   objects and patterns
 - removed UNSPECIFIED
 - we now detect and raise syntax error for pattern variables outside templates
 - corrected bug where pattern variable occurred outside a template in standard-libraries.scm
 - corrected bug preventing FOR clause from having no import levels
 - as required by r6rs, free-identifier=? now treats unbound identifiers specially
 - as required by r6rs, modified free-identifier=? behaviour w.r.t phases.
   Previously, free-identifier=? could return #f in cases where both arguments referred to
   the same binding but either argument was out of phase.  Now an out of phase error is
   raised in this case, so that a #f result can only happen when the arguments refer to
   different bindings, and a #t result when they refer to the same binding.
 - as allowed by R6RS, included refined phase checking of arguments to free-identifier=?
   An out of phase error is raised if the comparison succeeds but either argument is
   out of phase.  This is sufficient to ensure that literals such as ... in syntax-case
   are used in the correct phase.  See the examples file for more discussion.  
 - out of phase uses of internal DEFINE, DEFINE-SYNTAX, LET[REC]-SYNTAX, BEGIN will now give a phase error
 - for portability, out of context references to let[rec]-syntax bindings now give a syntax error
 - refined algorithm for better detection of definitions that affect previously expanded
   undeferred portions of body.  Should now catch all cases.
 - changed syntax of (primitives (id ...)) to (primitives id ...) 
 - implemented r6rs library version reference syntax
 - implemented (library ---) import clause
 - EVAL now raises a syntax error for definition or sequence containing definition
 - added (rnrs r5rs) including 
      (null-environment 5) 
      (scheme-report-environment 5)
      delay and force
 - Added stubs for (rnrs uncode), (rnrs io simple), (rnrs mutable-strings)
 - cleaned up standard library organization.

CHANGES SINCE VERSION OF MARCH 1, 2007:
---------------------------------------

 - added Larceny, MzScheme and Chez compatibilty files.
 - procedure ex:expand-file provided, that can be used as an r6rs front end
   to an existing r5rs compiler.  Libraries and toplevel programs may be in 
   separate files or in the same file, and may be separately or jointly 
   expanded or compiled.  Interface is 
   (ex:expand-file source-filename target-filename dependency ...)
   The dependencies must list the already expanded files
   containing libraries to be imported.  
 - added examples and scripts for separate and joint compilation in examples.scm
 - code no longer depends on the non-r5rs assumption of letrec* semantics for 
   internal definitions.
 - replaced uses of define-struct by uses of srfi-9.
 - export-levels of identifiers in standard libraries made consistent with 5.95,
   although I think 5.95 is still wrong on this.  
 - ... and _ disallowed as literals, although I think 5.95 is wrong on this.   
 - removed requirement that toplevel program should end with expression.
 - removed unused "level" argument in expanded library visiting and instantiation code. 
 - fixed bug that prevented certain redefinitions in toplevel repl (begin ...) sequences.  
 - changed contract-violation -> assertion-violation.
 - added (r6rs control) to standard libraries.
 - fixed bug that prevented shadowing of macro keywords by later variable definitions
   ar repl toplevel.

CHANGES SINCE VERSION OF JANUARY 15, 2007:
------------------------------------------

 - updated README to reflect compatibility with R(5.92)RS.

CHANGES SINCE VERSION OF DECEMBER 25, 2006:
-------------------------------------------

 - corrected bug in phase shift enforcement while visiting and
   invoking, which affected behaviour of syntax forms evaluated 
   in a library's top-level expand-time or runtime sequence.  
 - (r6rs base) now also exports ... and _ for use with syntax-rules.

CHANGES SINCE VERSION OF DECEMBER 17, 2006:
-------------------------------------------

 - dropped unshared instantiation option, keeping only shared semantics.
 - corrected export phase of syntax-rules and identifier-syntax
   in (r6rs base).  These are now exported for expand instead of 
   for run.
 - simplified documentation in readme.    

CHANGES SINCE VERSION OF DECEMBER 14, 2006:
-------------------------------------------

 - removed support for declarations
 - changed script -> program everywhere
 - changed add-prefix -> prefix everywhere
 - dropped the unparenthesized shorthand for library names
 - changed forall -> for-all everywhere
 - fenders have been removed from syntax-rules
 - added "identifier-syntax" to (r6rs base)

CHANGES SINCE VERSION OF NOVEMBER 27, 2006:
-------------------------------------------
 
 - corrected implementation and description of invocation semantics  
   and its interaction with negative import levels.    
   
CHANGES SINCE VERSION OF NOVEMBER 5, 2006:
------------------------------------------
 
 - added UNSHARED semantics (see above) as a configurable option.  
 
CHANGES SINCE VERSION OF SEPTEMBER 13, 2006:
--------------------------------------------
 
 - added support for scripts.
 - quasisyntax now works with unsyntax(-splicing) instead of unquote(-splicing). 
 - many bug fixes and improvements in code.
 - reorganized standard libraries. 
 - properly avoids defect in R(5.91)RS assigning lexical scope violating 
   semantics to certain expressions.
 - implemented library reference syntax.
   
   
