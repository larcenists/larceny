From http://www.het.brown.edu/people/andre/macros/index.htm

PORTABLE MACROS (SRFI-72) AND R6RS LIBRARIES (SRFI-83):
-------------------------------------------------------

To run the basic examples, download:

  simple-macros.scm          -- The basic primitives for macros and libraries
  simple-syntax-case.scm     -- For syntax-case and syntax-rules
  simple-macros-tests.scm    -- Examples: Macros
  simple-module-examples.scm -- Examples: Libraries 
  
Simply save these files together, open one of the examples files
and execute in the REPL.


SRFI-83 (R6RS) LIBRARIES: Description, Semantics Extension.
-----------------------------------------------------------

The semantics is as in SRFI-83, but has been extended to
allow import into higher phases (as is need when let-syntax
is nested).

The syntax for the latter is:

  (for import-set import-phase*)

where

  import-phase ::= run | expand | all | 0 | 1 | 2 | ...

Here 0 == run, 1 == expand, and |all| wil cause import into
all phases, which is implicitly done with the language.
See the file for commented examples of use.

The semantics is as follows:

To invoke a library at phase N:

   * Invoke at phase N any library that is imported by this library
     for run time, and that is not yet invoked at phase N. 
   * Evaluate all variable definitions and top-level expressions within
     the library. (Macro definitions are not evaluated.)

To visit a library at phase N:

   * For each k >= 1, invoke at phase N+k any library that is imported
     by this library for .... (phase k), and that is not yet invoked at
     phase N+k.
   * For each k >= 0, visit at phase N+k any library that is imported by
     this library for .... (phase k), and that is not yet visited at phase
     N+k.
   * Evaluate all syntax definitions within the library.
     (Top-level expressions are not evaluated, and the right-hand sides
     of variable definitions are not evaluated.)

Technical remarks:
------------------

* We do not enforce the no-shadowing and no-multiple-import
  constraints.
* We allow different bindings for the same identifier to be
  imported into different phases.
 
See the file for further description and commented examples.
See also simple-syntax-case.scm for a non-trivial
example of libraries.

