SYSTEM REQUIREMENTS

Currently, this software is distributed for
 - Windows on Intel x86 machines, Microsoft .NET Framework 2.0

You can make Common Larceny work on other platforms (e.g. Mac OS X
with the Mono Framework 1.1) but we do not yet provide support for
those systems.


WHAT YOU NEED

To run the interpreter, you will first need to the LARCENY_ROOT
environment variable explicitly to the Larceny's distribution's path 
before running Common Larceny. Otherwise, everything else should
work "out of the box".

To compile files, the development tools (e.g. ilasm) must be in your
search path.  Also, for now you must have Twobit.fasl and Twobit.exe
in the current directory when you start the compiler.

To run with auto-compilation of loaded files, for now you must have
Larceny.fasl and Larceny.exe in the current directory when you start
the runtime.  Note that the compiler itself has not been optimized,
and so there is a noticeable delay when loading source in this mode.


QUICK START

CommonLarceny (Win32):
 - If you want to run with auto-compilation of Scheme source loaded
   with the load procedure, run
   > CommonLarceny Larceny.fasl

 - If you want to use the compiler under the interpreter, run
   > CommonLarceny Twobit.fasl

 - If you're content to use only the interpreter, run
   > CommonLarceny

Do not run Twobit.exe; it is not a standalone application.
Do not run Larceny.exe; it is not a standalone application.


COMPILING SCHEME SOURCE WITH LARCENY

 - (compile-file <SOURCE> [<TARGET>])
   (compile-file "source.sch")
     compiles "source.sch", leaving the compiled code in "source.fasl"

   (compile-file "source.sch" "target.fasl")
     compiles "source.sch", leaving the compiled code in "target.fasl"

 - (load "target.fasl")
     loads the compiled code in "target.fasl"


FURTHER READING

For more information, including detailed instructions on how to build
Common Larceny, please see the Common Larceny User Manual under 
doc/CommonLarceny.

For a list of bugs that are known to persist in this release, see
doc/KNOWN-BUGS.

