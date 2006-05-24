SYSTEM REQUIREMENTS

Currently, this software is distributed for
 - Windows on Intel x86 machines, Microsoft .NET Framework 2.0

You can make Common Larceny work on other platforms (e.g. Mac OS X
with the Mono Framework 1.1) but we do not yet provide support for
those systems.

WHAT YOU NEED

To run the interpreter, everything should work out of the box.  (For
.NET before version 2.0, you may have to set the LARCENY_ROOT
environment variable explicitly to the Larceny's path before running
Common Larceny)

To compile files, the development tools (e.g. ilasm) must be in your
search path.  Also, for now you must have Twobit.fasl and Twobit.exe
in the current directory when you start the compiler.

QUICK START

CommonLarceny (Win32):
 - If you want to use the compiler, run
   > CommonLarceny -- Twobit.fasl

 - If you're content to use only the interpreter, run
   > CommonLarceny

Do not run Twobit.exe; it is not a standalone application.

COMPILING SCHEME SOURCE WITH LARCENY

 - (compile-file <SOURCE> [<TARGET>])

   (compile-file "source.sch")
     compiles "source.sch", leaving the compiled code in "source.fasl"

   (compile-file "source.sch" "target.fasl")
     compiles "source.sch", leaving the compiled code in "target.fasl"

 - (load "target.fasl")
     loads the compiled code in "target.fasl"


FURTHER READING

To build your own Common Larceny, take a look at Docs/HOWTO-DOTNET.
For a list of bugs that are known to persist in this release, see
Docs/KNOWN-BUGS.


