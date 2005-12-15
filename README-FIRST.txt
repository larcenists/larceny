SYSTEM REQUIREMENTS

Currently, this software is distributed for:
 - Solaris on Sparc machines
 - Mac OS X on PowerPC machines (versions 10.2 and higher)
 - Linux on Intel x86 machines
 - Windows on Intel x86 machines

You may be able to make it work on other platforms (e.g. Linux on
PowerPC machines), but we do not yet provide support for those systems.

WHAT YOU NEED

Native (Sparc): everything should work out of the box.
Petit/C (Sparc, Linux, Mac OS X): ensure that the GNU C Compiler (GCC)
    is in your search path
Petit/NASM (Linux): both the GNU C Compiler and NASM (The Netwide
    Assembler) must be in your path
Petit/NASM (Win32): both Microsoft's development tools and NASM must be
    in your search path

NASM:     http://sourceforge.net/projects/nasm
MS Tools: See http://www.winprog.org/tutorial/msvc.html
OS X:     Use Apple's Developer Tools, http://developer.apple.com/


QUICK START

* Native (Sparc)
  - If you want to use the compiler, run
    % ./larceny.bin larceny.heap

  - If you're content to use only the interpreter, run
    % ./larceny.bin r5rs.heap

* Petit (Linux, Sparc)
  - If you want to use the compiler, run
    % ./twobit twobit.heap

  - If you're content to use only the interpreter, run
    % ./petit petit.heap

* Petit/NASM (Win32)
  - If you want to use the compiler, run
    % twobit twobit.heap

  - If you're content to use only the interpreter, run
    % petit petit.heap

* Petit/C (Mac OS X)
  - If you want to use the compiler, run
    % twobit.app twobit.heap

  - If you're content to use only the interpreter, run
    % petit petit.heap


There are other binaries and heaps in the root directory of the
distribution, but they are meant for developers and are not all
mutually compatible with one another.  (In general, an executable
named "X" is meant to run with the heap named "X.heap".)


COMPILING

* Native (Sparc)
  - (compile-file <source> [<target>])

    (compile-file "source.sch")
    compiles "source.sch", leaving the compiled code in "source.fasl".
    (The .scm suffix is also recognized.)

    (compile-file "source.sch" "target.fasl")
    compiles "source.sch", leaving the compiled code in "target.fasl"


  - (load "target.fasl")
    loads the compiled code in "target.fasl"

* Petit (all platforms)
  - (compile-files <source-file-list> <target>)

    (compile-files '("source1.sch" "source2.sch") "target.fasl")
    compiles the two input source files, leaving the compiled code in
    "target.fasl", along with a shared object file with a name similar
    to "target.so".

    Out of the box, compile-files only works when the
    current-directory is the root directory of the Larceny
    distribution.  To allow compilation with a different
    current-directory, either:

    - follow the steps given in Docs/HOWTO-PETIT, section "INSTALLING
      TWOBIT ON YOUR SYSTEM", or,

    - load "Util/petit-compile-file.sch" while in the root directory
      of the Larceny distribution.  This will define compile-file
      (*not* compile-files) in a manner that works outside the root
      directory.

      Note that the arguments to compile-file and to compile-files are
      different; see the Sparc Native documentation above for the
      description of compile-file.


FURTHER READING

There is a lot of documentation available in the Docs/ directory, all
in various states of completeness.

If you want to build your own copy of Petit Larceny, the easiest way
to get your feet wet is to read Docs/HOWTO-SETUP.  There you'll find
the eight scheme commands to build Larceny from scratch, both from an
existing Larceny executable, or from another Scheme interpreter such
as Chez or PLT Scheme.
