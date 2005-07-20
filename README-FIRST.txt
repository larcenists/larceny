SYSTEM REQUIREMENTS

Currently, this software is distributed for Solaris on Sparc machines
and Mac OS X on PowerPC machines (versions 10.2 and higher).  

You may be able to make it work on other platforms (e.g. Linux on
Intel machines), but we do not yet provide support for those systems.
We expect to support Windows and Linux on Intel machines in the near
future.

For Sparc Native, everything should work out of the box.  For Sparc
and Mac OS X Petit, you need to ensure that the Gnu C Compiler (GCC)
is on your search path.  For Mac OS X Petit, it should suffice to
install the Developer Tools that come with your operating system
installation disks.


QUICK START 

* Sparc Native 
  - If you want to use the compiler, run
    % larceny.bin larceny.heap

  - If you're content to use only the interpreter, run 
    % larceny.bin r5rs.heap

* Sparc Petit  
  - If you want to use the compiler, run
    % twobit twobit.heap

  - If you're content to use only the interpreter, run 
    % petit petit.heap

* Mac OS X Petit
  - If you want to use the compiler, run
    % twobit.app twobit.heap

  - If you're content to use only the interpreter, run 
    % petit petit.heap


There are other binaries and heaps in the root directory of the
distribution, but they are meant for developers and are not all
mutually compatible with one another.  (In general, an executable
named "X" is meant to run with the heap named "X.heap".)


COMPILING

* Sparc Native
  - (compile-file <source> [<target>])

    (compile-file "source.sch") 
    compiles "source.sch", leaving the compiled code in "source.fasl".
    (The .scm suffix is also recognized.)

    (compile-file "source.sch" "target.fasl")
    compiles "source.sch", leaving the compiled code in "target.fasl"


  - (load "target.fasl")
    loads the compiled code in "target.fasl"

* Petit (both Sparc and Mac OS X)
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
