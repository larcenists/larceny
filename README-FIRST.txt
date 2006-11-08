SYSTEM REQUIREMENTS

Currently, this software is distributed for:
 - Solaris on SPARC machines (native SPARC and C backends)
 - Mac OS X (>= 10.2) on PPC machines (C backend)
 - Mac OS X (>= 10.4) on IA32 machines (native IA32 backend)
 - Linux on Intel x86 machines (native IA32 and C backends)
 - Windows on Intel x86 machines (native IA32 backend)

"Petit Larceny" is used throughout this documentation to refer to all
versions other than the SPARC and IA32 native backends [1].


WHAT YOU NEED

Native (SPARC, IA32): everything should work out of the box.

Petit/C (Solaris, Linux, Mac OS X): ensure that the GNU C Compiler (GCC)
    is in your search path [2].  (If you are building from source, see
    doc/HOWTO-SETUP.)

    Mac OS X: Use Apple's Developer Tools, http://developer.apple.com/


QUICK START

Solaris, Linux, and Mac OS X:
 - Run
   % ./larceny

Win32:
 - Run
   > larceny


COMPILING SCHEME SOURCE WITH LARCENY

 - (compile-file <SOURCE> [<TARGET>])

   (compile-file "source.sch")
     compiles "source.sch", leaving the compiled code in "source.fasl"

   (compile-file "source.sch" "target.fasl")
     compiles "source.sch", leaving the compiled code in "target.fasl"

 - (load "target.fasl")
     loads the compiled code in "target.fasl"


FURTHER READING

If you want to install Larceny for other users on your system, see
doc/HOWTO-INSTALL.  To build your own Larceny, take a look at
doc/HOWTO-BUILD.


NOTES

[1] You may be able to make Petit Larceny work on other platforms
(e.g. Linux/PPC or Solaris/IA32 with the C backend) but we do not
yet offer support for those systems.

[2] We currently support GCC version 3.3; other versions may work, but
currently GCC 4.0 seems to have trouble with the code we generate.  GCC
version 4.0 is the default C compiler on Mac OS X 10.4, but you can
select another version using /usr/sbin/gcc_select.
