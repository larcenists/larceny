SYSTEM REQUIREMENTS

Currently, this software is distributed for:
 - Solaris on SPARC machines (native SPARC and C backends)
 - Mac OS X (>= 10.2) on PowerPC machines (C backend)
 - Mac OS X (>= 10.4) on Intel x86 machines (native IA32 backend)
 - Linux on Intel x86 machines (native IA32 and C backends)
 - Windows on Intel x86 machines (native IA32 backend)

The current version of Larceny always runs in 32-bit mode, but the
native varieties of Larceny are known to work on 64-bit SPARC and
x86-64 hardware.

A binary distribution of Larceny will occupy about 25 megabytes
of disk when unpacked.  A source distribution of Larceny will
expand to almost 200 megabytes as it is built.

"Petit Larceny" is used throughout Larceny's documentation to
refer to all varieties other than Common Larceny and the SPARC
and IA32 native backends [1].  (If you intend to use Common
Larceny, see the Common Larceny user's manual [2].)


WHAT YOU NEED

Native (SPARC, IA32): everything should work out of the box.

Petit Larceny (Solaris, Linux, Mac OS X): ensure that the GNU C
    Compiler (gcc) is in your execution path [3].  (If you are
    building from source, see doc/HOWTO-SETUP.)

    Mac OS X: Use Apple's Developer Tools, http://developer.apple.com/

    After gcc is in your execution path, you should install
    the R6RS runtime and standard libraries by performing
    step 4 of the process described in doc/HOWTO-BUILD.


QUICK START

Solaris, Linux, and Mac OS X:
 - Run
   % ./larceny

Win32:
 - Run
   > larceny


EXECUTION MODES

    R5RS              traditional read/eval/print loop (the default)
    ERR5RS            see doc/HOWTO-ERR5RS
    R6RS              see doc/HOWTO-R6RS
    Scheme script     see doc/HOWTO-SCRIPT

The precompiled distributions of native Larceny should work
out of the box in all four modes, but the ERR5RS, R6RS, and
Scheme script modes will not work in Petit Larceny until
you have performed step 4 of the process described in
doc/HOWTO-BUILD.


COMPILING SCHEME SOURCE WITH LARCENY

Native versions of Larceny compile all definitions and most
expressions to machine code as they are typed or loaded.
Programs will load faster if precompiled; in Petit Larceny,
the precompiled programs will also run much faster.

 - (compile-file <SOURCE> [<TARGET>])

   (compile-file "source.sch")
     compiles "source.sch", leaving the compiled code in "source.fasl"

   (compile-file "source.sch" "target.fasl")
     compiles "source.sch", leaving the compiled code in "target.fasl"

 - (load "target.fasl")
     loads the compiled code in "target.fasl"

In Larceny v0.95, the compile-file procedure can only be used
to compile files that contain R5RS source code.  ERR5RS and
R6RS libraries cannot be precompiled.  This limitation will
be removed in the near future.


FURTHER READING

If you want to install Larceny for other users on your system, see
doc/HOWTO-INSTALL.  To build your own Larceny, take a look at
doc/HOWTO-BUILD.  To build Common Larceny, see the Common Larceny
documentation in doc/CommonLarceny/user-manual.txt.


NOTES

[1] You may be able to make Petit Larceny work on other platforms
(e.g. Linux/PowerPC or Solaris/IA32 with the C backend) but we do
not yet offer support for those systems.

[2] http://www.ccs.neu.edu/home/will/Larceny/CommonLarceny/user-manual.html

[3] We currently support gcc version 3.3; other versions may work, but
currently gcc 4.0 seems to have trouble with the code we generate.  gcc
version 4.0 is the default C compiler on Mac OS X 10.4, but you can
select another version using /usr/sbin/gcc_select.
