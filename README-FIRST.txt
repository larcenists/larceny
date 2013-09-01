SYSTEM REQUIREMENTS

Currently, this software is distributed for:
 - Windows on Intel x86 machines (native IA32 backend)
 - Linux on Intel x86 machines (native IA32 and C backends)
 - Linux on little-endian ARMv7 machines (native ARM-32 backend)
 - Mac OS X (>= 10.4) on Intel x86 machines (native IA32 backend)
 - Mac OS X (>= 10.2) on PowerPC machines (C backend)
 - Solaris on SPARC machines (native SPARC and C backends)

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

Native (SPARC, IA32, ARM): everything should work out of the box.

Petit Larceny (Solaris, Linux, Mac OS X): ensure that the GNU C
    Compiler (gcc) is in your execution path.  (If you are
    building from source, see doc/HOWTO-BUILD.)

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

To precompile files that contain R5RS Scheme code:

 - (compile-file <SOURCE> [<TARGET>])

   (compile-file "source.sch")
     compiles "source.sch", leaving the compiled code in "source.fasl"

   (compile-file "source.sch" "target.fasl")
     compiles "source.sch", leaving the compiled code in "target.fasl"

 - (load "target.fasl")
     loads the compiled code in "target.fasl"

To precompile ERR5RS/R6RS libraries, top-level programs, and
Scheme scripts, use the compile-stale Scheme script that is
in Larceny's root directory.  Please see the Larceny User
Manual [3] for details.


FURTHER READING

If you want to install Larceny for other users on your system, see
doc/HOWTO-INSTALL.  To build your own Larceny, take a look at
doc/HOWTO-BUILD.  To build Common Larceny, see the Common Larceny
documentation in doc/CommonLarceny/user-manual.txt.

Most importantly, see the Larceny user manual in doc/UserManual.
The most recent version of the user manual is autobuilt daily
and is online at http://larceny.ccs.neu.edu/doc/


NOTES

[1] You may be able to make Petit Larceny work on other platforms
(e.g. Linux/PowerPC or Solaris/IA32 with the C backend) but we do
not yet offer support for those systems.

[2] http://www.ccs.neu.edu/home/will/Larceny/CommonLarceny/user-manual.html

[3] See doc/UserManual or http://larceny.ccs.neu.edu/doc/
