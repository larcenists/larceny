SYSTEM REQUIREMENTS

Currently, Larceny is distributed for two 64-bit systems:
 - macOS on Apple Silicon (AArch)
 - Linux on Intel x86 machines (native and C backends)

Starting with Larceny v2.0, Larceny runs in 64-bit mode.
A 32-bit version, Larceny v1.5, remains available for Linux
on Intel x86 machines.

An even older version, v1.3, is still available for these systems:
 - Windows on Intel x86 machines (native IA32 backend)
 - Linux on little-endian ARMv7 machines (native ARM-32 backend)
 - macOS 10.12 through 10.14 on Intel x86 machines (native IA32)
To run any of the 32-bit versions on x86-64 hardware, the necessary
32-bit libraries must also be installed (see below).

A binary distribution of Larceny occupies about 40 megabytes
of disk when unpacked.  A source distribution of Larceny will
expand to almost 200 megabytes as it is built.

"Petit Larceny" is used throughout Larceny's documentation to
refer to all varieties other than Common Larceny and the native
backends for AArch, x86-64, IA32 and ARMv7 [1].  (If you intend
to use Common Larceny, see the Common Larceny user's manual [2].)

    Because Petit Larceny compiles to C instead of to native
    machine code, it is sensitive to changes in C compilers.
    The 32-bit version of Petit Larceny has not been updated,
    and is not working at the moment.  The 64-bit version of
    Petit Larceny can be built from source in Larceny v2.0.


WHAT YOU NEED

The 64-bit versions (AArch, x86-64) should work right out of the
box.  For the 32-bit versions (IA32, ARMv7), you may need to
install some 32-bit libraries or (on Windows) allow Larceny to
opt out of DEP.  See doc/HOWTO-INSTALL.

Petit Larceny (Linux x86-64, macOS): ensure that the GNU C Compiler
(gcc) is in your execution path.  (If you are building from source,
see doc/HOWTO-BUILD.)

    Petit Larceny only:  After gcc is in your execution path,
    you should install the R7RS runtime and standard libraries
    by performing step 4 of the process described in doc/HOWTO-BUILD.


QUICK START

Linux and macOS:
 - Run
   % ./larceny

Win32 (Larceny v1.3)
 - Run
   > larceny


EXECUTION MODES

    R5RS              traditional read/eval/print loop (the default)
    R6RS              see doc/HOWTO-R6RS
    R7RS              see doc/HOWTO-R7RS
    Scheme script     see doc/HOWTO-SCRIPT

The precompiled distributions of native Larceny should work
out of the box in all four modes, but the R6RS, R7RS, and
Scheme script modes will not work in Petit Larceny until
you have performed step 4 of the process described in
doc/HOWTO-BUILD.

Those modes of execution are described by Larceny's User
Manual [2].


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

To precompile R7RS/R6RS libraries, programs, and Scheme scripts,
use the compile-larceny and compile-stale scripts that are in
Larceny's root directory.
Please see the Larceny User Manual [2] for details.


FURTHER READING

If you want to install Larceny for other users on your system, see
doc/HOWTO-INSTALL.  To build your own Larceny, take a look at
doc/HOWTO-BUILD.  To build Common Larceny, see the Common Larceny
documentation in doc/CommonLarceny/user-manual.txt.

Most importantly, see Larceny's user manual [2].


NOTES

[1] You may be able to make Petit Larceny work on other platforms
(e.g. MacOSX/PowerPC or Linux/PowerPC with the C backend) but we do
not support those systems.

[2] https://larcenists.org/Documentation/Documentation2.0/index.html
