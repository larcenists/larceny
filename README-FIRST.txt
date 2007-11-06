SYSTEM REQUIREMENTS

Currently, this software is distributed for:
 - Solaris on SPARC machines (native SPARC and C backends)
 - Mac OS X (>= 10.2) on PPC machines (C backend)
 - Mac OS X (>= 10.4) on IA32 machines (native IA32 backend)
 - Linux on Intel x86 machines (native IA32 and C backends)
 - Windows on Intel x86 machines (native IA32 backend)

The current version of Larceny always runs in 32-bit mode, but the
native varieties of Larceny are known to work on 64-bit SPARC and
IA32 hardware.

A binary distribution of Larceny will occupy about 25 megabytes
of disk.  A source distribution of Larceny will expand to almost
200 megabytes as it is built.

"Petit Larceny" is used throughout Larceny's documentation to
refer to all varieties other than Common Larceny and the SPARC
and IA32 native backends [1].  (If you intend to use Common
Larceny, see the Common Larceny user's manual [2].)


WHAT YOU NEED

Native (SPARC, IA32): everything should work out of the box.

Petit Larceny (Solaris, Linux, Mac OS X): ensure that the GNU C
    Compiler (GCC) is in your search path [3].  (If you are
    building from source, see doc/HOWTO-SETUP.)

    Mac OS X: Use Apple's Developer Tools, http://developer.apple.com/


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


R6RS SCHEME SCRIPTS

On most Unix systems (including MacOS X and Linux), Larceny's
scheme-script will execute Scheme scripts as described in R6RS
non-normative appendix D, with or without the optional script
header.  To make Scheme scripts executable in their own right,
without executing scheme-script directly, add Larceny's root
directory to your path as described in doc/HOWTO-INSTALL.

Suppose, for example, that /home/myself/hello is an R6RS
Scheme script whose first line is the optional script header
(#!/usr/bin/env scheme-script).  If you do not have execute
permission for this script, or Larceny's root directory is
not in your path, then you can still run the script from
Larceny's root directory as follows:
     
    % ./scheme-script /home/myself/hello

If you have execute permission for the script, and Larceny's
root directory is in your path, then you can also run the
script as follows:

    % /home/myself/hello

If, in addition, the directory that contains the script is
in your path, then you can run the script as follows:

    % hello


FURTHER READING

If you want to install Larceny for other users on your system, see
doc/HOWTO-INSTALL.  To build your own Larceny, take a look at
doc/HOWTO-BUILD.  To build Common Larceny, see the Common Larceny
documentation in doc/CommonLarceny/user-manual.txt.


NOTES

[1] You may be able to make Petit Larceny work on other platforms
(e.g. Linux/PPC or Solaris/IA32 with the C backend) but we do not
yet offer support for those systems.

[2] http://www.ccs.neu.edu/home/will/Larceny/CommonLarceny/user-manual.html

[3] We currently support GCC version 3.3; other versions may work, but
currently GCC 4.0 seems to have trouble with the code we generate.  GCC
version 4.0 is the default C compiler on Mac OS X 10.4, but you can
select another version using /usr/sbin/gcc_select.
