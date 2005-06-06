There is a lot of documentation available in the Docs/ directory, all
in various states of completeness.

The simplest way to dive into Larceny is to run it; your distribution
should come with a binary for your architecture, here in the root of
this source tree.  [[TODO: give the name(s) for the binary]]

If you want to build your own copy of Petit Larceny, the easiest way
to get your feet wet is to read Docs/HOWTO-SETUP.  There you'll find
the eight scheme commands to build Larceny from scratch, both from an
existing Larceny executable, or from another Scheme interpreter such
as Chez or PLT.

After you have gone through the steps described in HOWTO-SETUP, you
may want to read the other documentation particular to your platform,
such as HOWTO-SPARC or HOWTO-MACOSX.  While the instructions given in
the different HOWTOs are often similar, be aware that they are not
always exactly alike.

The Petit Larceny executable that is distributed with the system (and
that is built by the process in HOWTO-SETUP) is designed to be run
from the root of the Larceny source tree; in particular, there are
header files and libraries that it will reference when it issues
commands to an external compiler like gcc, so it needs to be in the
right directory to run.  If you want to install Larceny (and its
Scheme compiler, Twobit), for general use on your system outside of
the Larceny source tree , then you should read the corresponding
documentation in HOWTO-PETIT.
