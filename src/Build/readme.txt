Copyright 2007 William D Clinger

$Id$


DOCUMENTATION OF LARCENY'S BUILD PROCESS
========================================

Larceny's build process is ad hoc and confusing.  In
particular, files whose names contain "petit" are
often used for native and/or Common Larceny as well
as Petit Larceny, and files and directories whose
names contain "mzscheme" may have nothing to do with
MzScheme.

The build process for Common Larceny is different,
and is not documented by this file, although the
Common Larceny build process is modelled after the
process described here.  The Common Larceny User
Manual contains instructions for building Common
Larceny.


Outline
=======

According to doc/HOWTO-BUILD, Larceny is built from
source code by following these steps:

    (load "setup.sch")
    (setup 'scheme: ${SCHEME} 'host: ${HOST} [ 'native | 'sassy ]
           'string-rep: ${STREP})
    (build-config-files)
    (load-compiler)

    (build-heap)
    (build-runtime)
    (build-executable)

    (build-larceny-files)

    (build-twobit)

Then:
Native Larceny (SPARC):
    % ./larceny.bin -stopcopy -- src/Build/sparc-larceny-heap.fasl
    % ./larceny.bin -stopcopy -- src/Build/sparc-twobit-heap.fasl
Native Larceny (Intel):
    % "./larceny.bin" -stopcopy -- src/Build/iasn-larceny-heap.fasl
    % "./larceny.bin" -stopcopy -- src/Build/iasn-twobit-heap.fasl
Petit Larceny:
    % ./petit-larceny.bin -stopcopy -- src/Build/petit-larceny-heap.sch
    % ./twobit.bin -stopcopy -- src/Build/petit-twobit-heap.sch

The purpose and files involved in each step are
documented below.


(load "setup.sch")
==================

This must be done in an R5RS-compatible implementation
of Scheme that supports all of the lexical syntax used
in the Larceny sources.  At the moment, the only host
systems that are known to work are Larceny and MzScheme.

The current directory must be the Larceny root directory.

The setup.sch file loads src/Build/petit-unix-defns.sch,
which is used for native as well as Petit Larceny and is
used for Windows as well as Unix.  That file defines the
procedures that implement the following steps of a build,
and loads src/Build/petit-unix-defns-globals.sch, which
defines a bunch of global variables.

The setup.sch file also defines the setup procedure and a
setup-load-build procedure, which takes the same arguments
as the setup procedure and performs all of the steps of
the build process through the call to build-larceny-files
(but does not call build-twobit).


(setup 'scheme: ... 'host: ... [ 'native | 'sassy ] 'string-rep: ...)
=====================================================================

This procedures assigns appropriate values to global
variables.  Then it calls unix-&-win32-initialize, which
is defined in src/Build/petit-unix-defns.sch and loads
the following files:

    src/Build/sysdep-unix.sch or src/Build/sysdep-win32.sch

        defines procedures for manipulating files and paths

    src/Build/nbuild-param.sch

        defines machinery and defaults for nbuild-parameter

    src/Compat/.../compat.sch

        defines a uniform host system

    src/Build/expander.sch

        defines translator from .mac to .c files

    src/Build/config.sch

        defines translators from .cfg to .h and .sch files


(build-config-files)
====================

Defined in src/Build/petit-unix-defns.sch, this procedure
creates a bunch of .h and .sch files in the include and
src/Rts directories.


(load-compiler)
===============

Defined in src/Build/petit-unix-defns.sch, this procedure
loads src/Build/nbuild.sch, which loads the Twobit compiler,
the appropriate assembler, and related files in this order:

    src/Build/nbuild-files.sch
    src/Build/nbuild-defns.sch
    nbuild:twobit-files         (defined in src/Build/nbuild-files.sch)
    nbuild:common-asm-files     (defined in src/Build/nbuild-files.sch)
    nbuild:machine-asm-files    (defined in src/Build/nbuild-files.sch)
    nbuild:heap-dumper-files    (defined in src/Build/nbuild-files.sch)
    nbuild:utility-files        (defined in src/Build/nbuild-files.sch)
    src/Rts/make-templates.sch
    src/Build/cleanup.sch

It then initializes the help system, compiler, assembler,
and heap dumper.


(build-heap)
============

Defined in src/Build/petit-unix-defns.sch, this procedure
calls one of the following three procedures, which are
defined in src/Lib/makefile.sch:

    make-sparc-heap
    make-sasstrap-heap
    make-petit-heap

This creates the basic heap image used for bootstrapping:
sparc.heap, sasstrap.heap, or petit.heap.


(build-runtime)
===============

Defined in src/Build/petit-unix-defns.sch, this procedure
generates a makefile (see src/Rts/make-templates.sch) and
uses it to compile the C and assembly language source code
into a binary executable (larceny.bin, larceny.bin.exe,
libpetit.a, or libpetit.lib).


(build-executable)
==================

Defined in src/Build/petit-unix-defns.sch, this procedure
copies the appropriate script(s) into the Larceny root
directory.  For Petit Larceny, it also generates a binary
executable.


Larceny and Twobit heaps
========================

The Larceny and Twobit heap images, which are used by the
standard scripts, are created by executing an appropriate
binary executable with the -stopcopy command-line argument
and an appropriate basic heap image, and loading one of
these files:

    src/Build/sparc-larceny-heap.sch
    src/Build/sparc-twobit-heap.scn
    src/Build/iasn-larceny-heap.sch
    src/Build/iasn-twobit-heap.sch
    src/Build/petit-larceny-heap.sch
    src/Build/petit-twobit-heap.sch

These files use the same machinery that was used to build
the standard heap image, but load several other files
besides and create a top-level interaction environment
distinct from the top-level environment of the standard
heap image.  They may also change the macro expander and
evaluator.

When creating a Larceny heap, the load-compiler procedure
is called with the symbol release as its argument.  When
creating a Twobit heap, load-compiler is called without
arguments.
