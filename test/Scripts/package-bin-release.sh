#!/bin/bash

# This script no longer invokes the Larceny executable being packaged,
# so it should be possible to do the packaging on a different system.
# The modification dates of the original must be preserved, however,
# lest compiled files be counted as stale.

set -o xtrace
set -o errexit

function maybe_mv () {
    if [ -e "$1" ]; then
        mv $1 $2
    fi
}

TEMPSCM=${TEMPSCM:-${HOME}/package-temp.scm}

# The nightly builds use git clone or svn checkout instead of svn export,
# so you need to remove the .svn directories that are littered in the tree.
find . -name .svn -type d | xargs rm -rf
rm -rf .git .gitignore

# Remove README-COMMON-LARCENY.txt from the non Common Larceny
# distributions; make it README-FIRST.txt in Common Larceny.
if [ -e Larceny.fasl ]; then
    maybe_mv README-COMMON-LARCENY.txt README-FIRST.txt
    maybe_mv dotnet.heap.exe CommonLarceny.exe

    # Common Larceny needs some files from src to be set aside and then
    # restored:
    for file in src/Build/nbuild-param \
                src/Lib/Common/toplevel \
                src/Lib/Arch/IL/toplevel-target; do
        mkdir -p aside/`dirname $file`
        for ext in sch fasl exe; do
            maybe_mv $file.$ext aside/$file.$ext
        done
    done
else
    rm -f README-COMMON-LARCENY.txt
    rm -rf examples/CommonLarceny
fi

# Remove README.md because that only makes sense for the development site.
rm README.md

# Remove the src directory, since that's not part of the binary distribution.
rm -rf src

# Remove the test directory, since that's not part of the binary distribution.
rm -rf test

# Remove the tools directory, since that's not part of the binary distribution.
rm -rf tools

# Remove the bin directory, since we don't use that yet
rm -rf bin

# Remove the setup script, since that's confusing
rm -f setup.sch

# Remove the larceny-np script, since that's confusing
if [ -e larceny.np ]; then
    rm -f larceny-np
fi

# Remove the include directory from the non-Petit distributions.
if [ ! -e petit.bin ] && [ ! -e petit-larceny.bin ] && [ ! -e petit.bin.exe ];
  then
    rm -rf include
fi

# For Petit:
if [ -e petit.bin ] || [ -e petit-larceny.bin ] || [ -e petit.bin.exe ]
#    remove the .o files
then find . -name '*.o' -type f | xargs rm -f
#    remove the generated .c files
     rm -f petit.bin.c petit-larceny.bin.c twobit.bin.c HEAPDATA.c
#      (this loop detects Scheme->C generated files)
     for f in `find . -name '*.c' | sed -e 's/.c$//' ` ; do 
	 if [ -e $f.sch ] ; then rm $f.c; fi; 
     done
fi

# For Common Larceny:
if [ -e Larceny.fasl ]; then
    mv aside/src .
    rmdir aside

    find . -name '*.lap'        \
        -o -name '*.lop'        \
        -o -name '*.manifest'   \
        -o -name '*.il'         \
        -o -name '*.code-il'    \
        -o -name '*.asm-il'     \
        | xargs rm -f
fi

# Remove heaps and scripts that would be confusing and waste space
rm -f sasstrap.heap arm.heap
rm -f twobit.heap twobit.bin twobit twobit.bat
rm -f petit.heap petit.bin petit

# On Ubuntu, as of the v0.99 release in May 2016, the FFI pre-compilation
# step below was generating multiple copies of this error message:
#
#     dlopen error: /usr/lib/libresolv.so: cannot open shared object file:
#         No such file or directory
#     ffi/load-libraries: /usr/lib/libresolv.so can't be opened.
#
# It looks as though Ubuntu's libresolv.so now resides within the
# /lib/x86_64-linux-gnu/ directory.  Despite these errors, the
# pre-compilation step seems to work.  We are no longer supporting
# the Gambit-hosted version of Snow, however, having gone over to
# Alex Shinn's version of Snow.  The FFI pre-compilation thus seems
# unnecessary.  Compiling the R7RS/R6RS/SRFI standard libraries
# should also be unnecessary, as that is already done by the build
# step.  (Otherwise it would not be possible to run the R6RS and
# R7RS tests.)  So I've commented out this entire section.

# # For systems with FFI support, we want to compile certain libraries
# # that use the foreign-ctools library ahead of time.
# #    (Felix is not going to do this for Petit systems for v0.94
# #    because it is not clear if we can safely distribute the .so files
# #    that compile-file on Petit Larceny generates.  So basically we're
# #    just doing it for the native distributions.)
# if [ -e larceny.bin ] || [ -e larceny.bin.exe ]
# then 
# # The important files for the v0.94 release are
# # lib/Experimental/socket.sch, lib/Experimental/unix.sch, and
# # lib/Standard/file-system.sch, because Snow relies on them for
# # directory listing and tcpip support.
# #
# # These files tend to assume that their syntax dependencies will be
# # handled automagically by require, so the safest way to compile them
# # is to first load the file (so that the syntactic environment will be
# # extended with any necessary dependencies) and then compile the file.
# cat > ${TEMPSCM} <<EOF
# (for-each (lambda (f) (load f) (compile-file f)) 
#           (list "lib/Experimental/socket.sch" 
#                 "lib/Experimental/unix.sch" 
#                 "lib/Standard/file-system.sch"))
# (exit)
# EOF
#     ./larceny -- ${TEMPSCM}
# 
# # For v0.95 and later, the important files are the ERR5RS/R6RS/R7RS
# # standard libraries.  We have documentation on how to build them in
# # doc/HOWTO-BUILD; the below is adapted from the steps there.
# cat > ${TEMPSCM} <<EOF
# (require 'r7rsmode)
# (larceny:compile-r7rs-runtime)
# (exit)
# EOF
#     ./larceny -- ${TEMPSCM}
# 
# fi

# Files generated by foreign-ctools.  (Arguably foreign-ctools should
# be cleaning up after itself.)
rm -f larceny-c-info.c larceny-c-info-output larceny-c-info

# Get rid of confusing documentation and documentation source.

rm -rf doc/CommonLarceny
rm -rf doc/DevManual
rm -rf doc/OldDocs
rm -f doc/HOWTO-ERR5RS
rm -f doc/KNOWN-BUGS
rm -f doc/larcenydoc.conf
rm -f doc/Makefile
rm -f doc/index.html
rm -f doc/UserManual/*

# FIXME: this next part is disabled until we move to a new web server

# # Fetch current user manual and standards.
# 
# mkdir doc/UserManual/user-manual.chunked
# cd doc/UserManual/user-manual.chunked
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s02.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s03.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s04.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s05.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s06.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s07.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s08.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s09.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s10.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s11.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s12.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ar01s13.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/index.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.chunked/ix01.html
# 
# cd ../../UserManual
# wget http://larceny.ccs.neu.edu/doc/index.html
# wget http://larceny.ccs.neu.edu/doc/builddate.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.pdf
# wget http://larceny.ccs.neu.edu/doc/user-manual-alt.html
# wget http://larceny.ccs.neu.edu/doc/user-manual.html
# wget http://larceny.ccs.neu.edu/doc/larceny-notes.html
# 
# cd ../../doc
# wget http://larceny.ccs.neu.edu/nightly/r5rs.pdf
# wget http://larceny.ccs.neu.edu/nightly/r6rs.pdf
# wget http://larceny.ccs.neu.edu/nightly/r6rs-lib.pdf
# wget http://larceny.ccs.neu.edu/nightly/r7rs.pdf
# 
# maybe_mv LarcenyNotes UserManual
# 
# cd ..
