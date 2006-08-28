# $Id$
# Compat/Gambit-C/Makefile
#
# Tested with CC=gcc only.

default:

compat:
	rm -f gsi-ffs.o*
	gsc gsi-ffs.scm
	$(CC) -shared -fPIC -D___DYNAMIC gsi-ffs.c gsi-ffs_.c -o gsi-ffs.o1 \
		-lc -lgambc

realclean:
	rm -f gsi-ffs.o*
	rm -f gsi-ffs.c

# eof
