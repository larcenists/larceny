# $Id$
# Experimental/Makefile

default:

libclean:
	rm -f *.lap *.lop

clean realclean: libclean
	rm -f *.seed *.fasl *.o *.so

# Solaris, at least
#unix-support.so:
#	gcc -fPIC -shared unix-support.c -lsocket -lnsl -o unix-support.so

# Linux
unix-support.so:
	gcc -fPIC -shared unix-support.c -lnsl -o unix-support.so

# eof
