# Copyright 1998 Lars T Hansen.		 -*- fundamental -*- 
#
# $Id$
#
# Just some targets to clean out generated files -- not very interesting.
#
# To learn how to build Larceny or Petit Larceny, read the file
# Docs/HOWTO-BUILD.

default:
	@echo ""
	@echo "Make what?"
	@echo ""
	@echo "Your options are:"
	@echo "  clean          - remove executables and objects"
	@echo "  realclean      - remove all generated and backup files"
	@echo "  lopclean       - remove all .LOP files"
	@echo "  libclean       - remove all .LAP and .LOP files"
	@echo "  soclean        - remove all .so files"
	@echo "  tildeclean     - remove all *~ files"
	@echo "  faslclean      - remove all .FASL files"

clean: libclean rtsclean compilerclean faslclean
	rm -f core

libclean:
	( cd Lib ; $(MAKE) clean )
	( cd Interpreter ; $(MAKE) clean )
	( cd Repl ; $(MAKE) clean )
	( cd Auxlib ; $(MAKE) clean )
	( cd Twobit ; $(MAKE) libclean )

rtsclean:
	( cd Rts ; \
	  rm -f Makefile larceny.bin hsplit bdwlarceny.bin petit-larceny core \
	   libpetit.lib libpetit.so libpetit.dylib libpetit.a libpetit.dll \
	   Build/*.o Intel/*.o Sparc/*.o Standard-C/*.o Sys/*.o Util/*.o \
	   Build/*.obj Intel/*.obj Sparc/*.obj Standard-C/*.obj Sys/*.obj Util/*.obj ; \
	  if [ -d bdw-gc ]; then ( cd bdw-gc ; make clean ); fi )

compilerclean:
	( cd Twobit ; $(MAKE) clean )

realclean: rtsclean tildeclean rejclean tcovclean faslclean lapclean lopclean
	rm -f core *.heap petit petit.c petit.o twobit twobit.c twobit.o \
		HEAPDATA.* *.fasl libheap.a
	( cd Lib ; $(MAKE) realclean )
	( cd Interpreter ; $(MAKE) realclean )
	( cd Repl ; $(MAKE) realclean )
	( cd Auxlib ; $(MAKE) realclean )
	( cd Twobit ; $(MAKE) realclean )
	( cd Compat ; $(MAKE) realclean )
	( cd Rts ; rm -rf Standard-C/arithmetic.c Build )

# These are pretty coarse; use with some discretion.

faslclean:
	rm -f `find . -name '*\.fasl' -print`

soclean:
	rm -f `find . -name '*\.so' -print`

tcovclean:
	rm -f `find . -name '*\.tcov' -print`
	rm -f `find . -name 'bb\.out*' -print`

tildeclean:
	rm -f `find . -name '*~' -print`

rejclean:
	rm -f `find . -name '*\.rej' -print`

lapclean:
	rm -f `find . -name '*\.lap' -print`

lopclean: 
	rm -f `find . -name '*\.lop' -print`

# eof
