# Copyright 1998 Lars T Hansen.		 -*- fundamental -*- 
#
# $Id$
#
# Larceny -- top-level Makefile


###########################################################################
#
# User configuration

# This file name is relative to the Larceny home directory.

BDW_DIST=bdw-gc-4.12.tar.gz
BDW_UNZIP=gunzip
CC=gcc

# End user configuration
#
###########################################################################

default:
	@echo ""
	@echo "Make what?"
	@echo ""
	@echo "Your options are:"
	@echo "  setup_larceny  - setup build system for larceny host"
	@echo "  setup_chez     - setup build system for chez scheme host"
	@echo "  setup_gambit   - setup build system for gambit-c host"
	@echo "  bdw_unpack     - unpack Boehm-Demers-Weiser collector"
	@echo ""
	@echo "  larceny.bin    - build standard generational system"
	@echo "  bdwlarceny.bin - build conservative collector system"
	@echo "  libpetit       - build Rts/libpetit.so"
	@echo "  hsplit         - build heap splitter"
	@echo ""
	@echo "  clean          - remove executables and objects"
	@echo "  realclean      - remove all generated and backup files"
	@echo "  lopclean       - remove all .LOP files"
	@echo "  libclean       - remove all .LAP and .LOP files"
	@echo "  soclean        - remove all .so files"
	@echo "  tildeclean     - remove all *~ files"
	@echo "  faslclean      - remove all .FASL files"


# Configuration

setup:
	@echo "'setup' is no longer a target.  Try one of:"
	@echo "   setup_chez     setup for chez scheme host system"
	@echo "   setup_gambit   setup for gambit-c host system"
	@echo "   setup_larceny  setup for larceny host system"

setup_larceny:
	( SETUP_HOST_NAME=larceny; \
	  export SETUP_HOST_NAME; \
	  $(MAKE) setup_generic; \
	  cd Compat/Larceny ; \
	  $(MAKE) CC=$(CC) compat )

setup_chez:
	( SETUP_HOST_NAME=chez; \
	  export SETUP_HOST_NAME; \
	  $(MAKE) setup_generic; \
	  cd Compat/Chez ; \
	  $(MAKE) CC=$(CC) compat )

setup_gambit:
	( SETUP_HOST_NAME=gambit; \
	  export SETUP_HOST_NAME; \
	  $(MAKE) setup_generic; \
	  cd Compat/Gambit-C ; \
	  $(MAKE) CC=$(CC) compat )

setup_generic:
	rm -f bdwlarceny.bin hsplit larceny.bin
	ln -s Rts/larceny.bin
	ln -s Rts/bdwlarceny.bin
	ln -s Rts/hsplit
	mv build build.safe
	sed \
	 -e "s|^LARCENY=.* #@LARCENY_DEF@.*\$$|LARCENY=`pwd` #@LARCENY_DEF@|" \
	 -e "s|^DEFAULT_BUILD_HOST=.* #@BUILD_HOST_DEF@.*\$$|DEFAULT_BUILD_HOST=$$SETUP_HOST_NAME #@BUILD_HOST_DEF@|" \
	    < build.safe > build || \
	      ( echo "build hack failed!"; mv build.safe build; exit 1 )
	rm build.safe
	chmod a+x build
	(cd Rts ; $(MAKE) setup)

bdw_unpack:
	( cd Rts ; $(BDW_UNZIP) < ../$(BDW_DIST) | tar xvf - ; mv gc bdw-gc );


# Build

larceny.bin: target_larceny
target_larceny:
	( cd Rts ; $(MAKE) larceny.bin )

bdwlarceny.bin: target_bdwlarceny
target_bdwlarceny:
	( cd Rts ; $(MAKE) bdwlarceny.bin )

hsplit: target_hsplit
target_hsplit:
	( cd Rts ; $(MAKE) hsplit )

libpetit: target_libpetit
target_libpetit:
	( cd Rts ; $(MAKE) libpetit.so )

petit.bin:
	$(MAKE) -f makefile.gcc petit.bin


# Cleanup

clean: libclean rtsclean compilerclean

libclean:
	( cd Lib ; $(MAKE) clean )
	( cd Interpreter ; $(MAKE) clean )
	( cd Repl ; $(MAKE) clean )
	( cd Auxlib ; $(MAKE) clean )
	( cd Compiler ; $(MAKE) libclean )

rtsclean:
	( cd Rts ; $(MAKE) clean )

compilerclean:
	( cd Compiler ; $(MAKE) clean )

realclean: tildeclean rejclean tcovclean
	rm -f *.heap 
	rm -f Testsuite/GC/bb.out*
	( cd Lib ; $(MAKE) realclean )
	( cd Interpreter ; $(MAKE) realclean )
	( cd Repl ; $(MAKE) realclean )
	( cd Auxlib ; $(MAKE) realclean )
	( cd Compiler ; $(MAKE) realclean )
	( cd Compat ; $(MAKE) realclean )
	( cd Rts ; $(MAKE) realclean )


# These are pretty coarse; use with some discretion.

faslclean:
	rm -f `find . -name '*\.fasl' -print`

soclean:
	rm -f `find . -name '*\.so' -print`

tcovclean:
	rm -f `find . -name '*\.tcov' -print`

tildeclean:
	rm -f `find . -name '*~' -print`

rejclean:
	rm -f `find . -name '*\.rej' -print`

seedclean:
	rm -f `find . -name '*\.seed' -print`

lapclean:
	rm -f `find . -name '*\.lap -print`

lopclean: 
	rm -f `find . -name '*\.lop -print`

# eof
