# -*- fundamental -*- 
#
# Larceny -- top-level Makefile
#
# $Id$


###########################################################################
#
# User configuration

BDW_DIST=bdw-gc-4.12.tar.gz
BDW_UNZIP=gunzip
CC=gcc

# End user configuration
#
###########################################################################

default:
	@echo "Make what?"
	@echo "Your options are:"
	@echo "  setup      - initialize system"
	@echo "  bdw_setup  - unpack Boehm-Demers-Weiser collector"
	@echo "  larceny    - build standard generational system"
	@echo "  bdwlarceny - build conservative collector system"
	@echo "  hsplit     - build heap splitter"
	@echo "  clean      - remove executables and objects"
	@echo "  lopclean   - remove all .LOP files"
	@echo "  libclean   - remove all .LAP and .LOP files"
	@echo "  soclean    - remove all .so files"
	@echo "  tildeclean - remove all *~ files"
	@echo "  faslclean  - remove all .FASL files"
	@echo "  realclean  - remove everything, included generated headers"

setup:
	rm -f bdwlarceny hsplit larceny Build
	ln -s Rts/larceny
	ln -s Rts/bdwlarceny
	ln -s Rts/hsplit
	ln -s Rts/Build
	(cd Rts ; $(MAKE) setup)
	$(MAKE) chezstuff

bdw_setup:
	( cd Rts ; $(BDW_UNZIP) < ../$(BDW_DIST) | tar xvf - ; mv gc bdw-gc );

larceny: target_larceny
target_larceny:
	( cd Rts ; $(MAKE) larceny )

bdwlarceny: target_bdwlarceny
target_bdwlarceny:
	( cd Rts ; $(MAKE) bdwlarceny )

hsplit: target_hsplit
target_hsplit:
	( cd Rts ; $(MAKE) hsplit )

clean: libclean rtsclean
	( cd Rts ; $(MAKE) clean )

lopclean:
	rm -f   Lib/Common/*.*lop Lib/Common/*.c Lib/Common/*.o \
	        Lib/Sparc/*.*lop Lib/Sparc/*.c Lib/Sparc/*.o \
	        Lib/Standard-C/*.*lop Lib/Standard-C/*.c Lib/Standard-C/*.o \
		Eval/*.*lop Eval/*.c Eval/*.o \
		Repl/*.*lop Repl/*.c Repl/*.o \
		Auxlib/*.*lop \
		Testsuite/Lib/*.*lop

libclean: lopclean
	rm -f   Lib/Common/*.lap Lib/Sparc/*.lap Lib/Standard-C/*.lap \
	        Lib/Common/ecodes.sch Lib/Common/globals.sch \
		Eval/*.lap \
		Repl/*.lap \
		Auxlib/*.lap \
		Testsuite/Lib/*.lap

soclean:
	rm -f Lib/*.so
	rm -f Compiler/*.so
	rm -f Asm/Common/*.so Asm/Sparc/*.so
	rm -f Compat/Chez/*.so

faslclean:
	rm -f Compiler/*.fasl
	rm -f Asm/Common/*.fasl Asm/Sparc/*.fasl Asm/Standard-C/*.fasl
	rm -f Experimental/*.fasl
	rm -f Compat/Larceny/*.fasl
	rm -f Util/*.fasl
	rm -f Lib/makefile.fasl
	rm -f Auxlib/*.fasl
	rm -f Debugger/*.fasl
	rm -f Testsuite/GC/*.fasl
	rm -f Testsuite/Lib/*.fasl

rtsclean:
	rm -f *.map
	rm -f larceny petit-larceny Build hsplit
	rm -f Compat/Chez/*.o
	( cd Rts ; $(MAKE) rtsclean )

realclean: clean libclean tildeclean rejclean soclean tcovclean faslclean
	rm -f larceny Build hsplit bdwlarceny larceny.heap
	rm -f Compat/Chez/*.o
	rm -f Testsuite/GC/bb.out*
	( cd Rts ; $(MAKE) realclean )

tcovclean:
	rm -f `find . -name '*\.tcov' -print`

tildeclean:
	rm -f `find . -name '*~' -print`

rejclean:
	rm -f `find . -name '*\.rej' -print`

Build/schdefs.h:
	@( cd Rts ; $(MAKE) Build/schdefs.h )

# For Chez-hosted system

chezstuff: 
	( cd Compat/Chez ; $(CC) -c bitpattern.c mtime.c )

# eof
