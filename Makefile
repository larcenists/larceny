# Copyright 1998 Lars T Hansen.          -*- fundamental -*- 
#
# $Id$
#
# Larceny -- top-level Makefile


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
	@echo "  setup          - initialize system"
	@echo "  bdw_setup      - unpack Boehm-Demers-Weiser collector"
	@echo "  larceny.bin    - build standard generational system"
	@echo "  bdwlarceny.bin - build conservative collector system"
	@echo "  hsplit         - build heap splitter"
	@echo "  clean          - remove executables and objects"
	@echo "  lopclean       - remove all .LOP files"
	@echo "  libclean       - remove all .LAP and .LOP files"
	@echo "  soclean        - remove all .so files"
	@echo "  tildeclean     - remove all *~ files"
	@echo "  faslclean      - remove all .FASL files"
	@echo "  realclean      - remove all generated and backup files"

setup:
	rm -f bdwlarceny.bin hsplit larceny.bin Build
	ln -s Rts/larceny.bin
	ln -s Rts/bdwlarceny.bin
	ln -s Rts/hsplit
	ln -s Rts/Build
	mv nbuild nbuild.safe
	sed "s|^LARCENY=.* #@LARCENY_DEF@.*\$$|LARCENY=`pwd` #@LARCENY_DEF@|" < nbuild.safe > nbuild || \
	   ( echo "nbuild hack failed!"; mv nbuild.safe nbuild; exit 1 )
	rm nbuild.safe
	chmod a+x nbuild
	(cd Rts ; $(MAKE) setup)
	$(MAKE) chezstuff

bdw_setup:
	( cd Rts ; $(BDW_UNZIP) < ../$(BDW_DIST) | tar xvf - ; mv gc bdw-gc );

larceny.bin: target_larceny
target_larceny:
	( cd Rts ; $(MAKE) larceny.bin )

bdwlarceny.bin: target_bdwlarceny
target_bdwlarceny:
	( cd Rts ; $(MAKE) bdwlarceny.bin )

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
	rm -f Ffi/*.fasl

rtsclean:
	rm -f *.map
	rm -f larceny petit-larceny Build hsplit
	rm -f Compat/Chez/*.o
	( cd Rts ; $(MAKE) rtsclean )

realclean: clean libclean tildeclean rejclean soclean tcovclean faslclean
	rm -f larceny.bin Build hsplit bdwlarceny.bin *.heap 
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
