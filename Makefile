# -*- fundamental -*- 
#
# Larceny -- top-level Makefile
#
# $Id: Makefile,v 1.6 1997/02/11 21:48:32 lth Exp $
#
# This is the top-level makefile. The Makefile for building the runtime,
# as well as configuration options, is Rts/Makefile.

# Directories
RTS=Rts
SYS=$(RTS)/Sys
MACH=$(RTS)/Sparc
BUILD=$(RTS)/Build
UTIL=Util
ASM=Sparcasm
LIB=Lib
EVAL=Eval
REPL=Repl
AUXLIB=Auxlib
TEST=Test
COMP=Compiler
TEXT=Text

# Programs
#COMPRESS=compress
#Z=Z
COMPRESS=gzip
Z=gz

# Lists of files
# CCFG, ACFG, SCFG, and HDRFILES also exist in $(RTS)/Makefile. Watch it!
CCFG=$(BUILD)/globals.ch $(BUILD)/except.ch $(BUILD)/layouts.ch

ACFG=$(BUILD)/globals.ah $(BUILD)/regs.ah $(BUILD)/except.ah \
	$(BUILD)/layouts.ah $(BUILD)/mprocs.ah

SCFG=$(BUILD)/globals.sh $(BUILD)/regs.sh $(BUILD)/except.sh \
	$(BUILD)/layouts.sh 

HDRFILES=$(CCFG) $(ACFG) $(SCFG)

# These exist only in this file
MISCFILES=COPYRIGHTS Makefile nbuild larceny.1 README CHGLOG
BUGSFILES=BUGS BUGS-FIXED
MISC2FILES=$(BUGSFILES) BUGS-0.25 PROBLEMS
ASMFILES=$(ASM)/*.sch
LIBFILES=$(LIB)/*.sch $(LIB)/*.mal $(EVAL)/*.sch $(REPL)/*.sch $(TEST)/*.sch
CHEZFILES=Chez/*.c Chez/*.ss Chez/*.h Chez/*.sch
COMPFILES=$(COMP)/*.sch
TEXTFILES=$(TEXT)/*.tex
AUXFILES=$(AUXLIB)/*.sch $(AUXLIB)/*.mal
TESTFILES=$(TEST)/*.sch $(TEST)/*.mal $(TEST)/README

RTSFILES0=$(RTS)/Makefile $(RTS)/config $(RTS)/*.cfg \
	$(SYS)/*.c $(SYS)/*.h $(MACH)/*.s $(MACH)/*.h $(MACH)/*.c \
	$(UTIL)/*.sch

# Files for 'rtstar'
RTSFILES=$(RTSFILES0) $(HDRFILES) $(BUILD)/*.s


# Files for 'tar'
ALLFILES=$(MISCFILES) $(RTSFILES) $(ASMFILES) $(LIBFILES) $(AUXFILES) \
	$(CHEZFILES) $(COMPFILES)

# Files for 'distribution'
DISTFILES=$(MISCFILES) $(BUGSFILES) $(RTSFILES0) $(ASMFILES) $(LIBFILES) \
	$(AUXFILES) $(CHEZFILES) $(COMPFILES)

# Files for 'bigtar'
MOREFILES=$(RTS)/larceny larceny.heap $(RTS)/sclarceny larceny.eheap \
	$(TEXTFILES) $(TESTFILES) $(MISC2FILES)

# Tar file names; can be overridden when running make.
RTSTAR=larceny-rts.tar
TARFILE=larceny.tar
ALLTAR=larceny-all.tar
HUGETAR=larceny-huge.tar

# Targets
default:
	@echo "Make what?"
	@echo "Your options are:"
	@echo "  setup      - initialize system"
	@echo "  larceny    - build standard generational system"
	@echo "  sclarceny  - build stop-and-copy system"
	@echo "  exlarceny  - build experimental generational system"
	@echo "  hsplit     - build heap splitter"
	@echo "  clean      - remove executables and objects"
	@echo "  lopclean   - remove all .LOP files"
	@echo "  libclean   - remove all .LAP and .LOP files"
	@echo "  realclean  - remove everything, included generated headers"
	@echo "  rtstar     - tar up all RTS sources"
	@echo "  tar        - RTS and library sources"
	@echo "  dist       - Distribution tar file"
	@echo "  bigtar     - RTS and library sources; gsgc and scgc binaries;"
	@echo "               gsgc and scgc heaps."
	@echo "  hugetar    - Everything."

setup:
	rm -f larceny exlarceny sclarceny Build
	ln -s $(RTS)/larceny
	ln -s $(RTS)/exlarceny
	ln -s $(RTS)/sclarceny
	ln -s $(RTS)/hsplit
	ln -s $(RTS)/Build
	(cd $(RTS) ; $(MAKE) setup)
	$(MAKE) chezstuff

larceny: target_larceny
target_larceny:
	( cd $(RTS) ; $(MAKE) larceny )

sclarceny: target_sclarceny
target_sclarceny:
	( cd $(RTS) ; $(MAKE) sclarceny )

exlarceny: target_exlarceny
target_exlarceny:
	( cd $(RTS) ; $(MAKE) exlarceny )

hsplit: target_hsplit
target_hsplit:
	( cd $(RTS) ; $(MAKE) hsplit )

clean:
	( cd $(RTS) ; $(MAKE) clean )
	rm -f *.map

lopclean:
	rm -f   $(LIB)/*.*lop \
		$(EVAL)/*.*lop \
		$(REPL)/*.*lop \
		$(AUXLIB)/*.*lop \
		$(TEST)/*.*lop

libclean: lopclean
	rm -f   $(LIB)/*.lap \
		$(EVAL)/*.lap \
		$(REPL)/*.lap \
		$(AUXLIB)/*.lap \
		$(TEST)/*.lap

rtsclean: clean
	rm -f larceny sclarceny exlarceny Build hsplit
	rm -f Chez/*.o
	( cd $(RTS) ; $(MAKE) rtsclean )

realclean: clean libclean
	rm -f larceny sclarceny exlarceny Build hsplit
	rm -f Chez/*.o
	( cd $(RTS) ; $(MAKE) realclean )

rtstar:
	tar cf $(RTSTAR) $(RTSFILES)
	if [ ! -b $(RTSTAR) -a ! -c $(RTSTAR) ]; then \
		$(COMPRESS) $(RTSTAR); fi

tar:
	tar cf $(TARFILE) $(ALLFILES)
	if [ ! -b $(TARFILE) -a ! -c $(TARFILE) ]; then \
		$(COMPRESS) $(TARFILE); fi

dist:
	tar cf $(TARFILE) $(DISTFILES)
	if [ ! -b $(TARFILE) -a ! -c $(TARFILE) ]; then \
		$(COMPRESS) $(TARFILE); fi

bigtar:
	tar cf $(ALLTAR) $(ALLFILES) $(MOREFILES)
	if [ ! -b $(ALLTAR) -a ! -c $(ALLTAR) ]; then \
		$(COMPRESS) $(ALLTAR); fi

hugetar:
	if [ ! -b $(HUGETAR) -a ! -c $(HUGETAR) ]; then \
		rm -f $(HUGETAR) $(HUGETAR).$(Z) ; fi
	tar cf $(HUGETAR) .
	if [ ! -b $(HUGETAR) -a ! -c $(HUGETAR) ]; then \
		$(COMPRESS) $(HUGETAR); fi

backup:
	$(MAKE) tar
	ls -l $(TARFILE).$(Z)
	tar cf /dev/fd0 $(TARFILE).$(Z)
	rm $(TARFILE).$(Z)

Build/schdefs.h:
	@( cd $(RTS) ; $(MAKE) Build/schdefs.h )

# For Chez-hosted system

chezstuff: 
	( cd Chez ; $(CC) -c bitpattern.c mtime.c )

# eof
