# Emacs: just stick to -*- fundamental -*- mode, will you?
#
# Makefile for Larceny
# $Id: Makefile,v 1.2 1995/08/01 04:36:02 lth Exp lth $
#
# This is the top-level makefile. The Makefile for building the runtime,
# as well as configuration options, is Rts/Makefile.

# Directories
RTS=Rts
SYS=$(RTS)/Sys
MACH=$(RTS)/Sparc
BUILD=$(RTS)/Build
ASM=Sparcasm
LIB=Lib
EVAL=Lib/Eval
COMP=Compiler
TEXT=Text

# Lists of files
# CCFG, ACFG, SCFG, and HDRFILES also exist in $(RTS)/Makefile. Watch it!
CCFG=$(BUILD)/globals.ch $(BUILD)/except.ch $(BUILD)/layouts.ch

ACFG=$(BUILD)/globals.ah $(BUILD)/regs.ah $(BUILD)/except.ah \
	$(BUILD)/layouts.ah $(BUILD)/mprocs.ah

SCFG=$(BUILD)/globals.sh $(BUILD)/regs.sh $(BUILD)/except.sh \
	$(BUILD)/layouts.sh 

HDRFILES=$(CCFG) $(ACFG) $(SCFG)

# These exist only in this file
MISCFILES=COPYRIGHTS CHGLOG Makefile nbuild larceny.1 \
	loadcompiler.sch rewrite
ASMFILES=$(ASM)/*.sch
LIBFILES=$(LIB)/*.sch $(LIB)/*.mal $(EVAL)/*.sch
CHEZFILES=Chez/*.c Chez/*.ss Chez.*.h
COMPFILES=$(COMP)/*.sch
TEXTFILES=$(TEXT)/*.tex

# Files for 'rtstar'
RTSFILES=$(RTS)/Makefile $(RTS)/config $(RTS)/*.cfg $(RTS)/Makefile \
	$(SYS)/*.c $(SYS)/*.h $(MACH)/*.s $(MACH)/*.h $(HDRFILES) $(BUILD)/*.s

# Files for 'tar'
ALLFILES=$(MISCFILES) $(RTSFILES) $(ASMFILES) $(LIBFILES) \
	$(CHEZFILES) $(COMPFILES) $(TEXTFILES)

# Files for 'bigtar'
MOREFILES=$(RTS)/larceny larceny.heap $(RTS)/sclarceny larceny.eheap

# Targets
default:
	@echo "Make what?"
	@echo "Your options are:"
	@echo "  setup      - initialize system"
	@echo "  larceny    - build standard generational system"
	@echo "  sclarceny  - build stop-and-copy system"
	@echo "  exlarceny  - build experimental generational system"
	@echo "  clean      - remove executables and objects"
	@echo "  lopclean   - remove all .LOP files"
	@echo "  libclean   - remove all .LAP and .LOP files"
	@echo "  realclean  - remove everything, included generated headers"
	@echo "  rtstar     - tar up all RTS sources"
	@echo "  tar        - RTS and library sources"
	@echo "  bigtar     - RTS and library sources; gsgc and scgc binaries;"
	@echo "               gsgc and scgc heaps."

setup:
	rm -f larceny exlarceny sclarceny Build
	ln -s $(RTS)/larceny
	ln -s $(RTS)/exlarceny
	ln -s $(RTS)/sclarceny
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

clean:
	( cd $(RTS) ; $(MAKE) clean )
	rm -f *.map

lopclean:
	rm -f $(LIB)/*.lop $(EVAL)/*.lop

libclean:
	rm -f $(LIB)/*.lap $(LIB)/*.lop 
	rm -f $(EVAL)/*.lap $(EVAL)/*.lop

realclean: clean libclean
	rm -f larceny sclarceny exlarceny Build
	rm -f Chez/*.o
	( cd $(RTS) ; $(MAKE) realclean )

rtstar:
	tar cvf larceny-rts.tar $(RTSFILES)
	compress larceny-rts.tar

tar:
	tar cvf larceny.tar $(ALLFILES)
	compress larceny.tar

bigtar:
	tar cvf larceny-all.tar $(ALLFILES) $(MOREFILES)
	compress larceny-all.tar

Build/schdefs.h:
	@( cd $(RTS) ; $(MAKE) Build/schdefs.h )

# For Chez-hosted system

chezstuff: 
	( cd Chez ; $(CC) -c bitpattern.c mtime.c )

# eof
