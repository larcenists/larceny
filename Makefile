# Emacs: please stick to -*- fundamental -*- mode, will you?
#
# Makefile for Larceny, version 0.23
#
# This is the top-level makefile. The Makefile for building the runtime,
# as well as configuration options, is Rts/Makefile.

# It is important to keep the version number correct.
VERSION=0.23

# Directories
RTS=rts-$(VERSION)
SYS=$(RTS)/Sys
MACH=$(RTS)/Sparc
BUILD=$(RTS)/Build
ASM=Sparcasm
LIB=Lib
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
MISCFILES=CHGLOG BUGS WISHLIST Makefile nbuild larceny.1 \
	loadcompiler.sch rewrite
ASMFILES=$(ASM)/*.sch
LIBFILES=$(LIB)/*.sch $(LIB)/*.mal $(LIB)/Eval/*.sch
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
	rm -f larceny exlarceny sclarceny Rts Build
	ln -s Rts/larceny
	ln -s Rts/exlarceny
	ln -s Rts/sclarceny
	ln -s $(RTS) Rts
	ln -s $(RTS)/Build Build
	(cd Rts ; $(MAKE) setup)
	$(MAKE) chezstuff

larceny: 
	(cd $(RTS) ; $(MAKE) VERSION=$(VERSION) larceny)

sclarceny:
	(cd $(RTS) ; $(MAKE) VERSION=$(VERSION) sclarceny)

exlarceny:
	(cd $(RTS) ; $(MAKE) VERSION=$(VERSION) exlarceny)

clean:
	(cd $(RTS) ; $(MAKE) clean)
	rm -f *.map

lopclean:
	rm -f $(LIB)/*.lop $(LIB)/Eval/*.lop

libclean:
	rm -f $(LIB)/*.lap $(LIB)/*.lop 
	rm -f $(LIB)/Eval/*.lap $(LIB)/Eval/*.lop

realclean: clean libclean
	rm -f larceny sclarceny exlarceny Build Rts
	rm -f Chez/*.o
	(cd $(RTS) ; $(MAKE) realclean)

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
	( cd Rts ; $(MAKE) Build/schdefs.h )

# For Chez-hosted system

chezstuff: 
	(cd Chez ; $(CC) -c bitpattern.c mtime.c)

# eof
