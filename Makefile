# -*- fundamental -*- 
#
# Larceny -- top-level Makefile
#
# $Id$


###########################################################################
#
# User configuration

# Boehm-Demers-Weiser garbage collector

BDW_DIST=bdw-gc-4.12.tar.gz

# Programs (note also setup in Rts/Makefile).

COMPRESS=gzip
Z=gz
CC=gcc

# End user configuration
#
###########################################################################

# Directories

FFI=Ffi
EXPERIMENTAL=Experimental
RTS=Rts
SYS=$(RTS)/Sys
BUILD=$(RTS)/Build
UTIL=Util
ASM=Asm
LIB=Lib
EVAL=Eval
REPL=Repl
AUXLIB=Auxlib
TEST=Testsuite
COMP=Compiler
TEXT=Text
HTML=HTML

# Lists of files
# CCFG, ACFG, SCFG, and HDRFILES also exist in $(RTS)/Makefile. Watch it!

CCFG=$(BUILD)/globals.ch $(BUILD)/except.ch $(BUILD)/layouts.ch

ACFG=$(BUILD)/globals.ah $(BUILD)/regs.ah $(BUILD)/except.ah \
	$(BUILD)/layouts.ah $(BUILD)/mprocs.ah

SCFG=$(BUILD)/globals.sh $(BUILD)/regs.sh $(BUILD)/except.sh \
	$(BUILD)/layouts.sh 

HDRFILES=$(CCFG) $(ACFG) $(SCFG)


# These exist only in this file

MISCFILES=COPYRIGHTS README README-0.?? CHGLOG Makefile nbuild
BUGSFILES=BUGS BUGS-FIXED BUGS-RETIRED
ASMFILES=$(ASM)/Common/*.sch $(ASM)/Sparc-old/*.sch $(ASM)/Sparc/*.sch \
	$(ASM)/MacScheme/*.sch $(ASM)/Standard-C/*.sch
LIBFILES=$(LIB)/*.sch $(LIB)/*.mal $(EVAL)/*.sch $(REPL)/*.sch
CHEZFILES=Compat/Chez/*.c Compat/Chez/*.ss Compat/Chez/*.h Compat/Chez/*.sch
LARCFILES=Compat/Larceny/*.sch
COMPFILES=$(COMP)/*.sch $(COMP)/help-topics.txt
TEXTFILES=$(TEXT)/*.tex
AUXFILES=$(AUXLIB)/*.sch $(AUXLIB)/*.mal
TESTFILES=$(TEST)/Lib/*.sch $(TEST)/Lib/*.mal $(TEST)/Lib/README \
	$(TEST)/GC/*.sch $(TEST)/GC/README $(TEST)/*.sch
HTMLFILES=$(HTML)/*.html
FFIFILES=$(FFI)/*.sch $(FFI)/*.txt $(FFI)/README

# Only a subset of experimental code is distributed.
EXPERIMENTALFILES=$(EXPERIMENTAL)/record.sch $(EXPERIMENTAL)/record.doc \
	Debugger/*.sch $(EXPERIMENTAL)/applyhook.sch \
	$(EXPERIMENTAL)/applyhook0.mal $(EXPERIMENTAL)/apropos.sch \
	$(EXPERIMENTAL)/system-stuff.sch

SPARCFILES=Rts/Sparc/*.s Rts/Sparc/*.h Rts/Sparc/*.c
PETITFILES=Rts/Standard-C/*.h Rts/Standard-C/*.c

RTSFILES0=$(RTS)/Makefile $(RTS)/config $(RTS)/*.cfg $(RTS)/Util/*.c \
	$(SYS)/*.c $(SYS)/*.h $(SPARCFILES) $(PETITFILES) \
	$(UTIL)/*.sch $(UTIL)/modules.list

# Files for 'rtstar'
RTSFILES=$(RTSFILES0) $(HDRFILES) $(BUILD)/*.s

# Files for 'tar'
ALLFILES=$(MISCFILES) $(RTSFILES) $(ASMFILES) $(LIBFILES) $(AUXFILES) \
	$(CHEZFILES) $(LARCFILES) $(COMPFILES)

# Files for 'dist'
DISTFILES=$(MISCFILES) $(BUGSFILES) $(RTSFILES0) $(ASMFILES) $(LIBFILES) \
	$(AUXFILES) $(CHEZFILES) $(LARCFILES) $(COMPFILES) \
	$(HDRFILES) $(BUILD)/table.s $(FFIFILES) $(EXPERIMENTALFILES) \
	$(TESTFILES)

# Files for 'bigtar'
MOREFILES=$(RTS)/larceny larceny.heap larceny.eheap \
	$(TEXTFILES) $(HTMLFILES) $(TESTFILES)

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
	@echo "  rtstar     - tar up all RTS sources"
	@echo "  tar        - RTS and library sources"
	@echo "  dist       - Distribution tar file"
	@echo "  bigtar     - RTS and library sources; gsgc and scgc binaries;"
	@echo "               gsgc and scgc heaps."
	@echo "  hugetar    - Everything."

setup:
	rm -f bdwlarceny hsplit larceny Build
	ln -s $(RTS)/larceny
	ln -s $(RTS)/bdwlarceny
	ln -s $(RTS)/hsplit
	ln -s $(RTS)/Build
	(cd $(RTS) ; $(MAKE) setup)
	$(MAKE) chezstuff

bdw_setup:
	( cd $(RTS) ; gunzip < ../$(BDW_DIST) | tar xvf - ; mv gc bdw-gc );

larceny: target_larceny
target_larceny:
	( cd $(RTS) ; $(MAKE) larceny )

bdwlarceny: target_bdwlarceny
target_bdwlarceny:
	( cd $(RTS) ; $(MAKE) bdwlarceny )

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
		$(TEST)/Lib/*.*lop

libclean: lopclean
	rm -f   $(LIB)/*.lap \
		$(EVAL)/*.lap \
		$(REPL)/*.lap \
		$(AUXLIB)/*.lap \
		$(TEST)/Lib/*.lap

soclean:
	rm -f $(LIB)/*.so
	rm -f $(COMP)/*.so
	rm -f $(ASM)/Common/*.so $(ASM)/Sparc/*.so
	rm -f Compat/Chez/*.so

faslclean:
	rm -f Compiler/*.fasl
	rm -f Asm/Common/*.fasl Asm/Sparc/*.fasl Asm/Standard-C/*.fasl
	rm -f Experimental/*.fasl
	rm -f Compat/Larceny/*.fasl
	rm -f Util/*.fasl
	rm -f Lib/makefile.fasl Lib/globals.sch Lib/ecodes.sch
	rm -f Auxlib/*.fasl
	rm -f Testsuite/GC/*.fasl
	rm -f Testsuite/Lib/*.fasl

rtsclean: clean
	rm -f larceny Build hsplit
	rm -f Compat/Chez/*.o
	( cd $(RTS) ; $(MAKE) rtsclean )

realclean: clean libclean tildeclean rejclean soclean tcovclean faslclean
	rm -f larceny Build hsplit bdwlarceny larceny.heap
	rm -f Compat/Chez/*.o
	rm -f $(TEST)/GC/bb.out*
	( cd $(RTS) ; $(MAKE) realclean )

tcovclean:
	rm -f `find . -name '*\.tcov' -print`

tildeclean:
	rm -f `find . -name '*~' -print`

rejclean:
	rm -f `find . -name '*\.rej' -print`

rtstar:
	tar cf $(RTSTAR) $(RTSFILES)
	if [ ! -b $(RTSTAR) -a ! -c $(RTSTAR) ]; then \
		$(COMPRESS) $(RTSTAR); fi

tar:
	tar cf $(TARFILE) $(ALLFILES)
	if [ ! -b $(TARFILE) -a ! -c $(TARFILE) ]; then \
		$(COMPRESS) $(TARFILE); fi

dist:
	-tar cf $(TARFILE) $(DISTFILES)
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
	( cd Compat/Chez ; $(CC) -c bitpattern.c mtime.c )

# eof
