# Emacs: Please stick to -*- fundamental -*- mode.
#
# Rts/Makefile
# Larceny -- the main Makefile
#
# $Id: Makefile,v 1.10 1997/05/31 01:48:56 lth Exp lth $


############################################################################
#
# Section 1: Target OS selection

# Uncomment the following to compile for SunOS 4.x (aka Solaris 1.x)
TARGETOS=SUNOS
EXTRALIBS=
EXTRALIBPATH=

# Uncomment the following to compile for Solaris (aka SunOS 5.x)
#TARGETOS=SOLARIS
#EXTRALIBS=-lucb
#EXTRALIBPATH=-L/usr/ucblib


###########################################################################
#
# Section 2: Feature configuration section.

# Remembered set computation.
#
# By setting SIMULATE_NEW_BARRIER to 1, then doing a make clean ; make larceny,
# and making sure that all Scheme code was in fact compiled with no in-line
# write barrier checking, Larceny will switch to a (very slow) write barrier
# that simulates and gathers accurate statistics for the planned write
# barrier.

SIMULATE_NEW_BARRIER=0


# Stack underflow statistics.
#
# By setting STACK_UNDERFLOW_COUNTING to 1, the number of stack underflows 
# are recorded at a slight (probably negligible) performance penalty.

STACK_UNDERFLOW_COUNTING=1


# Set this to 0 at your own risk.

NP_EXTRA_REMSET=1


# Instruction cache flushing.
#
# In general, Larceny detects automatically whether cache flushing needs
# to be performed on the architecture Larceny is running on. You can,
# however, override this behaviour by uncommenting one of the following
# lines and recompiling Rts/$(MACH)/cache.c and relinking.

#IFLUSH=-DFLUSH_ALWAYS
#IFLUSH=-DFLUSH_NEVER


# Division by zero handling.
#
# Currently all checking is explicit, so don't undefine this.

DIVZ=-DCHECK_DIVISION_BY_ZERO


############################################################################
#
# Directory paths relative to the larceny top-level directory.

# Architecture-independent stuff
SYS=Sys

# Subdirectory for machine-dependent stuff.
MACH=Sparc

# Build directory
BUILD=Build

# Conservative GC directory
BDW_DIR=bdw-gc


############################################################################
#
# Compiler flags
#
# Defining DEBUG adds some debugging output.
# Defining DEBUG2 may or may not add a _lot_ of debugging output (old switch)

CONFIG=-DSIMULATE_NEW_BARRIER=$(SIMULATE_NEW_BARRIER) \
	-DSTACK_UNDERFLOW_COUNTING=$(STACK_UNDERFLOW_COUNTING) \
	-DNP_EXTRA_REMSET=$(NP_EXTRA_REMSET)

CC=gcc
AS=as
LIBS=-lm

# A mixed blessing, at best, whatwith all the casting going on (or not).
WARN=#-W #-Wimplicit -Wformat -Wpointer-arith

DEBUGINFO=-g
DEBUGFLAGS=#-DDEBUG # -DGCLIB_DEBUG -DDEBUG2 
PROFILE=#-p
OPTIMIZE=-DNDEBUG -O3
ARCHINFO=-DENDIAN_BIG -DBITS_32

# You typically want this to remain on, unless you want multiple collectors
# in the same process (beyond the realm of the possible, at the moment).  
# See Sys/bdw-collector.c for details, and grovel over the source tree :-)
BDW_DEBUG=-DBDW_DEBUG #-DBDW_CLEAR_STACK

CFLAGS=	-c $(DEBUGINFO) $(OPTIMIZE) $(PROFILE) -D$(TARGETOS) $(CONFIG) \
	-I$(SYS) -I$(BUILD) $(DEBUGFLAGS) $(IFLUSH) $(DIVZ) $(BDW_GC) \
	$(ARCHINFO) $(WARN) $(BDW_DEBUG)

ASFLAGS= -P -I$(MACH) -I$(BUILD) -D$(TARGETOS) $(CONFIG) $(DEBUGFLAGS) \
	$(IFLUSH) $(DIVZ) $(BDW_GC)


############################################################################
#
# Big bags of files
#
# GSGC_OBJS are the object files to link for a generation-scavenging collector.

# Common C objects

COBJS=$(SYS)/stack.o $(SYS)/cglue.o $(SYS)/heapio.o \
	$(SYS)/version.o $(SYS)/ldebug.o $(SYS)/unix.o\
	$(SYS)/argv.o $(SYS)/malloc.o $(SYS)/util.o \
	$(MACH)/cache.o

# Common ASM objects

ASMOBJS=$(MACH)/mcode.o $(MACH)/glue.o $(BUILD)/table.o \
	$(MACH)/generic.o $(MACH)/cache0.o $(MACH)/barrier.o

# C and asm objects for all GCs except the Boehm collector

ALL_GC=$(SYS)/larceny.o $(SYS)/memmgr.o $(SYS)/remset.o \
	$(SYS)/young-heap.o $(SYS)/old-heap.o $(SYS)/sc-heap.o \
	$(SYS)/unix-alloc.o $(SYS)/semispace.o $(SYS)/cheney.o \
	$(SYS)/barrier.o $(SYS)/np-sc-heap.o $(SYS)/static-heap.o \
	$(MACH)/memory.o $(SYS)/stats.o $(SYS)/gc.o

# C and asm objects for the Boehm collector.  The source files for
# these invariably #define BDW_GC and then #include the other source
# file.

BOEHM_GC=$(SYS)/bdw-larceny.o $(SYS)/bdw-gc.o $(SYS)/bdw-stats.o \
	$(SYS)/bdw-collector.o $(MACH)/bdw-memory.o 

BOEHMLIB=bdw-gc/gc.a

# Standard generational collector: multiple copying generations

ALL_GC_OBJS=$(COBJS) $(ASMOBJS) $(ALL_GC)


# Boehm-Demers-Weiser conservative collector

BOEHM_GC_OBJS=$(COBJS) $(ASMOBJS) $(BOEHM_GC)


# Header files.
#
# ACFG, CCFG, SCFG, and HDRFILES also exists in ../Makefile. Watch it!

CCFG=$(BUILD)/globals.ch $(BUILD)/except.ch $(BUILD)/layouts.ch

ACFG=$(BUILD)/globals.ah $(BUILD)/regs.ah $(BUILD)/except.ah \
	$(BUILD)/layouts.ah $(BUILD)/mprocs.ah

SCFG=$(BUILD)/globals.sh $(BUILD)/regs.sh $(BUILD)/except.sh \
	$(BUILD)/layouts.sh 

HDRFILES=$(CCFG) $(ACFG) $(SCFG)

HDRS=$(SYS)/larceny.h $(SYS)/macros.h $(SYS)/gc.h $(BUILD)/cdefs.h

GCHDRS=$(SYS)/memmgr.h $(SYS)/barrier.h $(SYS)/gclib.h $(SYS)/semispace.h


############################################################################
#
# Rules

.SUFFIXES:	.cfg .ch .ah .sh

.cfg.ch:
	./config $(TARGETOS) $<
.cfg.ah:
	./config $(TARGETOS) $<
.cfg.sh:
	./config $(TARGETOS) $<
.s.o:
	$(AS) $(ASFLAGS) -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -DUSER=\"$$LOGNAME\" -DDATE="\"`date`\"" -o $*.o $<

############################################################################
#
# Targets

default:
	@echo "Please give me a target."

setup:
	if [ ! -d Build ]; then mkdir Build; fi
	(cd Build; for i in ../*.cfg; do ln -s $$i ; done)

larceny: $(ALL_GC_OBJS)
	$(CC) $(PROFILE) -o larceny \
		$(ALL_GC_OBJS) $(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) 
	/bin/rm -f $(SYS)/version.o

bdwlarceny: $(BOEHM_GC_OBJS)
	( cd $(BDW_DIR) ; make gc.a )
	$(CC) $(PROFILE) -o bdwlarceny $(BOEHM_GC_OBJS) \
		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) \
		$(BOEHMLIB)
	/bin/rm -f $(SYS)/version.o

hsplit: $(SYS)/hsplit.o
	$(CC) $(PROFILE) $(DEBUGINFO) -o hsplit $(SYS)/hsplit.o

$(SYS)/hsplit.o: $(SYS)/hsplit.c
	$(CC) -g -o $(SYS)/hsplit.o -I$(BUILD) -I$(SYS) -c $(SYS)/hsplit.c

clean:
	rm -f larceny hsplit core $(SYS)/*.o $(MACH)/*.o $(BUILD)/*.o

rtsclean: clean
	rm -f $(BUILD)/*.s $(BUILD)/*.*h

realclean: clean
	( cd $(BDW_DIR) ; make clean )
	rm -f $(BUILD)/*


###########################################################################
#
# sources

$(BUILD)/cdefs.h:	$(CCFG)
	cat $(CCFG) > $(BUILD)/cdefs.h

$(BUILD)/asmdefs.h:	$(ACFG)
	cat $(ACFG) > $(BUILD)/asmdefs.h

$(BUILD)/schdefs.h:	$(SCFG)
	cat $(SCFG) > $(BUILD)/schdefs.h

$(MACH)/bdw-memory.o: $(MACH)/memory.s
$(SYS)/bdw-gc.o: $(SYS)/gc.c
$(SYS)/bdw-larceny.o: $(SYS)/larceny.c
$(SYS)/bdw-stats.o: $(SYS)/stats.c

$(COBJS): $(HDRS)
$(ALL_GC): $(HDRS) $(GCHDRS)
$(BOEHM_GC): $(HDRS)

$(ASMOBJS): $(BUILD)/asmdefs.h

$(BUILD)/table.s: $(BUILD)/globals.cfg
	./config $(TARGETOS) $(BUILD)/globals.cfg

# eof

