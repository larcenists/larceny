# Emacs: Please stick to -*- fundamental -*- mode.
#
# Makefile for Larceny v0.23 Run-time.

############################################################################
#
# Target OS configuration section

# Uncomment the following to compile for SunOS 4.x (aka Solaris 1.x)
TARGETOS=SUNOS
EXTRALIBS=
EXTRALIBPATH=

# Uncomment the following to compile for Solaris (aka SunOS 5.x)
#TARGETOS=SOLARIS
#EXTRALIBS=-lucb
#EXTRALIBPATH=-L/usr/ucblib

# Uncomment the following line to include instruction cache flush code.
# Cache flushing is needed for correct operation on Sparc10 and Sparc20
# machines, at least.
#IFLUSH=-DIFLUSH

############################################################################
#
# Directory paths relative to the larceny top-level directory.

# Architecture-independent stuff
SYS=Sys

# Subdirectory for machine-dependent stuff.
MACH=Sparc

# Build directory
BUILD=Build


############################################################################
#
# Compiler flags
#
# Defining DEBUG adds some debugging output.
# Defining DEBUG2 may or may not add a _lot_ of debugging output (old switch)

#PROFILE=-pg
#DEBUG=-g
#DFLAGS=-DDEBUG # -DDEBUG2
OPTIMIZE=-O2
CC=gcc
AS=as

CFLAGS=	-c $(DEBUG) $(OPTIMIZE) $(PROFILE) -D$(TARGETOS) -I$(SYS) -I$(BUILD) $(DFLAGS) $(IFLUSH)

ASFLAGS= -P -I$(MACH) -I$(BUILD) -D$(TARGETOS) $(DFLAGS) $(IFLUSH)


############################################################################
#
# Big bags of files
#
# GSGC_OBJS are the object files to link for a generation-scavenging collector.
# SCGC_OBJS are the object files to link for a stop-and-copy collector.
# EXGC_OBJS are the object files to link for the experimental collector.

COBJS=$(SYS)/larceny.o $(SYS)/remset.o $(SYS)/stack.o\
	$(SYS)/cglue.o $(SYS)/heapio.o $(SYS)/malloc.o\
	$(SYS)/memstats.o $(SYS)/version.o $(SYS)/ldebug.o $(SYS)/unix.o

ASMOBJS=$(MACH)/mcode.o $(MACH)/memory.o $(MACH)/glue.o $(BUILD)/table.o \
	$(MACH)/generic.o $(MACH)/unix.o

GSGC_OBJS=$(COBJS) $(ASMOBJS) $(SYS)/policy.o $(SYS)/gc.o

SCGC_OBJS=$(COBJS) $(ASMOBJS) $(SYS)/scpolicy.o $(SYS)/gc.o

EXGC_OBJS=$(COBJS) $(ASMOBJS) $(SYS)/expolicy.o

CCFG=$(BUILD)/globals.ch $(BUILD)/except.ch $(BUILD)/layouts.ch

# ACFG, CCFG, SCFG, and HDRFILES also exists in ../Makefile. Watch it!
ACFG=$(BUILD)/globals.ah $(BUILD)/regs.ah $(BUILD)/except.ah \
	$(BUILD)/layouts.ah $(BUILD)/mprocs.ah

SCFG=$(BUILD)/globals.sh $(BUILD)/regs.sh $(BUILD)/except.sh \
	$(BUILD)/layouts.sh 

HDRFILES=$(CCFG) $(ACFG) $(SCFG)

############################################################################
#
# Rules

.SUFFIXES:	.cfg .ch .ah .sh

.cfg.ch:
	./config $<
.cfg.ah:
	./config $<
.cfg.sh:
	./config $<
.s.o:
	$(AS) $(ASFLAGS) -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -DUSER=\"$$USER\" -DDATE="\"`date`\"" -o $*.o $<

############################################################################
#
# Targets

default:
	@echo "Please give me a target."

setup:
	if [ ! -d Build ]; then mkdir Build; fi
	(cd Build; for i in ../*.cfg; do ln -s $$i ; done)

larceny: $(GSGC_OBJS)
	$(CC) $(PROFILE) -o larceny $(GSGC_OBJS) $(EXTRALIBS) $(EXTRALIBPATH) 
	/bin/rm -f $(SYS)/version.o

sclarceny: $(SCGC_OBJS)
	$(CC) $(PROFILE) -o sclarceny $(SCGC_OBJS) $(EXTRALIBS) $(EXTRALIBPATH)
	/bin/rm -f $(SYS)/version.o

exlarceny: $(EXGC_OBJS)
	$(CC) $(PROFILE) -o exlarceny $(EXGC_OBJS) $(EXTRALIBS) $(EXTRALIBPATH)
	/bin/rm -f $(SYS)/version.o

clean:
	rm -f larceny sclarceny exlarceny 
	rm -f $(GSGC_OBJS) $(SCGC_OBJS) $(EXGC_OBJS)

realclean:
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

$(COBJS): $(SYS)/larceny.h $(SYS)/macros.h $(BUILD)/cdefs.h

$(ASMOBJS): $(BUILD)/asmdefs.h

$(BUILD)/table.s: $(BUILD)/globals.cfg
	./config $(BUILD)/globals.cfg

# eof

