############################################################################
#                        Makefile for Larceny v0.20                        #
############################################################################

# Architecture-independent stuff
SYS=Sys

# Subdirectory for machine-dependent stuff.
MACH=Sparc

# Build directory
BUILD=Build

# Where the Scheme libraries live
LIB=Lib

# Where the Compiler lives
COMP=Compiler

# Where the documentation lives
TEXT=Text

############################################################################
#
# Compiler flags
#
# The flag DEBUG adds some debugging output.
# The flag DEBUG2 adds a _lot_ of debugging output.
#
# There are also some #define's in larceny.h (architecture and OS selection)
# which should probably be moved into the makefile. It is not currently
# a problems since we support only one arch. and OS. (duh.)

# PROFILE=-pg
#DEBUG=-g
#DFLAGS=-DDEBUG # -DDEBUG2
OPTIMIZE=-O2
CC=gcc
AS=as

CFLAGS=	-c $(DEBUG) $(OPTIMIZE) $(PROFILE) -I$(SYS) -I$(BUILD) $(DFLAGS)

ASFLAGS= -P -I$(MACH) -I$(BUILD) $(DFLAGS)


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

ACFG=$(BUILD)/globals.ah $(BUILD)/regs.ah $(BUILD)/except.ah \
	$(BUILD)/layouts.ah $(BUILD)/mprocs.ah

SCFG=$(BUILD)/globals.sh $(BUILD)/regs.sh $(BUILD)/except.sh \
	$(BUILD)/layouts.sh 


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
.cfg.s:
	./config $<
.s.o:
	$(AS) $(ASFLAGS) -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -DUSER=\"$$USER\" -DDATE="\"`date`\"" -o $*.o $<


############################################################################
#
# Targets

default:
	@echo "Make what?"

larceny: $(GSGC_OBJS)
	$(CC) $(PROFILE) -o larceny $(GSGC_OBJS)
	/bin/rm -f $(SYS)/version.o

sclarceny: $(SCGC_OBJS)
	$(CC) $(PROFILE) -o sclarceny $(SCGC_OBJS)
	/bin/rm -f $(SYS)/version.o

exlarceny: $(EXGC_OBJS)
	$(CC) $(PROFILE) -o exlarceny $(EXGC_OBJS)
	/bin/rm -f $(SYS)/version.o

clean:
	rm -f larceny sclarceny exlarceny $(GSGC_OBJS) $(SCGC_OBJS) $(EXGC_OBJS)

lopclean:
	rm -f $(LIB)/*.lop $(LIB)/Eval/*.lop

libclean:
	rm -f $(LIB)/*.lap $(LIB)/*.lop 
	rm -f $(LIB)/Eval/*.lap $(LIB)/Eval/*.lop

realclean: clean libclean
	rm -f $(BUILD)/*.ch $(BUILD)/*.ah $(BUILD)/*.sh $(BUILD)/*.h

tar:
	tar cvf larceny.tar Makefile CHGLOG BUGS WISHLIST *.cfg nbuild \
		config larceny.1 loadcompiler.sch rewrite $(SYS)/*.c \
		$(SYS)/*.h $(MACH)/*.s $(LIB)/*.sch $(LIB)/*.mal \
		$(LIB)/Eval/*.sch Chez/*.c Chez/*.ss Chez/*.h \
		$(COMP)/*.sch $(TEXT)/*.tex
	compress larceny.tar


###########################################################################
#
# Support stuff for Chez hosted system.

basicstuff: bits1 bits2
bits1: Chez/bitpattern.o
bits2: Compiler/mtime.o


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


# eof

