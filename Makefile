# Makefile for Larceny
#
# $Id: Makefile,v 1.8 1992/05/15 22:17:22 lth Exp lth $

# Architecture-independent stuff
SYS=Sys

# Subdirectory for machine-dependent stuff.
MACH=Sparc

# Where the Scheme libraries live
LIB=Lib

# Where thesis variations live
THESIS=Thesis

CHDRS=	$(SYS)/larceny.h $(SYS)/offsets.h $(SYS)/macros.h $(SYS)/millicode.h \
	$(SYS)/layouts.h $(SYS)/exceptions.h
AHDRS=	$(MACH)/registers.s.h $(MACH)/offsets.s.h $(MACH)/millicode.s.h \
	$(MACH)/layouts.s.h $(MACH)/exceptions.s.h
OBJS=	$(SYS)/main.o \
	$(SYS)/memsupport.o \
	$(SYS)/cglue.o \
	$(SYS)/localdebugger.o \
	$(SYS)/version.o \
	$(MACH)/memory.o \
	$(MACH)/tables.o \
	$(MACH)/glue.o \
	$(MACH)/generic.o

# Three garbage collectors: generation-scavenging, mostly-generational,
# and stop-and-copy.

GSGC=	$(SYS)/gc
MGGC=	$(THESIS)/mg-gc
SCGC=	$(THESIS)/sc-gc

# PROFILE=-pg
DEBUG=-g
DFLAG=-DDEBUG
CC=cc
# OPTIMIZE=-O4

COMPILE=-c
COUTPUT=$*.o

# COMPILE=-E
# COUTPUT=$*.i


CFLAGS=	$(COMPILE) $(PREPROCESS) $(OPTIMIZE) $(PROFILE) $(DEBUG) -I$(SYS)\
	$(DFLAG)

.s.o:
	as -P $(DFLAG) -DASSEMBLY -I$(MACH) -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -DUSER=\"$$USER\" -DDATE="\"`date`\"" -o $(COUTPUT) $<

larceny: $(OBJS) $(GSGC).o
	$(CC) $(PROFILE) -o larceny $(OBJS) $(GSGC).o
	/bin/rm -f $(SYS)/version.o

mg-larceny: $(OBJS) $(MGGC).o
	$(CC) $(PROFILE) -o mg-larceny $(OBJS) $(MGGC).o
	/bin/rm -f $(SYS)/version.o

sc-larceny: $(OBJS) $(SCGC).o
	$(CC) $(PROFILE) -o sc-larceny $(OBJS) $(SCGC).o
	/bin/rm -f $(SYS)/version.o

clean:
	rm -f larceny $(OBJS)

libclean:
	rm -f $(LIB)/*.lap $(LIB)/*.lop $(LIB)/Sparc/*.lap $(LIB)/Sparc/*.lop
	rm -f Eval/*.lap Eval/*.lop

# Support stuff for Chez hosted system.

basicstuff: bits1 bits2
bits1: Chez/bitpattern.o
bits2: Compiler/mtime.o

# sources
$(SYS)/main.o:		$(SYS)/main.c $(CHDRS)
$(SYS)/gc.o:		$(SYS)/gc.c $(CHDRS) $(SYS)/gc.h
$(SYS)/cglue.o:		$(SYS)/cglue.c $(CHDRS)
$(SYS)/memsupport.o:	$(SYS)/memsupport.c $(CHDRS) $(SYS)/memstats.h
$(SYS)/localdebugger.o:	$(SYS)/localdebugger.c $(CHDRS)
$(SYS)/version.o:	$(SYS)/version.c

$(MACH)/memory.o:	$(MACH)/memory.s $(AHDRS)
$(MACH)/tables.o:	$(MACH)/tables.s $(MACH)/memory.o $(MACH)/glue.o \
			$(MACH)/generic.o
$(MACH)/glue.o:		$(MACH)/glue.s $(AHDRS) $(MACH)/milliprocs.s.h
$(MACH)/generic.o:	$(MACH)/generic.s $(AHDRS) $(MACH)/milliprocs.s.h
$(THESIS)/mg-gc.o:	$(THESIS)/mg-gc.c $(CHDRS) $(SYS)/gc.h
$(THESIS)/sc-gc.o:	$(THESIS)/sc-gc.c $(CHDRS) $(SYS)/gc.h

# headers to build from config files.

$(MACH)/milliprocs.s.h:	milliprocs.cfg
	./config milliprocs.cfg
$(MACH)/millicode.s.h + $(MACH)/millicode.sch.h + $(SYS)/millicode.h: millicode.cfg
	./config millicode.cfg
$(MACH)/offsets.s.h + $(MACH)/offsets.sch.h + $(SYS)/offsets.h: offsets.cfg
	./config offsets.cfg
$(MACH)/layouts.s.h + $(MACH)/layouts.sch.h + $(SYS)/layouts.h: layouts.cfg
	./config layouts.cfg
$(MACH)/registers.s.h + $(MACH)/registers.sch.h: registers.cfg
	./config registers.cfg
$(MACH)/exceptions.s.h + $(SYS)/exceptions.h: exceptions.cfg
	./config exceptions.cfg
$(SYS)/memstats.h: memstats.cfg
	./config memstats.cfg

# eof
