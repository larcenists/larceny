# Makefile for Larceny
#
# $Id: Makefile,v 1.4 92/02/10 12:24:18 lth Exp Locker: lth $

# Architecture-independent stuff
SYS=Sys

# Subdirectory for machine-dependent stuff.
MACH=Sparc

# Where the Scheme libraries live
LIB=Lib

CHDRS=	$(SYS)/main.h $(SYS)/offsets.h $(SYS)/macros.h $(SYS)/millicode.h \
	$(SYS)/layouts.h $(SYS)/exceptions.h
AHDRS=	$(MACH)/registers.s.h $(MACH)/offsets.s.h $(MACH)/millicode.s.h \
	$(MACH)/layouts.s.h $(MACH)/exceptions.s.h
OBJS=	$(SYS)/main.o \
	$(SYS)/memsupport.o \
	$(SYS)/gc.o \
	$(SYS)/cglue.o \
	$(SYS)/version.o \
	$(SYS)/localdebugger.o \
	$(MACH)/memory.o \
	$(MACH)/tables.o \
	$(MACH)/glue.o \
	$(MACH)/generic.o

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
	$(CC) $(CFLAGS) -o $(COUTPUT) $<

larceny: $(OBJS)
	$(CC) $(PROFILE) -o larceny $(OBJS)

clean:
	rm larceny $(OBJS)

libclean:
	rm $(LIB)/*.lap $(LIB)/*.lop

# Support stuff for Chez hosted system.

basicstuff: bits1 bits2
bits1: Chez/bitpattern.o
bits2: Compiler/mtime.o

# sources
$(SYS)/main.o:		$(SYS)/main.c $(CHDRS)
$(SYS)/gc.o:		$(SYS)/gc.c $(CHDRS)
$(SYS)/cglue.o:		$(SYS)/cglue.c $(CHDRS)
$(SYS)/version.o:	$(SYS)/version.c
$(SYS)/memsupport.o:	$(SYS)/memsupport.c $(CHDRS)
$(SYS)/localdebugger.o:	$(SYS)/localdebugger.c $(CHDRS)
$(MACH)/memory.o:	$(MACH)/memory.s $(AHDRS)
$(MACH)/tables.o:	$(MACH)/tables.s $(MACH)/memory.o $(MACH)/glue.o \
			$(MACH)/generic.o
$(MACH)/glue.o:		$(MACH)/glue.s $(AHDRS) $(MACH)/milliprocs.s.h
$(MACH)/generic.o:	$(MACH)/generic.s $(AHDRS) $(MACH)/milliprocs.s.h

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

# eof
