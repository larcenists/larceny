# Makefile for Larceny
#
# $Id: Makefile,v 1.1 91/09/13 03:00:12 lth Exp Locker: lth $

# Subdirectory for machine-dependent stuff.
MACH=Sparc
# Place for most of the header files
INCLUDE=/owyhee2/users/lth/scheme313

CHDRS=main.h offsets.h macros.h millicode.h layouts.h
AHDRS=$(MACH)/registers.s.h offsets.h millicode.h $(MACH)/layouts.s.h
OBJS=main.o memsupport.o gc.o cglue.o version.o localdebugger.o \
	$(MACH)/memory.o \
	$(MACH)/exception.o \
	$(MACH)/schemestart.o \
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

CFLAGS=	$(COMPILE) $(PREPROCESS) $(OPTIMIZE) $(PROFILE) $(DEBUG) -I$(INCLUDE)\
	$(DFLAG) -DUSER=\"$$USER\"

.s.o:
	as -P $(DFLAG) -DASSEMBLY -I$(INCLUDE) -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -o $(COUTPUT) $<

larceny: $(OBJS)
	$(CC) $(PROFILE) -o larceny $(OBJS)

main.o:			main.c $(CHDRS)
gc.o:			gc.c $(CHDRS)
cglue.o:		cglue.c $(CHDRS)
version.o:		version.c
memsupport.o:		memsupport.c $(CHDRS)
localdebugger.o:	localdebugger.c $(CHDRS)
$(MACH)/exception.o:	$(MACH)/exception.s $(AHDRS)
$(MACH)/memory.o:	$(MACH)/memory.s $(AHDRS)
$(MACH)/schemestart.o:	$(MACH)/schemestart.s $(AHDRS)
$(MACH)/tables.o:	$(MACH)/tables.s $(MACH)/memory.o
$(MACH)/glue.o:		$(MACH)/glue.s $(AHDRS)
$(MACH)/generic.o:	$(MACH)/generic.s $(AHDRS)
