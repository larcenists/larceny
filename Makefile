# Makefile for Larceny
#
# $Id: Makefile,v 1.9 1992/05/18 05:10:31 lth Exp lth $

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

# Five garbage collectors: generation-scavenging, mostly-generational,
# stop-and-copy, reference-remembered-set, card-marking.

GSGC=	$(SYS)/gc
MGGC=	$(THESIS)/Sys/mg-gc
SCGC=	$(THESIS)/Sys/sc-gc
RRGC=   $(THESIS)/Sys/rr-gc
CMGC=   $(THESIS)/Sys/cm-gc

# PROFILE=-pg
# DEBUG=-g
# DFLAG=-DDEBUG
CC=cc
OPTIMIZE=-O4

COMPILE=-c
COUTPUT=$*.o

# COMPILE=-E
# COUTPUT=$*.i


CFLAGS=	$(COMPILE) $(PREPROCESS) $(OPTIMIZE) $(PROFILE) $(DEBUG) -I$(SYS)\
	$(DFLAG)

.s.o:
	as -P $(DFLAG) -DASSEMBLY -D$(TRANS) -I$(MACH) -o $*.o $<
.c.o:
	$(CC) $(CFLAGS) -DUSER=\"$$USER\" -DDATE="\"`date`\"" -o $(COUTPUT) $<

larceny: $(OBJS) $(GSGC).o
	rm -f $(MACH)/memory.o
	make TRANS=REGULAR $(MACH)/memory.o
	$(CC) $(PROFILE) -o larceny $(OBJS) $(GSGC).o
	/bin/rm -f $(SYS)/version.o

mg-larceny: $(OBJS) $(MGGC).o
	rm -f $(MACH)/memory.o
	make TRANS=NONE $(MACH)/memory.o
	$(CC) $(PROFILE) -o Execs/mg-larceny $(OBJS) $(MGGC).o
	/bin/rm -f $(SYS)/version.o

sc-larceny: $(OBJS) $(SCGC).o
	rm -f $(MACH)/memory.o
	make TRANS=NONE $(MACH)/memory.o
	$(CC) $(PROFILE) -o Execs/sc-larceny $(OBJS) $(SCGC).o
	/bin/rm -f $(SYS)/version.o

rr-larceny: $(OBJS) $(RRGC).o
	rm -f $(MACH)/memory.o
	make TRANS=REFERENCE $(MACH)/memory.o
	$(CC) $(PROFILE) -o Execs/rr-larceny $(OBJS) $(RRGC).o
	/bin/rm -f $(SYS)/version.o
	
cm-larceny: $(OBJS) $(CMGC).o
	rm -f $(MACH)/memory.o
	make TRANS=CARDMARKING $(MACH)/memory.o
	$(CC) $(PROFILE) -o Execs/cm-larceny $(OBJS) $(CMGC).o
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

$(MGGC).o:		$(MGGC).c $(CHDRS) $(SYS)/gc.h
$(SCGC).o:		$(SCGC).c $(CHDRS) $(SYS)/gc.h
$(RRGC).o:		$(RRGC).c $(CHDRS) $(SYS)/gc.h
$(CMGC).o:		$(CMGC).c $(CHDRS) $(SYS)/gc.h

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
