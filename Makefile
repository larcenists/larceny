# Makefile for Larceny, with (extensive) support for thesis stuff.
# Needs cleaning up when thesis work is over.
# Needs cleaning up in general.
#
# $Id: Makefile,v 1.11 1992/06/13 08:08:20 lth Exp lth $

# Architecture-independent stuff
SYS=Sys

# Subdirectory for machine-dependent stuff.
MACH=Sparc

# Where the Scheme libraries live
LIB=Lib
TLIB=Thesis/Lib-mg+sc

# Where thesis variations live
THESIS=Thesis

# Where the experimental executables live
EXEC=/home/systems/lth/Thesis/Exec

CHDRS=	$(SYS)/larceny.h $(SYS)/offsets.h $(SYS)/macros.h $(SYS)/millicode.h \
	$(SYS)/layouts.h $(SYS)/exceptions.h
AHDRS=	$(MACH)/registers.s.h $(MACH)/offsets.s.h $(MACH)/millicode.s.h \
	$(MACH)/layouts.s.h $(MACH)/exceptions.s.h
OBJS=	$(SYS)/main.o \
	$(SYS)/memsupport.o \
	$(SYS)/cglue.o \
	$(SYS)/localdebugger.o \
	$(SYS)/version.o \
	$(MACH)/tables.o \
	$(MACH)/glue.o \
	$(MACH)/generic.o

# Five garbage collectors: generation-scavenging, mostly-generational,
# stop-and-copy, reference-remembered-set, card-marking.

GSGC=	$(SYS)/gc
GSMEM=	$(MACH)/memory
MGGC=	$(THESIS)/Sys/mg-gc
MGMEM=	$(THESIS)/Sys/mg-memory
SCGC=	$(THESIS)/Sys/sc-gc
SCMEM=	$(THESIS)/Sys/sc-memory
RRGC=   $(THESIS)/Sys/rr-gc
RRMEM=	$(THESIS)/Sys/rr-memory
CMGC=   $(THESIS)/Sys/cm-gc
CMMEM=	$(THESIS)/Sys/cm-memory

# PROFILE=-pg
# DEBUG=-g
# DFLAG=-DDEBUG
CC=gcc
OPTIMIZE=-O2 -funroll-loops # -DGNUC

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

# This is the default binary

larceny: $(OBJS) $(GSGC).o $(GSMEM).o
	$(CC) $(PROFILE) -o larceny $(OBJS) $(GSGC).o $(GSMEM).o
	/bin/rm -f $(SYS)/version.o

# The following five executables are for the Thesis work.

gs-larceny: larceny
	/bin/cp larceny $(EXEC)/gs-larceny

mg-larceny: $(OBJS) $(MGGC).o $(MGMEM).o
	$(CC) $(PROFILE) -o $(EXEC)/mg-larceny $(OBJS) $(MGGC).o $(MGMEM).o
	/bin/rm -f $(SYS)/version.o

sc-larceny: $(OBJS) $(SCGC).o $(SCMEM).o
	$(CC) $(PROFILE) -o $(EXEC)/sc-larceny $(OBJS) $(SCGC).o $(SCMEM).o
	/bin/rm -f $(SYS)/version.o

rr-larceny: $(OBJS) $(RRGC).o $(RRMEM).o
	$(CC) $(PROFILE) -o $(EXEC)/rr-larceny $(OBJS) $(RRGC).o $(RRMEM).o
	/bin/rm -f $(SYS)/version.o
	
cm-larceny: $(OBJS) $(CMGC).o $(CMMEM).o
	$(CC) $(PROFILE) -o $(EXEC)/cm-larceny $(OBJS) $(CMGC).o $(CMMEM).o
	/bin/rm -f $(SYS)/version.o

# Housekeeping stuff

clean:
	rm -f larceny $(OBJS)

libclean:
	rm -f $(LIB)/*.lap $(LIB)/*.lop $(LIB)/Sparc/*.lap $(LIB)/Sparc/*.lop
	rm -f $(LIB)/Eval/*.lap $(LIB)/Eval/*.lop

tlibclean:
	rm -f $(TLIB)/*.lap $(TLIB)/*.lop $(TLIB)/Sparc/*.lap \
		$(TLIB)/Sparc/*.lop
	rm -f $(TLIB)/Eval/*.lap $(TLIB)/Eval/*.lop

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

# $(MACH)/memory.o:	$(MACH)/memory.s $(AHDRS)
$(MACH)/tables.o:	$(MACH)/tables.s
$(MACH)/glue.o:		$(MACH)/glue.s $(AHDRS) $(MACH)/milliprocs.s.h
$(MACH)/generic.o:	$(MACH)/generic.s $(AHDRS) $(MACH)/milliprocs.s.h

$(MGGC).o:		$(MGGC).c $(CHDRS) $(SYS)/gc.h
$(SCGC).o:		$(SCGC).c $(CHDRS) $(SYS)/gc.h
$(RRGC).o:		$(RRGC).c $(CHDRS) $(SYS)/gc.h
$(CMGC).o:		$(CMGC).c $(CHDRS) $(SYS)/gc.h

$(GSMEM).o:		$(MACH)/memory.s $(AHDRS)
	as -P $(DFLAG) -DASSEMBLY -DREGULAR -I$(MACH) -o $(GSMEM).o $(MACH)/memory.s
$(MGMEM).o:		$(MACH)/memory.s $(AHDRS)
	as -P $(DFLAG) -DASSEMBLY -DNONE -I$(MACH) -o $(MGMEM).o $(MACH)/memory.s
$(SCMEM).o:		$(MACH)/memory.s $(AHDRS)
	as -P $(DFLAG) -DASSEMBLY -DNONE -I$(MACH) -o $(SCMEM).o $(MACH)/memory.s
$(RRMEM).o:		$(MACH)/memory.s $(AHDRS)
	as -P $(DFLAG) -DASSEMBLY -DREFERENCE -I$(MACH) -o $(RRMEM).o $(MACH)/memory.s
$(CMMEM).o:		$(MACH)/memory.s $(AHDRS)
	as -P $(DFLAG) -DASSEMBLY -DCARDMARKING -I$(MACH) -o $(CMMEM).o $(MACH)/memory.s

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
