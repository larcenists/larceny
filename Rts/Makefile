# -*- mode: fundamental -*-
#
# Rts/Makefile
#
# $Id: Makefile,v 1.12 1997/09/17 15:15:54 lth Exp lth $

###########################################################################
#
# Section 0: Programs
#
# You need an ANSI C compiler.
#
# The assembler must accept -P; the GNU assembler does not.  If you
# experience assembler errors, you most likely have the wrong assembler.
# On SunOS systems, try /bin/as.

CC=gcc
CCFLAGS=-D__USE_FIXED_PROTOTYPES__ -Wpointer-arith -Wmissing-prototypes \
	-Wimplicit
AS=as


############################################################################
#
# Section 1: Target OS selection

# Target OS is detected automatically in Sys/config.h.  If you need to
# override the detection, then hack that file, and additionally add to
# the following variables.

EXTRALIBPATH=
EXTRALIBS=
LDXFLAGS=


###########################################################################
#
# Section 2: Feature configuration section.

# Stack underflow statistics.
#
# By setting STACK_UNDERFLOW_COUNTING to 1, the number of stack underflows 
# are recorded at a slight (probably negligible) performance penalty.

STACK_UNDERFLOW_COUNTING=1


# Instruction cache flushing.
#
# In general, Larceny detects automatically whether cache flushing needs
# to be performed on the architecture Larceny is running on. You can,
# however, override this behaviour by uncommenting one of the following
# lines and recompiling Rts/Sparc/cache.c and relinking.

#IFLUSH=-DFLUSH_ALWAYS
#IFLUSH=-DFLUSH_NEVER


# Division by zero handling.
#
# It should not be necessary to uncomment the following line.

#DIVZ=-DCHECK_DIVISION_BY_ZERO


############################################################################
#
# Compiler flags
#
# Defining DEBUG adds some debugging output.
# Defining DEBUG2 may or may not add a _lot_ of debugging output (old switch)

CONFIG=-DSTACK_UNDERFLOW_COUNTING=$(STACK_UNDERFLOW_COUNTING)

LIBS=-lm -ldl

DEBUGINFO=-g
DEBUGFLAGS=#-DDEBUG # -DGCLIB_DEBUG -DDEBUG2 
PROFILE=#-p
TCOV=#-a -g -DNDEBUG
LDYFLAGS=#Util/ffi-dummy.o  # For compiling with -p (avoids linker trouble)
OPTIMIZE=-DNDEBUG -O3
ARCHINFO=-DENDIAN_BIG -DBITS_32

# You typically want this to remain on, unless you want multiple collectors
# in the same process (beyond the realm of the possible, at the moment).  
# See Sys/bdw-collector.c for details, and grovel over the source tree :-)

BDW_DEBUG=-DBDW_DEBUG #-DBDW_CLEAR_STACK

CFLAGS=	-c $(DEBUGINFO) $(OPTIMIZE) $(PROFILE) $(TCOV) $(CONFIG) \
	-ISys -IBuild -ICPetit $(DEBUGFLAGS) $(IFLUSH) $(DIVZ) \
	$(ARCHINFO) $(BDW_DEBUG) $(CCFLAGS)

SPARC_ASFLAGS= -P -ISparc -IBuild $(CONFIG) $(DEBUGFLAGS) \
	$(IFLUSH) $(DIVZ) $(ASXFLAGS)


############################################################################
#
# Big bags of files

COMMON_RTS_OBJECTS=\
	Sys/argv.o Sys/barrier.o Sys/callback.o Sys/ffi.o Sys/gc_t.o \
	Sys/heapio.o Sys/ldebug.o Sys/malloc.o Sys/signals.o Sys/sro.o \
	Sys/stack.o Sys/syscall.o Sys/unix.o Sys/util.o Sys/version.o

PRECISE_GC_OBJECTS=\
	Sys/alloc.o Sys/cheney.o Sys/gc.o Sys/los.o \
	Sys/memmgr.o Sys/np-sc-heap.o Sys/nursery.o Sys/old_heap_t.o \
	Sys/old-heap.o Sys/remset.o Sys/sc-heap.o Sys/semispace.o \
	Sys/static-heap.o Sys/stats.o Sys/young_heap_t.o

BOEHM_GC_OBJECTS=\
	Sys/bdw-gc.o Sys/bdw-stats.o Sys/bdw-collector.o

BOEHM_GC_SPARC_OBJECTS=\
	Sparc/bdw-memory.o SPARC/bdw-generic.o

BOEHM_GC_LIBRARIES=\
	bdw-gc/gc.a

COMMON_SPARC_OBJECTS=\
	Sparc/barrier.o Sparc/cache.o Sparc/cache0.o Sparc/cglue.o \
	Sparc/glue.o Sparc/mcode.o Build/table.o

SPARC_PRECISE_GC_OBJECTS=\
	Sparc/memory.o Sparc/generic.o

PETIT_OBJECTS=\
	CPetit/arithmetic.o CPetit/millicode.o CPetit/multiply.o

LARCENY_OBJECTS=\
	Sys/larceny.o \
	$(COMMON_RTS_OBJECTS) \
	$(COMMON_SPARC_OBJECTS) \
	$(PRECISE_GC_OBJECTS) \
	$(SPARC_PRECISE_GC_OBJECTS)

BDW_LARCENY_OBJECTS=\
	Sys/bdw-larceny.o \
	$(COMMON_RTS_OBJECTS) \
	$(COMMON_SPARC_OBJECTS) \
	$(BOEHM_GC_OBJECTS) \
	$(BOEHM_GC_SPARC_OBJECTS)

PETIT_LARCENY_OBJECTS=\
	Sys/petit-larceny.o \
	$(COMMON_RTS_OBJECTS) \
	$(PRECISE_GC_OBJECTS) \
	$(PETIT_OBJECTS)


# Generated header files.
# ACFG, CCFG, SCFG, and HDRFILES also exists in ../Makefile.  Watch it!

CCFG=Build/globals.ch Build/except.ch Build/layouts.ch

ACFG=Build/globals.ah Build/regs.ah Build/except.ah Build/layouts.ah \
	Build/mprocs.ah

SCFG=Build/globals.sh Build/regs.sh Build/except.sh Build/layouts.sh 


############################################################################
#
# Rules

.SUFFIXES:	.cfg .ch .ah .sh

.cfg.ch:
	./config "UNKNOWN" $<

.cfg.ah:
	./config "UNKNOWN" $<

.cfg.sh:
	./config "UNKNOWN" $<

# Yow!
.s.o:
	/bin/sh -c "$(AS) $(SPARC_ASFLAGS) \`defs\` -o $*.o $<"

# Really want %Y but SunOS 4 doesn't do that.
.c.o:
	$(CC) $(CFLAGS) -DUSER=\"$$LOGNAME\" -DDATE="\"`date '+%d-%h-%y %T'`\"" -o $*.o $<

############################################################################
#
# Targets

default:
	@echo "Please give me a target."

setup:
	if [ ! -d Build ]; then mkdir Build; fi
	-(cd Build; for i in ../*.cfg; do ln -s $$i ; done)

larceny: $(LARCENY_OBJECTS) Util/ffi-dummy.o
	$(CC) $(PROFILE) $(TCOV) -o larceny $(LARCENY_OBJECTS) \
		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) $(LDXFLAGS) $(LDYFLAGS)
	/bin/rm -f Sys/version.o

bdwlarceny: $(BDW_LARCENY_OBJECTS) Util/ffi-dummy.o
	( cd bdw-gc ; make gc.a )
	$(CC) $(PROFILE) $(TCOV) -o bdwlarceny $(BDW_LARCENY_OBJECTS) \
		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) \
		$(BOEHM_GC_LIBRARIES) $(LDXFLAGS) $(LDYFLAGS)
	/bin/rm -f Sys/version.o

petit: $(PETIT_LARCENY_OBJECTS)
	$(CC) $(PROFILE) $(TCOV) -o petit-larceny $(PETIT_LARCENY_OBJECTS) \
		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) $(LDXFLAGS)
	/bin/rm -f Sys/version.o

hsplit: Util/hsplit.o
	$(CC) $(PROFILE) $(TCOV) $(DEBUGINFO) $(LDXFLAGS) -o hsplit Util/hsplit.o

Util/hsplit.o: Util/hsplit.c
	$(CC) -g -o Util/hsplit.o -IBuild -ISys -c Util/hsplit.c

clean:
	rm -f larceny hsplit bdwlarceny petit-larceny core defs \
		Sys/*.o Sparc/*.o Build/*.o CPetit/*.o Util/ffi-dummy.o

rtsclean: clean
	rm -f Build/*.s Build/*.*h

realclean: clean
	if [ -d bdw-gc ]; then ( cd bdw-gc ; make clean ); fi
	rm -f Build/*

defs: Util/defs.c
	$(CC) -o defs Util/defs.c

###########################################################################
#
# Magic header files

Build/cdefs.h:	$(CCFG)
	cat $(CCFG) > Build/cdefs.h

Build/asmdefs.h:	$(ACFG)
	cat $(ACFG) > Build/asmdefs.h

Build/schdefs.h:	$(SCFG)
	cat $(SCFG) > Build/schdefs.h

Build/table.s: Build/globals.cfg
	./config "UNKNOWN" Build/globals.cfg



###########################################################################
#
# Dependencies for source files beyond this point.

LARCENY_H=Sys/larceny.h Sys/larceny-types.h Sys/macros.h Sys/assert.h \
	  Build/cdefs.h Sys/config.h
SPARC_ASM_H=Build/asmdefs.h Sparc/asmmacro.h defs

CPetit/arithmetic.o: $(LARCENY_H) CPetit/millicode.h
CPetit/millicode.o: $(LARCENY_H) CPetit/millicode.h Sys/gc_t.h
CPetit/multiply.o: $(LARCENY_H) CPetit/millicode.h

Sparc/barrier.o: $(SPARC_ASM_H)
Sparc/bdw-memory.o: Sparc/memory.s $(SPARC_ASM_H)
Sparc/cache.o: $(LARCENY_H)
Sparc/cache0.o: $(SPARC_ASM_H)
Sparc/cglue.o: $(LARCENY_H) Sys/signals.h
Sparc/generic.o: $(SPARC_ASM_H)
Sparc/glue.o: $(SPARC_ASM_H)
Sparc/mcode.o: $(SPARC_ASM_H)
Sparc/memory.o: $(SPARC_ASM_H)
Sparc/table.o: $(SPARC_ASM_H)

Sys/alloc.o: Sys/posix-alloc.c $(LARCENY_H) Sys/barrier.h Sys/gclib.h \
	Sys/semispace_t.h
Sys/argv.o: $(LARCENY_H)
Sys/barrier.o: $(LARCENY_H) Sys/memmgr.h Sys/barrier.h
Sys/bdw-collector.o: $(LARCENY_H) Sys/barrier.h Sys/gc.h Sys/gc_t.h \
	Sys/gclib.h Sys/heap_stats_t.h Sys/memmgr.h Sys/stack.h \
	bdw-gc/include/gc.h
Sys/bdw-gc.o: Sys/gc.c $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/heapio.h \
	Sys/static_heap_t.h
Sys/bdw-larceny.o: Sys/larceny.c $(LARCENY_H) Sys/gc.h
Sys/bdw-stats.o: Sys/stats.c $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/heap_stats_t.h Sys/memmgr.h
Sys/callback.o: $(LARCENY_H)
Sys/cheney.o: $(LARCENY_H) Sys/barrier.h Sys/gc_t.h Sys/gclib.h \
	Sys/los_t.h Sys/memmgr.h Sys/semispace_t.h Sys/static_heap_t.h
Sys/ffi.o: $(LARCENY_H)
Sys/gc.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/heapio.h Sys/semispace_t.h \
	Sys/static_heap_t.h
Sys/gc_t.o: $(LARCENY_H) Sys/gc_t.h
Sys/heapio.o: $(LARCENY_H) Sys/heapio.h Sys/semispace_t.h Sys/gclib.h
Sys/larceny.o: $(LARCENY_H) Sys/gc.h
Sys/ldebug.o: $(LARCENY_H) Sys/macros.h
Sys/los.o: $(LARCENY_H) Sys/gclib.h Sys/los_t.h
Sys/malloc.o: $(LARCENY_H)
Sys/memmgr.o: $(LARCENY_H) Sys/barrier.h Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/heap_stats_t.h Sys/heapio.h Sys/los_t.h Sys/memmgr.h \
	Sys/old_heap_t.h Sys/remset_t.h Sys/static_heap_t.h Sys/young_heap_t.h 
Sys/np-sc-heap.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/heap_stats_t.h Sys/los_t.h Sys/memmgr.h Sys/old_heap_t.h \
	Sys/remset_t.h Sys/semispace_t.h Sys/young_heap_t.h
Sys/nursery.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/heap_stats_t.h Sys/los_t.h Sys/memmgr.h Sys/stack.h \
	Sys/young_heap_t.h
Sys/old_heap_t.o: $(LARCENY_H) Sys/old_heap_t.h
Sys/old-heap.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/heap_stats_t.h Sys/los_t.h Sys/memmgr.h Sys/old_heap_t.h \
	Sys/remset_t.h Sys/semispace_t.h Sys/static_heap_t.h Sys/young_heap_t.h
Sys/remset.o: $(LARCENY_H) Sys/gclib.h Sys/memmgr.h Sys/remset_t.h
Sys/sc-heap.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/heap_stats_t.h Sys/los_t.h Sys/memmgr.h Sys/semispace_t.h \
	Sys/stack.h Sys/static_heap_t.h Sys/young_heap_t.h
Sys/semispace.o: $(LARCENY_H) Sys/gclib.h Sys/semispace_t.h
Sys/signals.o: $(LARCENY_H) Sys/signals.h
Sys/sro.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h Sys/heapio.h \
	Sys/memmgr.h
Sys/stack.o: $(LARCENY_H) Sys/stack.h
Sys/static-heap.o: $(LARCENY_H) Sys/gc.h Sys/gclib.h Sys/heap_stats_t.h \
	Sys/memmgr.h Sys/semispace_t.h Sys/static_heap_t.h
Sys/stats.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/heap_stats_t.h Sys/memmgr.h
Sys/syscall.o: $(LARCENY_H) Sys/signals.h
Sys/unix.o: $(LARCENY_H)
Sys/util.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h
Sys/version.o: Sys/config.h
Sys/young_heap_t.o: $(LARCENY_H) Sys/young_heap_t.h

# eof

