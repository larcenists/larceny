# Copyright 1998 Lars T Hansen       -*- mode: fundamental -*-
#
# $Id$
#
# This is Rts/Makefile.

###########################################################################
#
# Section 0: Programs
#
# You need an ANSI C compiler.
#
# The assembler must accept -P; the GNU assembler does not.  If you
# experience assembler errors, you most likely have the wrong assembler.
# On SunOS systems, try /bin/as.  gcc -E might work.

# Solaris (native): GNU C compiler

CC=gcc
CCXFLAGS=-D__USE_FIXED_PROTOTYPES__ -Wpointer-arith -Wmissing-prototypes \
	-Wimplicit -Wreturn-type -Wunused -Wuninitialized

# Solaris (native): Solaris assembler

AS=as
ASXFLAGS=

# DEC Alpha OSF/1 (Petit Larceny): DEC C compiler
#
# CC=cc
# CCXFLAGS=-g3 -taso -xtaso_short -ieee
# LD_SHARED=ld -shared -taso -g3 -soname libpetit.so -o libpetit.so \
#	-lm -lc -all
## Needed because make leaves the -o off.
# Why is this here?  See other .c.o rule below.
#.c.o:
#	$(CC) $(CFLAGS) -c $< -o $*.o

#CC=lcc
#CCXFLAGS=-DSTDC_SOURCE


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

# Do all your configuration selection by editing Sys/config.h.


############################################################################
#
# Compiler flags

LIBS=-lm -ldl

DEBUGINFO=-g3
PROFILE=#-pg
TCOV=#-a -g
LDYFLAGS=#Util/ffi-dummy.o  # For compiling with -p (avoids linker trouble)
OPTIMIZE=-O3 -DNDEBUG2 # -DNDEBUG

# You typically want this to remain on, unless you want multiple collectors
# in the same process (beyond the realm of the possible, at the moment).  
# See Sys/bdw-collector.c for details, and grovel over the source tree :-)

BDW_DEBUG=-DBDW_DEBUG #-DBDW_CLEAR_STACK

CFLAGS=-c -ISys -IBuild -IStandard-C \
	$(DEBUGINFO) $(OPTIMIZE) $(PROFILE) $(TCOV) $(BDW_DEBUG) $(CCXFLAGS)

ASFLAGS=-P -ISparc -IBuild $(DEBUGFLAGS) $(ASXFLAGS)

############################################################################
#
# Big bags of files

COMMON_RTS_OBJECTS=\
	Sys/argv.o Sys/barrier.o Sys/callback.o Sys/ffi.o Sys/gc_t.o \
	Sys/ldebug.o Sys/malloc.o Sys/osdep-generic.o \
	Sys/osdep-macos.o Sys/osdep-unix.o Sys/osdep-win32.o \
	Sys/primitive.o Sys/signals.o Sys/sro.o Sys/stack.o Sys/syscall.o \
	Sys/util.o Sys/version.o

PRECISE_GC_OBJECTS=\
	Sys/alloc.o Sys/cheney.o Sys/dof-heap.o Sys/gc.o Sys/heapio.o \
	Sys/los.o Sys/memmgr.o \
	Sys/msgc-core.o Sys/np-sc-heap.o Sys/nursery.o Sys/old_heap_t.o \
	Sys/old-heap.o Sys/remset.o Sys/sc-heap.o Sys/semispace.o \
	Sys/static-heap.o Sys/stats.o Sys/young_heap_t.o

BOEHM_GC_OBJECTS=\
	Sys/bdw-gc.o Sys/bdw-stats.o Sys/bdw-collector.o Sys/bdw-heapio.o 

BOEHM_GC_SPARC_OBJECTS=\
	Sparc/bdw-memory.o Sparc/bdw-generic.o Sparc/bdw-cglue.o

BOEHM_GC_LIBRARIES=\
	bdw-gc/gc.a

COMMON_SPARC_OBJECTS=\
	Sparc/barrier.o Sparc/cache.o Sparc/cache0.o \
	Sparc/glue.o Sparc/mcode.o Sparc/signals.o Sparc/syscall2.o \
	Build/sparc-table.o 

SPARC_PRECISE_GC_OBJECTS=\
	Sparc/memory.o Sparc/generic.o Sparc/cglue.o

PETIT_OBJECTS=\
	Standard-C/arithmetic.o Standard-C/millicode.o Standard-C/multiply.o \
	Standard-C/syscall2.o Build/c-table.o

# SPARC only
LARCENY_OBJECTS=\
	Sys/larceny.o \
	Sparc/config.o \
	$(COMMON_RTS_OBJECTS) \
	$(COMMON_SPARC_OBJECTS) \
	$(PRECISE_GC_OBJECTS) \
	$(SPARC_PRECISE_GC_OBJECTS)

# SPARC only
BDW_LARCENY_OBJECTS=\
	Sys/bdw-larceny.o \
	Sparc/config.o \
	$(COMMON_RTS_OBJECTS) \
	$(COMMON_SPARC_OBJECTS) \
	$(BOEHM_GC_OBJECTS) \
	$(BOEHM_GC_SPARC_OBJECTS)

# Generic Unix
PETIT_LARCENY_OBJECTS=\
	Sys/larceny.o \
	Standard-C/config.o \
	$(COMMON_RTS_OBJECTS) \
	$(PRECISE_GC_OBJECTS) \
	$(PETIT_OBJECTS)


# Generated header files.
# ACFG, CCFG, SCFG, and HDRFILES also exists in ../Makefile.  Watch it!

CCFG=Build/globals.ch Build/except.ch Build/layouts.ch Build/mprocs.ch

ACFG=Build/globals.ah Build/regs.ah Build/except.ah Build/layouts.ah \
	Build/mprocs.ah

SCFG=Build/globals.sh Build/regs.sh Build/except.sh Build/layouts.sh 


############################################################################
#
# Rules

.SUFFIXES:	.cfg .ch .ah .sh .mac

.cfg.ch:
	../build -config $<

.cfg.ah:
	../build -config $<

.cfg.sh:
	../build -config $<

.mac.c:
	../build -expand $<

.s.o:
	$(AS) $(ASFLAGS) -o $*.o $<

# Not everyone accepts %Y -- notably SunOS 4.  But that is _so_ last century...
.c.o:
	$(CC) $(CFLAGS) -DUSER=\"$$LOGNAME\" -DDATE="\"`date '+%Y-%m-%d %T'`\"" -o $*.o $<

############################################################################
#
# Targets

default:
	@echo "Please give me a target."

setup:
	if [ ! -d Build ]; then mkdir Build; fi
	-(cd Build; for i in ../*.cfg; do ln -s $$i ; done)

# The 'config' target is used before shipping so that a working Scheme
# system is not required to build larceny.bin.
config:
	$(MAKE) Build/cdefs.h
	$(MAKE) Build/asmdefs.h
	$(MAKE) Build/schdefs.h
	$(MAKE) Build/sparc-table.s
	$(MAKE) Build/c-table.c
	$(MAKE) Standard-C/arithmetic.c

larceny.bin: $(LARCENY_OBJECTS) Util/ffi-dummy.o
	$(CC) $(PROFILE) $(TCOV) -o larceny.bin $(LARCENY_OBJECTS) \
		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) $(LDXFLAGS) $(LDYFLAGS)
	/bin/rm -f Sys/version.o

bdwlarceny.bin: $(BDW_LARCENY_OBJECTS) Util/ffi-dummy.o
	( cd bdw-gc ; make gc.a )
	$(CC) $(PROFILE) $(TCOV) -o bdwlarceny.bin $(BDW_LARCENY_OBJECTS) \
		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) \
		$(BOEHM_GC_LIBRARIES) $(LDXFLAGS) $(LDYFLAGS)
	/bin/rm -f Sys/version.o

# SunOS 5.6, for sure.
libpetit.so: $(PETIT_LARCENY_OBJECTS)
	ld -G -o libpetit.so -L/usr/lib -lc $(PETIT_LARCENY_OBJECTS) \
		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) $(LDXFLAGS)
	/bin/rm -f Sys/version.o

hsplit: Util/hsplit.o
	$(CC) $(PROFILE) $(TCOV) $(DEBUGINFO) $(LDXFLAGS) -o hsplit Util/hsplit.o

Util/hsplit.o: Util/hsplit.c
	$(CC) -g -o Util/hsplit.o -IBuild -ISys -c Util/hsplit.c

clean:
	rm -f larceny.bin hsplit bdwlarceny.bin petit-larceny core \
		Sys/*.o Sparc/*.o Build/*.o Standard-C/*.o Util/*.o \
		libpetit.so Standard-C/arithmetic.c

rtsclean: clean
	rm -f Build/*.s Build/*.*h

realclean: clean
	if [ -d bdw-gc ]; then ( cd bdw-gc ; make clean ); fi
	rm -rf Build

###########################################################################
#
# Magic header files

Build/cdefs.h:	$(CCFG)
	cat $(CCFG) > Build/cdefs.h

Build/asmdefs.h:	$(ACFG)
	cat $(ACFG) > Build/asmdefs.h

Build/schdefs.h:	$(SCFG)
	cat $(SCFG) > Build/schdefs.h

Build/sparc-table.s: Build/globals.cfg
	../build -config Build/globals.cfg

Build/c-table.c: Build/globals.cfg
	../build -config Build/globals.cfg



###########################################################################
#
# Dependencies for source files beyond this point.

LARCENY_H=Sys/larceny.h Sys/larceny-types.h Sys/macros.h Sys/assert.h \
	  Build/cdefs.h Sys/config.h
SPARC_ASM_H=Build/asmdefs.h Sparc/asmmacro.h
PETIT_H=Standard-C/millicode.h Standard-C/petit-config.h \
	Standard-C/petit-hacks.h

Standard-C/arithmetic.o: $(LARCENY_H) $(PETIT_H)
Standard-C/millicode.o: $(LARCENY_H) $(PETIT_H) Sys/gc_t.h Sys/barrier.h \
	Sys/stack.h
Standard-C/multiply.o: $(LARCENY_H) $(PETIT_H)
Standard-C/syscall2.o: $(LARCENY_H) $(PETIT_H)

Sparc/barrier.o: $(SPARC_ASM_H)
Sparc/bdw-memory.o: Sparc/memory.s $(SPARC_ASM_H)
Sparc/cache.o: $(LARCENY_H)
Sparc/cache0.o: $(SPARC_ASM_H)
Sparc/cglue.o: $(LARCENY_H) Sys/signals.h
Sparc/bdw-cglue.o: Sparc/cglue.c $(LARCENY_H) Sys/signals.h
Sparc/generic.o: $(SPARC_ASM_H)
Sparc/bdw-generic.o: Sparc/generic.s $(SPARC_ASM_H)
Sparc/glue.o: $(SPARC_ASM_H)
Sparc/mcode.o: $(SPARC_ASM_H)
Sparc/memory.o: $(SPARC_ASM_H)
Sparc/signals.o: $(LARCENY_H)
Sparc/syscall2.o: $(LARCENY_H)

Sys/alloc.o: $(LARCENY_H) Sys/barrier.h Sys/gclib.h Sys/semispace_t.h
Sys/argv.o: $(LARCENY_H)
Sys/barrier.o: $(LARCENY_H) Sys/memmgr.h Sys/barrier.h
Sys/bdw-collector.o: $(LARCENY_H) Sys/barrier.h Sys/gc.h Sys/gc_t.h \
	Sys/gclib.h Sys/stats.h Sys/memmgr.h Sys/stack.h \
	bdw-gc/include/gc.h
Sys/bdw-gc.o: Sys/gc.c $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/heapio.h \
	Sys/static_heap_t.h
Sys/bdw-larceny.o: Sys/larceny.c $(LARCENY_H) Sys/gc.h
Sys/bdw-stats.o: Sys/stats.c $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/memmgr.h
Sys/callback.o: $(LARCENY_H)
Sys/cheney.o: $(LARCENY_H) Sys/barrier.h Sys/gc_t.h Sys/gclib.h \
	Sys/los_t.h Sys/memmgr.h Sys/semispace_t.h Sys/static_heap_t.h
Sys/dof-heap.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/los_t.h Sys/memmgr.h Sys/old_heap_t.h \
	Sys/remset_t.h Sys/semispace_t.h Sys/young_heap_t.h
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
	Sys/stats.h Sys/heapio.h Sys/los_t.h Sys/memmgr.h \
	Sys/old_heap_t.h Sys/remset_t.h Sys/static_heap_t.h Sys/young_heap_t.h 
Sys/np-sc-heap.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/los_t.h Sys/memmgr.h Sys/old_heap_t.h \
	Sys/remset_t.h Sys/semispace_t.h Sys/young_heap_t.h
Sys/nursery.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/los_t.h Sys/memmgr.h Sys/stack.h \
	Sys/young_heap_t.h
Sys/old_heap_t.o: $(LARCENY_H) Sys/old_heap_t.h
Sys/old-heap.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/los_t.h Sys/memmgr.h Sys/old_heap_t.h \
	Sys/remset_t.h Sys/semispace_t.h Sys/static_heap_t.h Sys/young_heap_t.h
Sys/osdep.o: $(LARCENY_H)
Sys/remset.o: $(LARCENY_H) Sys/gclib.h Sys/memmgr.h Sys/remset_t.h
Sys/sc-heap.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/los_t.h Sys/memmgr.h Sys/semispace_t.h \
	Sys/stack.h Sys/static_heap_t.h Sys/young_heap_t.h
Sys/semispace.o: $(LARCENY_H) Sys/gclib.h Sys/semispace_t.h
Sys/signals.o: $(LARCENY_H) Sys/signals.h
Sys/sro.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h Sys/heapio.h \
	Sys/memmgr.h
Sys/stack.o: $(LARCENY_H) Sys/stack.h
Sys/static-heap.o: $(LARCENY_H) Sys/gc.h Sys/gclib.h Sys/stats.h \
	Sys/memmgr.h Sys/semispace_t.h Sys/static_heap_t.h
Sys/stats.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/memmgr.h
Sys/syscall.o: $(LARCENY_H) Sys/signals.h
Sys/primitive.o: $(LARCENY_H)  Sys/signals.h
Sys/osdep-unix.o: $(LARCENY_H)
Sys/osdep-win32.o: $(LARCENY_H)
Sys/osdep-generic.o: $(LARCENY_H)
Sys/util.o: $(LARCENY_H) Sys/gc.h Sys/gc_t.h
Sys/version.o: Sys/config.h
Sys/young_heap_t.o: $(LARCENY_H) Sys/young_heap_t.h

# eof

