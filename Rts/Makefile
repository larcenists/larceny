# Copyright 1998 Lars T Hansen       -*- mode: fundamental -*-
#
# $Id$
#
# This is Rts/Makefile.

###########################################################################
#
# Defaults compiler configuration
#
# The defaults are OK with GCC and Sun assembler on Solaris.
#
# You need an ANSI C compiler, and the assembler must run cpp before
# assembly.  Note, GAS does not run CPP.
#
###########################################################################

O=o
CC=gcc
CCXFLAGS=-D__USE_FIXED_PROTOTYPES__ -Wpointer-arith -Wmissing-prototypes \
	-Wimplicit -Wreturn-type -Wunused -Wuninitialized
AS=as
ASXFLAGS=
EXTRALIBPATH=
EXTRALIBS=
LDXFLAGS=
LIBS=-lm -ldl

DEBUGINFO=-g3
PROFILE=#-pg
TCOV=#-a -g
LDYFLAGS=#Util/ffi-dummy.o  # For compiling with -p (avoids linker trouble)
OPTIMIZE=-O3 -DNDEBUG2 # -DNDEBUG

BDW_DEBUG=-DBDW_DEBUG #-DBDW_CLEAR_STACK

CFLAGS=-c -ISys -IBuild -IStandard-C \
	$(DEBUGINFO) $(OPTIMIZE) $(PROFILE) $(TCOV) $(BDW_DEBUG) $(CCXFLAGS)

ASFLAGS=-P -ISparc -IBuild $(DEBUGFLAGS) $(ASXFLAGS)


######################################################################
#                                                                    #
#                 Pick a compiler variation here!                    #
#                                                                    #
######################################################################

# GCC with GAS on Debian SPARC-Linux, at least
#
# ASXFLAGS=-x assembler-with-cpp -c
 

# GCC with Sun assembler on SunOS (obsolete)
#
# AS=/bin/as


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


# Microsoft C/C++ with DOS shell on Win32 (Petit Larceny)
#
# O=obj
# CC=cl


############################################################################
#
# Big bags of files

COMMON_RTS_OBJECTS=\
	Sys/argv.$(O) Sys/barrier.$(O) Sys/callback.$(O) Sys/gc_t.$(O) \
	Sys/ldebug.$(O) Sys/malloc.$(O) Sys/osdep-generic.$(O) \
	Sys/osdep-macos.$(O) Sys/osdep-unix.$(O) Sys/osdep-win32.$(O) \
	Sys/primitive.$(O) Sys/signals.$(O) Sys/sro.$(O) Sys/stack.$(O) \
	Sys/syscall.$(O) Sys/util.$(O) Sys/version.$(O)

PRECISE_GC_OBJECTS=\
	Sys/alloc.$(O) Sys/cheney.$(O) Sys/dof-heap.$(O) Sys/gc.$(O) Sys/heapio.$(O) \
	Sys/los.$(O) Sys/memmgr.$(O) Sys/ffi.$(O) \
	Sys/msgc-core.$(O) Sys/np-sc-heap.$(O) Sys/nursery.$(O) Sys/old_heap_t.$(O) \
	Sys/old-heap.$(O) Sys/remset.$(O) Sys/sc-heap.$(O) Sys/semispace.$(O) \
	Sys/static-heap.$(O) Sys/stats.$(O) Sys/young_heap_t.$(O)

BOEHM_GC_OBJECTS=\
	Sys/bdw-gc.$(O) Sys/bdw-stats.$(O) Sys/bdw-collector.$(O) \
	Sys/bdw-heapio.$(O) Sys/bdw-ffi.$(O)

BOEHM_GC_SPARC_OBJECTS=\
	Sparc/bdw-memory.$(O) Sparc/bdw-generic.$(O) Sparc/bdw-cglue.$(O)

BOEHM_GC_LIBRARIES=\
	bdw-gc/gc.a

COMMON_SPARC_OBJECTS=\
	Sparc/barrier.$(O) Sparc/cache.$(O) Sparc/cache0.$(O) \
	Sparc/glue.$(O) Sparc/mcode.$(O) Sparc/signals.$(O) Sparc/syscall2.$(O) \
	Build/sparc-table.$(O) 

SPARC_PRECISE_GC_OBJECTS=\
	Sparc/memory.$(O) Sparc/generic.$(O) Sparc/cglue.$(O)

PETIT_OBJECTS=\
	Standard-C/arithmetic.$(O) Standard-C/millicode.$(O) \
	Standard-C/multiply.$(O) Standard-C/syscall2.$(O) Build/c-table.$(O)

# SPARC only
LARCENY_OBJECTS=\
	Sys/larceny.$(O) \
	Sparc/config.$(O) \
	$(COMMON_RTS_OBJECTS) \
	$(COMMON_SPARC_OBJECTS) \
	$(PRECISE_GC_OBJECTS) \
	$(SPARC_PRECISE_GC_OBJECTS)

# SPARC only
BDW_LARCENY_OBJECTS=\
	Sys/bdw-larceny.$(O) \
	Sparc/config.$(O) \
	$(COMMON_RTS_OBJECTS) \
	$(COMMON_SPARC_OBJECTS) \
	$(BOEHM_GC_OBJECTS) \
	$(BOEHM_GC_SPARC_OBJECTS)

# Generic Unix
PETIT_LARCENY_OBJECTS=\
	Sys/larceny.$(O) \
	Standard-C/config.$(O) \
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

# Win32
.c.obj:
	cl /c /Zp4 /O2 /Zi /ISys /IStandard-C /IBuild /DSTDC_SOURCE /Fo$*.obj $<

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

# Win32
petit-rts.lib: $(PETIT_LARCENY_OBJECTS)
	lib /libpath:Rts /name:petit-rts /out:petit-rts.lib $(PETIT_LARCENY_OBJECTS)

hsplit: Util/hsplit.o
	$(CC) $(PROFILE) $(TCOV) $(DEBUGINFO) $(LDXFLAGS) -o hsplit Util/hsplit.o

Util/hsplit.o: Util/hsplit.c
	$(CC) -g -o Util/hsplit.o -IBuild -ISys -c Util/hsplit.c

clean:
	rm larceny.bin hsplit bdwlarceny.bin petit-larceny core \
	   Sys/*.$(O) Sparc/*.$(O) Build/*.$(O) Standard-C/*.$(O) Util/*.$(O) \
	   petit-rts.lib petit-lib.lib libpetit.so Standard-C/arithmetic.c

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
Sys/bdw-ffi.o: Sys/ffi.c $(LARCENY_H)
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

