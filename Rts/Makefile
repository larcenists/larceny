# Copyright 1998 Lars T Hansen       -*- mode: fundamental -*-
#
# $Id$
#
# This is Rts/Makefile.


###########################################################################
#
# Compiler/assembler selection.  Uncomment one of these or hack up a new one.

# DEFAULT CONFIGURATION
#
# Petit Larceny on generic Unix with gcc
O=o
CC=gcc
DEBUGINFO=-gstabs+
OPTIMIZE=-O3 -DNDEBUG2 # -DNDEBUG
CFLAGS=-c -ISys -IBuild -IStandard-C $(DEBUGINFO) $(OPTIMIZE)

# Petit Larceny on Win32: Metrowerks CodeWarrior 6
#O=obj
#CC=mwcc
#OPT=-opt on
#DEBUG=-g
#CFLAGS=-c $(DEBUG) $(OPT) -ISys -IBuild -IStandard-C
#LIBS=-lm
#.c.obj:
#	$(CC) -c $(CFLAGS) -o $*.obj $<


# Petit Larceny on MacOS X: gcc disguised as cc
#O=o
#CC=cc
#DEBUG=-gstabs+
#OPTIMIZE=-O3 -DNDEBUG2 # -DNDEBUG
#CCXFLAGS=-D__USE_FIXED_PROTOTYPES__ -Wpointer-arith -Wmissing-prototypes \
#	-Wimplicit -Wreturn-type -Wunused -Wuninitialized
#CFLAGS=-c -ISys -IBuild -IStandard-C $(DEBUG) $(OPTIMIZE) $(CCXFLAGS)
#LIBS=-lm
#.c.o:
#	$(CC) $(CFLAGS) -o $*.o $<


# Petit Larceny on DEC Alpha OSF/1: DEC C compiler
# Probably not up-to-date, has not been tested for some time!
#CC=cc
#CCXFLAGS=-g3 -taso -xtaso_short -ieee
#LD_SHARED=ld -shared -taso -g3 -soname libpetit.so -o libpetit.so -lm -lc -all
## Needed because make leaves the -o off.
# Why is this here?  See other .c.o rule below.
#.c.o:
#	$(CC) $(CFLAGS) -c $< -o $*.o


# Microsoft Visual C/C++ with DOS shell on Win32
# Probably not up-to-date, has not been tested for some time!
#O=obj
#CC=cl
#.c.obj:
# 	cl /c /Zp4 /O2 /Zi /ISys /IStandard-C /IBuild /DSTDC_SOURCE /Fo$*.obj $<


# Native Larceny on Solaris/SPARC: gcc
#
# The assembler must be able to run the preprocessor; the Sun
# assembler accepts -P to do this, but the GNU assembler does not.  If
# you experience assembler errors, you most likely have the wrong
# assembler.  On SunOS systems, try /bin/as.  Or look at 
# ../Util/gasmask.sh, which runs the preprocessor for you.

#O=o
#CC=gcc
#DEBUG=-gstabs+
#OPTIMIZE=-O3 -DNDEBUG2 # -DNDEBUG
#PROFILE=#-pg
#TCOV=#-a -g
#BDW_DEBUG=-DBDW_DEBUG #-DBDW_CLEAR_STACK
#LDXFLAGS=#Util/ffi-dummy.o  # For compiling with -p (avoids linker trouble)
#CFLAGS=-c -ISys -IBuild -IStandard-C \
#	$(DEBUG) $(OPTIMIZE) $(PROFILE) $(TCOV) $(BDW_DEBUG) $(CCXFLAGS)
#CCXFLAGS=-D__USE_FIXED_PROTOTYPES__ -Wpointer-arith -Wmissing-prototypes \
#	-Wimplicit -Wreturn-type -Wunused -Wuninitialized
#AS=../Util/gasmask.sh
##These flags for the native Solaris assembler only
##ASFLAGS=-P -ISparc -IBuild
#LIBS=-lm -ldl
#.c.o:
#	$(CC) $(CFLAGS) -DUSER=\"$$LOGNAME\" -DDATE="\"`date '+%Y-%m-%d %T'`\"" -o $*.o $<


# Native Larceny on Debian Linux: gcc + nasm (still debugging)
#O=o
#CC=gcc
#DEBUGINFO=-gstabs+
#OPTIMIZE=-O3 -DNDEBUG2 # -DNDEBUG
#CFLAGS=-c -ISys -IBuild -IStandard-C $(DEBUGINFO) $(OPTIMIZE)
#AS=nasm
#ASFLAGS=-f elf -IIntel -IBuild -DLINUX


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
	Sys/alloc.$(O) Sys/cheney.$(O) Sys/dof-heap.$(O) Sys/gc.$(O) \
	Sys/heapio.$(O) Sys/los.$(O) Sys/memmgr.$(O) Sys/ffi.$(O) \
	Sys/msgc-core.$(O) Sys/np-sc-heap.$(O) Sys/nursery.$(O) \
	Sys/old_heap_t.$(O) Sys/old-heap.$(O) Sys/remset.$(O) \
	Sys/sc-heap.$(O) Sys/semispace.$(O) Sys/static-heap.$(O) \
	Sys/stats.$(O) Sys/young_heap_t.$(O)

BOEHM_GC_OBJECTS=\
	Sys/bdw-gc.$(O) Sys/bdw-stats.$(O) Sys/bdw-collector.$(O) \
	Sys/bdw-heapio.$(O) Sys/bdw-ffi.$(O)

BOEHM_GC_SPARC_OBJECTS=\
	Sparc/bdw-memory.$(O) Sparc/bdw-generic.$(O) Sparc/bdw-cglue.$(O)

BOEHM_GC_LIBRARIES=\
	bdw-gc/gc.a

COMMON_SPARC_OBJECTS=\
	Sparc/barrier.$(O) Sparc/cache.$(O) Sparc/cache0.$(O) \
	Sparc/glue.$(O) Sparc/mcode.$(O) Sparc/signals.$(O) \
	Sparc/syscall2.$(O) Build/sparc-table.$(O) 

SPARC_PRECISE_GC_OBJECTS=\
	Sparc/memory.$(O) Sparc/generic.$(O) Sparc/cglue.$(O)

PETIT_OBJECTS=\
	Standard-C/arithmetic.$(O) Standard-C/millicode.$(O) \
	Standard-C/multiply.$(O) Standard-C/syscall2.$(O) Build/c-table.$(O)

X86_NASM_OBJECTS=\
	Standard-C/arithmetic.$(O) Standard-C/millicode.$(O) \
	Intel/i386-driver.$(O) Intel/i386-millicode.$(O) \
	Standard-C/multiply.$(O) Standard-C/syscall2.$(O) Build/nasm-table.$(O)

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

# Intel x86 with the NASM backend
X86_NASM_LARCENY_OBJECTS=\
	Sys/larceny.$(O) \
	Standard-C/config.$(O) \
	$(COMMON_RTS_OBJECTS) \
	$(PRECISE_GC_OBJECTS) \
	$(X86_NASM_OBJECTS)


# Generated header files.
# ACFG, CCFG, SCFG, and HDRFILES also exists in ../Makefile.  Watch it!

CCFG=Build/globals.ch Build/except.ch Build/layouts.ch Build/mprocs.ch

ACFG=Build/globals.ah Build/regs.ah Build/except.ah Build/layouts.ah \
	Build/mprocs.ah

SCFG=Build/globals.sh Build/regs.sh Build/except.sh Build/layouts.sh 


############################################################################
#
# Rules

.SUFFIXES:	.cfg .ch .ah .sh .mac .asm

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

.asm.o:
	$(AS) $(ASFLAGS) -o $*.o $<


############################################################################
#
# Targets

default:
	echo "Please give me a target."

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
		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) $(LDXFLAGS) 
	/bin/rm -f Sys/version.o

bdwlarceny.bin: $(BDW_LARCENY_OBJECTS) Util/ffi-dummy.o
	( cd bdw-gc ; make gc.a )
	$(CC) $(PROFILE) $(TCOV) -o bdwlarceny.bin $(BDW_LARCENY_OBJECTS) \
		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) \
		$(BOEHM_GC_LIBRARIES) $(LDXFLAGS) $(LDYFLAGS)
	/bin/rm -f Sys/version.o

# SunOS 5.6, for sure.
#libpetit.so: $(PETIT_LARCENY_OBJECTS)
#	ld -G -o libpetit.so -L/usr/lib -lc $(PETIT_LARCENY_OBJECTS) \
#		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) $(LDXFLAGS)
#	/bin/rm -f Sys/version.o

# MacOS X shared object (not tested much)
#libpetit.so: $(PETIT_LARCENY_OBJECTS)
#	$(CC) -r -shared -o libpetit.so $(PETIT_LARCENY_OBJECTS) \
#		$(LIBS) $(EXTRALIBS) $(EXTRALIBPATH) $(LDXFLAGS) \
#		-L/usr/lib -lc -lm
#	/bin/rm -f Sys/version.o

# Generic Unix
libpetit.a: $(PETIT_LARCENY_OBJECTS)
	ar -r libpetit.a $(PETIT_LARCENY_OBJECTS)
	ranlib libpetit.a

# Generic Unix (?)
libpetit.so: $(PETIT_LARCENY_OBJECTS)
	gcc -shared -o libpetit.so $(PETIT_LARCENY_OBJECTS)

# Intel x86 with the NASM back-end, on Unix
libx86.a: $(X86_NASM_LARCENY_OBJECTS)
	ar -r libx86.a $(X86_NASM_LARCENY_OBJECTS)
	ranlib libx86.a

# Win32
petit-rts.lib: $(PETIT_LARCENY_OBJECTS)
	lib /libpath:Rts /name:petit-rts /out:petit-rts.lib \
	   $(PETIT_LARCENY_OBJECTS)

hsplit: Util/hsplit.o
	$(CC) $(PROFILE) $(TCOV) $(DEBUGINFO) $(LDXFLAGS) -o hsplit \
	   Util/hsplit.o

Util/hsplit.o: Util/hsplit.c
	$(CC) -g -o Util/hsplit.o -IBuild -ISys -c Util/hsplit.c

clean:
	rm -f larceny.bin hsplit bdwlarceny.bin petit-larceny core \
	   Build/*.$(O) Intel/*.$(O) Sparc/*.$(O) Standard-C/*.$(O) \
	   Sys/*.$(O) Util/*.$(O) \
	   petit-rts.lib petit-lib.lib libpetit.so libpetit.a \
	   Standard-C/arithmetic.c

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
	  Build/cdefs.h Build/config.h
SPARC_ASM_H=Build/asmdefs.h Sparc/asmmacro.h
PETIT_H=Standard-C/millicode.h Standard-C/petit-config.h \
	Standard-C/petit-machine.h

Standard-C/arithmetic.$(O): $(LARCENY_H) $(PETIT_H)
Standard-C/millicode.$(O): $(LARCENY_H) $(PETIT_H) Sys/gc_t.h Sys/barrier.h \
	Sys/stack.h
Standard-C/multiply.$(O): $(LARCENY_H) $(PETIT_H)
Standard-C/syscall2.$(O): $(LARCENY_H) $(PETIT_H)

Sparc/barrier.$(O): $(SPARC_ASM_H)
Sparc/bdw-memory.$(O): Sparc/memory.s $(SPARC_ASM_H)
Sparc/cache.$(O): $(LARCENY_H)
Sparc/cache0.$(O): $(SPARC_ASM_H)
Sparc/cglue.$(O): $(LARCENY_H) Sys/signals.h
Sparc/bdw-cglue.$(O): Sparc/cglue.c $(LARCENY_H) Sys/signals.h
Sparc/generic.$(O): $(SPARC_ASM_H)
Sparc/bdw-generic.$(O): Sparc/generic.s $(SPARC_ASM_H)
Sparc/glue.$(O): $(SPARC_ASM_H)
Sparc/mcode.$(O): $(SPARC_ASM_H)
Sparc/memory.$(O): $(SPARC_ASM_H)
Sparc/signals.$(O): $(LARCENY_H)
Sparc/syscall2.$(O): $(LARCENY_H)

Sys/alloc.$(O): $(LARCENY_H) Sys/barrier.h Sys/gclib.h Sys/semispace_t.h
Sys/argv.$(O): $(LARCENY_H)
Sys/barrier.$(O): $(LARCENY_H) Sys/memmgr.h Sys/barrier.h
Sys/bdw-collector.$(O): $(LARCENY_H) Sys/barrier.h Sys/gc.h Sys/gc_t.h \
	Sys/gclib.h Sys/stats.h Sys/memmgr.h Sys/stack.h \
	bdw-gc/include/gc.h
Sys/bdw-gc.$(O): Sys/gc.c $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/heapio.h \
	Sys/static_heap_t.h
Sys/bdw-larceny.$(O): Sys/larceny.c $(LARCENY_H) Sys/gc.h
Sys/bdw-stats.$(O): Sys/stats.c $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/memmgr.h
Sys/bdw-ffi.$(O): Sys/ffi.c $(LARCENY_H)
Sys/callback.$(O): $(LARCENY_H)
Sys/cheney.$(O): $(LARCENY_H) Sys/barrier.h Sys/gc_t.h Sys/gclib.h \
	Sys/los_t.h Sys/memmgr.h Sys/semispace_t.h Sys/static_heap_t.h
Sys/dof-heap.$(O): $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/los_t.h Sys/memmgr.h Sys/old_heap_t.h \
	Sys/remset_t.h Sys/semispace_t.h Sys/young_heap_t.h
Sys/ffi.$(O): $(LARCENY_H)
Sys/gc.$(O): $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/heapio.h Sys/semispace_t.h \
	Sys/static_heap_t.h
Sys/gc_t.$(O): $(LARCENY_H) Sys/gc_t.h
Sys/heapio.$(O): $(LARCENY_H) Sys/heapio.h Sys/semispace_t.h Sys/gclib.h
Sys/larceny.$(O): $(LARCENY_H) Sys/gc.h
Sys/ldebug.$(O): $(LARCENY_H) Sys/macros.h
Sys/los.$(O): $(LARCENY_H) Sys/gclib.h Sys/los_t.h
Sys/malloc.$(O): $(LARCENY_H)
Sys/memmgr.$(O): $(LARCENY_H) Sys/barrier.h Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/heapio.h Sys/los_t.h Sys/memmgr.h \
	Sys/old_heap_t.h Sys/remset_t.h Sys/static_heap_t.h Sys/young_heap_t.h 
Sys/np-sc-heap.$(O): $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/los_t.h Sys/memmgr.h Sys/old_heap_t.h \
	Sys/remset_t.h Sys/semispace_t.h Sys/young_heap_t.h
Sys/nursery.$(O): $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/los_t.h Sys/memmgr.h Sys/stack.h \
	Sys/young_heap_t.h
Sys/old_heap_t.$(O): $(LARCENY_H) Sys/old_heap_t.h
Sys/old-heap.$(O): $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/los_t.h Sys/memmgr.h Sys/old_heap_t.h \
	Sys/remset_t.h Sys/semispace_t.h Sys/static_heap_t.h Sys/young_heap_t.h
Sys/osdep.$(O): $(LARCENY_H)
Sys/remset.$(O): $(LARCENY_H) Sys/gclib.h Sys/memmgr.h Sys/remset_t.h
Sys/sc-heap.$(O): $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/los_t.h Sys/memmgr.h Sys/semispace_t.h \
	Sys/stack.h Sys/static_heap_t.h Sys/young_heap_t.h
Sys/semispace.$(O): $(LARCENY_H) Sys/gclib.h Sys/semispace_t.h
Sys/signals.$(O): $(LARCENY_H) Sys/signals.h
Sys/sro.$(O): $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h Sys/heapio.h \
	Sys/memmgr.h
Sys/stack.$(O): $(LARCENY_H) Sys/stack.h
Sys/static-heap.$(O): $(LARCENY_H) Sys/gc.h Sys/gclib.h Sys/stats.h \
	Sys/memmgr.h Sys/semispace_t.h Sys/static_heap_t.h
Sys/stats.$(O): $(LARCENY_H) Sys/gc.h Sys/gc_t.h Sys/gclib.h \
	Sys/stats.h Sys/memmgr.h
Sys/syscall.$(O): $(LARCENY_H) Sys/signals.h
Sys/primitive.$(O): $(LARCENY_H)  Sys/signals.h
Sys/osdep-unix.$(O): $(LARCENY_H)
Sys/osdep-win32.$(O): $(LARCENY_H)
Sys/osdep-generic.$(O): $(LARCENY_H)
Sys/util.$(O): $(LARCENY_H) Sys/gc.h Sys/gc_t.h
Sys/version.$(O): Build/config.h
Sys/young_heap_t.$(O): $(LARCENY_H) Sys/young_heap_t.h

# eof

