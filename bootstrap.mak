# Sample makefile for building Petit Larceny from C sources on
# Unix-like systems.  This was used to bootstrap Petit Larceny on
# MacOS X and Linux.  It may require Gnu Make.

ENDIANNESS=el
OSDEP=unix

HEAPFILES=Twobit/expand.o Twobit/lowlevel.o Twobit/pass1.aux.o Twobit/pass1.o Twobit/pass2.aux.o Twobit/prefs.o Twobit/syntaxenv.o Twobit/syntaxrules.o Twobit/usual.o Interpreter/interp-prim.o Interpreter/interp.o Interpreter/macro-expand.o Interpreter/switches.o Lib/Common/belle.o Lib/Common/bignums-$(ENDIANNESS).o Lib/Common/bignums.o Lib/Common/command-line.o Lib/Common/conio.o Lib/Common/contag.o Lib/Common/control.o Lib/Common/dump.o Lib/Common/ecodes.o Lib/Common/ehandler.o Lib/Common/env.o Lib/Common/error.o Lib/Common/error0.o Lib/Common/eval.o Lib/Common/exit.o Lib/Common/fileio.o Lib/Common/flonums-$(ENDIANNESS).o Lib/Common/flonums.o Lib/Common/format.o Lib/Common/gcctl.o Lib/Common/globals.o Lib/Common/go.o Lib/Common/hash.o Lib/Common/hashtable.o Lib/Common/ioboot.o Lib/Common/iosys.o Lib/Common/list.o Lib/Common/load.o Lib/Common/malcode.o Lib/Common/mcode.o Lib/Common/memstats.o Lib/Common/num2str.o Lib/Common/number.o Lib/Common/oblist.o Lib/Common/preds.o Lib/Common/print.o Lib/Common/procinfo.o Lib/Common/profile.o Lib/Common/ratnums.o Lib/Common/reader.o Lib/Common/rectnums.o Lib/Common/secret.o Lib/Common/sort.o Lib/Common/stdio.o Lib/Common/str2num.o Lib/Common/string.o Lib/Common/stringio.o Lib/Common/struct.o Lib/Common/sys-$(OSDEP).o Lib/Common/syscall-id.o Lib/Common/syshooks.o Lib/Common/sysparam.o Lib/Common/system-interface.o Lib/Common/timer.o Lib/Common/toplevel.o Lib/Common/typetags.o Lib/Common/vector.o Lib/Standard-C/loadable.o Lib/Standard-C/primops.o Lib/Standard-C/toplevel-target.o Repl/main.o Repl/reploop.o HEAPDATA.o

CFLAGS += -O3 -gstabs+ -IRts/Build -IRts -IRts/Sys -IRts/Standard-C

petit: libheap.a petit.o
	( cd Rts ; $(MAKE) libpetit.a )
	cc -gstabs+ -o petit petit.o -L. -LRts -lpetit -lheap -lm

libheap.a: $(HEAPFILES)
	ar -r libheap.a $(HEAPFILES)
	ranlib libheap.a
