CHDRS=../main.h ../offsets.h ../macros.h ../millicode.h ../layouts.h
AHDRS=../registers.s.h ../offsets.h ../millicode.h ../layouts.s.h
INCLUDE=/owyhee2/users/lth/gc

# PROFILE=-pg
DEBUG=-g
# DFLAG=-DDEBUG
CC=cc
# OPTIMIZE=-O4
CFLAGS=-c $(OPTIMIZE) $(PROFILE) $(DEBUG) -I$(INCLUDE) $(DFLAG)

.s.o:
	as -P $(DFLAG) -I$(INCLUDE) -o $*.o $<
.c.o:
	$(CC) $(CFLAGS)  -o $*.o $<

#

STDOBJS=../main.o ../gc.o ../memsupport.o ../schemestart.o ../exception.o \
	../memory.o
BENCH1OBJS=bench1.o $(STDOBJS)
BENCH2OBJS=bench2.o $(STDOBJS)
BENCH1BOBJS=bench1b.o $(STDOBJS)
BENCH1COBJS=bench1c.o $(STDOBJS)

#

bench1: $(BENCH1OBJS)
	$(CC) $(PROFILE) -o bench1 $(BENCH1OBJS)

bench1b: $(BENCH1BOBJS)
	$(CC) $(PROFILE) -o bench1b $(BENCH1BOBJS)

bench1c: $(BENCH1COBJS)
	$(CC) $(PROFILE) -o bench1c $(BENCH1COBJS)

bench2:	$(BENCH2OBJS)
	$(CC) $(PROFILE) -o bench2 $(BENCH2OBJS)

clean:
	rm -f $(STDOBJS) bench1.o bench2.o

../main.o:		../main.c $(CHDRS)
../exception.o:		../exception.s $(AHDRS)
../memsupport.o:	../memsupport.c $(CHDRS)
../memory.o:		../memory.s $(AHDRS)
../gc.o:		../gc.c ../gc.h ../gcinterface.h ../offsets.h
../schemestart.o:	../schemestart.s $(AHDRS)
bench1.o:		bench1.s $(AHDRS)
bench1b.o:		bench1b.s $(AHDRS)
bench1c.o:		bench1c.s $(AHDRS)
bench2.o:		bench2.s $(AHDRS)
