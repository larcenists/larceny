HDRS=../main.h ../offsets.h ../macros.h ../millicode.h
AHDRS=../registers.s.h ../offsets.h ../millicode.h
INCLUDE=/owyhee2/users/lth/gc
OBJS=memtest.o memtest2.o ../memsupport.o ../gc.o ../memory.o 1stest/sum.o 1stest/consup.o ../exception.o

OPTIMIZE=-O -fstrength-reduce
CC=gcc
CFLAGS=-c $(OPTIMIZE) -g -I$(INCLUDE) -DDEBUG

.s.o:
	as -P -I$(INCLUDE) -DASSEMBLY -o $*.o $<
.c.o:
	$(CC) $(CFLAGS)  -o $*.o $<

memtest: $(OBJS)
	$(CC) -o memtest $(OBJS)

gctest:	gctest.o ../gc.o
	$(CC) -o gctest gctest.o ../gc.o

1stest/sum.o:		1stest/sum.s $(AHDRS)
1stest/consup.o:	1stest/consup.s $(AHDRS)
../exception.o:		../exception.s $(AHDRS)
memtest.o:		memtest.c $(HDRS)
../memsupport.o:	../memsupport.c $(HDRS) ../layouts.h
../memory.o:		../memory.s $(AHDRS)
memtest2.o:		memtest2.s $(AHDRS)
gctest.o:		gctest.c ../gcinterface.h
../gc.o:		../gc.c ../gc.h ../gcinterface.h ../offsets.h
