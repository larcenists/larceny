# This file will be processed by nmake, not make!

# Creates SchemeMachine.dll in the current directory.

SRCS = Call.cs \
	Exn.cs \
	Reg.cs \
	Instructions.cs \
	SchemeObject.cs \
	Ops.cs \
	OpsSpecial.cs \
	Number.cs \
	Syscall.cs \
	Syscall-enum.cs \
	Constants.cs \
	Load.cs

GC_SRC = CFrame\GC.cs
ISH_SRC = CFrame\ISH-alt.cs

# Mac OS X specific defines.
CSC = csc /d:USING_ROTOR /d:BIG_ENDIAN
#CSC=csc

DEBUG_OPT = /debug:full /d:DEBUG
#DEBUG_OPT = 
OUTFILE = Scheme.dll

NO_INLINE_CALLS = /d:NO_INLINE_CALLS
#INLINE_OPT = $(NO_INLINE_CALLS)
INLINE_OPT = /d:NO_INLINE_OPTIONS

default : ISH-FieldRegs DynLoad.exe

clean :
	rm $(OUTFILE)

GC : $(SRCS) $(GC_SRC)
	$(CSC) $(INLINE_OPT) $(DEBUG_OPT) /target:library /out:$(OUTFILE) $(SRCS) $(GC_SRC)

ISH-FieldRegs : $(SRCS) $(ISH_SRC)
	$(CSC) $(INLINE_OPT) $(DEBUG_OPT) /target:library /out:$(OUTFILE) $(SRCS) $(ISH_SRC)

DynLoad.exe : DynLoad.cs
	$(CSC) $(INLINE_OPT) $(DEBUG_OPT) /out:DynLoad.exe /r:$(OUTFILE) DynLoad.cs 

XLoadCode.exe : XLoadCode.cs
	$(CSC) $(INLINE_OPT) $(DEBUG_OPT) /out:XLoadCode.exe /r:$(OUTFILE) XLoadCode.cs
