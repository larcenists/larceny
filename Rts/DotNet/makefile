##########################################
## USER-CONFIGURABLE SECTION
##########################################

# Uncomment the block corresponding to the platform 
# you are building on.

## Win32 (.NET SDK)
CSC = csc

## MacOS X (Rotor)
#CSC = csc /d:USING_ROTOR /d:BIG_ENDIAN

## Solaris (Mono)
#CSC = mcs /d:BIG_ENDIAN

## Linux (Mono)
#CSC = mcs

##########################################
## END USER-CONFIGURABLE SECTION
##########################################

# Creates Scheme.dll (Scheme runtime) in the current directory
# Creates DynLoad.exe (loader) in the current directory

SRCS =	Call.cs \
	Exn.cs \
	Reg.cs \
	ContinuationISH.cs \
	Instructions.cs \
	SchemeObject.cs \
	Factory.cs \
	Ops.cs \
	OpsSpecial.cs \
	Number.cs \
	Syscall.cs \
	Syscall-enum.cs \
	Constants.cs \
	Load.cs \
	FFI.cs

DEBUG_OPT = /debug:full /d:DEBUG
#DEBUG_OPT = 

OUTFILE = Scheme.dll

default : Scheme.dll DynLoad.exe

clean :
	rm Scheme.dll DynLoad.exe

Scheme.dll : $(SRCS)
	$(CSC) $(INLINE_OPT) $(DEBUG_OPT) /target:library /out:Scheme.dll $(SRCS) $(ISH_SRC)

DynLoad.exe : DynLoad.cs
	$(CSC) /target:winexe $(INLINE_OPT) $(DEBUG_OPT) /out:DynLoad.exe /r:Scheme.dll DynLoad.cs 

SchemeObject.cs : SchemeObject.cs.cpp Ops.h Ops_SObject.inc Ops_SImmediate.inc Ops_SFixnum.inc Ops_SChar.inc Ops_SVL.inc Ops_SByteVL.inc Ops_SPair.inc Ops_Procedure.inc
	cpp -P -C -nostdinc SchemeObject.cs.cpp -o SchemeObject.cs

FFI.cs : FFI.cs.cpp Macros.h
	cpp -P -C -nostdinc FFI.cs.cpp -o FFI.cs
