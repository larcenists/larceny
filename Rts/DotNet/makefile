##########################################
## USER-CONFIGURABLE SECTION
##########################################

# Uncomment the block corresponding to the platform 
# you are building on.

## Win32 (.NET SDK)
CSC = csc

## MacOS X (Rotor)
#CSC = csc /d:USING_ROTOR /d:BIG_ENDIAN

## Linux (Mono)
#CSC = mcs

## Solaris (Mono)
#CSC = mcs /d:BIG_ENDIAN

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
	$(CSC) $(INLINE_OPT) $(DEBUG_OPT) /out:DynLoad.exe /r:Scheme.dll DynLoad.cs 
