##########################################
## USER-CONFIGURABLE SECTION
##########################################

# Parameters are set by make invocation constructed 
# by (build-runtime-system); see Util/dotnet.sch

## Win32 (.NET SDK)
#CSC = csc
#DEFINES = /d:HAS_OSVERSION /d:HAS_PERFORMANCE_COUNTERS /d:HAS_WINDOWS_FORMS /d:HAS_SETENV_SUPPORT

## MacOS X (Rotor)
#CSC = csc
#DEFINES = /d:USING_ROTOR /d:BIG_ENDIAN

## Solaris & MacOS X (Mono)
#CSC = mcs
#DEFINES = /d:BIG_ENDIAN /d:USING_MONO /d:USING_UNIX

## Linux (Mono)
# CSC = mcs
# DEFINES = /d:USING_MONO /d:USING_UNIX

##########################################
## END USER-CONFIGURABLE SECTION
##########################################

# Creates Scheme.dll (Scheme runtime) in the current directory
# Creates DynLoad.exe (loader) in the current directory

SRCS =	CodeAddress.cs \
	Call.cs \
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

#DEBUG_OPT = /checked+ /warn:4 /debug:full /d:DEBUG
#DEBUG_OPT = /optimize+

OUTFILE = Scheme.dll

default : Scheme.dll DynLoad.exe

clean :
	rm Scheme.dll DynLoad.exe

Scheme.dll : $(SRCS)
	$(CSC) $(INLINE_OPT) $(DEBUG_OPT) $(DEFINES) /target:library /out:Scheme.dll $(SRCS) $(ISH_SRC)

DynLoad.exe : DynLoad.cs
	$(CSC) /target:winexe $(INLINE_OPT) $(DEBUG_OPT) $(DEFINES) /out:DynLoad.exe /r:Scheme.dll DynLoad.cs

SchemeObject.cs : SchemeObject.cs.cpp Ops.h Ops_SObject.inc Ops_SImmediate.inc Ops_SFixnum.inc Ops_SChar.inc Ops_SVL.inc Ops_SByteVL.inc Ops_SPair.inc Ops_Procedure.inc
	cpp -P -C -nostdinc SchemeObject.cs.cpp -o SchemeObject.cs
