; CommonLarceny.mm
; MakeMSI file for Common Larceny
;
; This is *not* a Scheme source file (despite the semi colon comments)

;; At some point, we will factor out functionality for a 
;; common header file (with extension .mmh) and then 
;; use #include to include it.  But for now, a single file 
;; should do.
#include "Larceny.mmh"

;; Almost everything below was first swiped from CL/files.ss
;; and then munged by hand afterward.  We should unify the 
;; distribution mechanisms (prefably across the board, not just
;; on Common Larceny)

#define? CL_ROOT [ProgramFilesFolder]\CommonLarceny\

<$DirectoryTree Key="CL_INSTALLDIR" Dir="<$CL_ROOT>" CHANGE="\" PrimaryFolder="Y">
<$DirectoryTree Key="CL_ASMCMDIR"   Dir="[CL_INSTALLDIR]\Asm\Common">
<$DirectoryTree Key="CL_ASMILDIR"   Dir="[CL_INSTALLDIR]\Asm\IL\">
<$DirectoryTree Key="CL_AUXLIBDIR"  Dir="[CL_INSTALLDIR]\Auxlib\">
<$DirectoryTree Key="CL_BINDBGDIR"  Dir="[CL_INSTALLDIR]\bin\Debug\">
<$DirectoryTree Key="CL_BINRELDIR"  Dir="[CL_INSTALLDIR]\bin\Release\">
<$DirectoryTree Key="CL_CLDIR"      Dir="[CL_INSTALLDIR]\CL\">
<$DirectoryTree Key="CL_CLSCRDIR"   Dir="[CL_INSTALLDIR]\CL\Scripts\">
<$DirectoryTree Key="CL_CLVS8LDIR"  Dir="[CL_INSTALLDIR]\CL\VS8\Larceny\">
<$DirectoryTree Key="CL_CLVS8LBDIR" Dir="[CL_INSTALLDIR]\CL\VS8\Larceny\Bundle\">
<$DirectoryTree Key="CL_CLVS8LCDIR" Dir="[CL_INSTALLDIR]\CL\VS8\Larceny\Configure\">
<$DirectoryTree Key="CL_CLVS8LHDIR" Dir="[CL_INSTALLDIR]\CL\VS8\Larceny\Heap\">
<$DirectoryTree Key="CL_CLVS8LMDIR" Dir="[CL_INSTALLDIR]\CL\VS8\Larceny\Msi\">
<$DirectoryTree Key="CL_CLVS8LPDIR" Dir="[CL_INSTALLDIR]\CL\VS8\Larceny\Preprocess\">
<$DirectoryTree Key="CL_CLVSNLDIR"  Dir="[CL_INSTALLDIR]\CL\VSNET\Larceny\">
<$DirectoryTree Key="CL_CLVSNLBDIR" Dir="[CL_INSTALLDIR]\CL\VSNET\Larceny\Bundle\">
<$DirectoryTree Key="CL_CLVSNLCDIR" Dir="[CL_INSTALLDIR]\CL\VSNET\Larceny\Configure\">
<$DirectoryTree Key="CL_CLVSNLHDIR" Dir="[CL_INSTALLDIR]\CL\VSNET\Larceny\Heap\">
<$DirectoryTree Key="CL_CLVSNLMDIR" Dir="[CL_INSTALLDIR]\CL\VSNET\Larceny\Msi\">
<$DirectoryTree Key="CL_CLVSNLPDIR" Dir="[CL_INSTALLDIR]\CL\VSNET\Larceny\Preprocess\">
<$DirectoryTree Key="CL_COMPATLDIR" Dir="[CL_INSTALLDIR]\Compat\Larceny\">
<$DirectoryTree Key="CL_COMPATMDIR" Dir="[CL_INSTALLDIR]\Compat\MzScheme\">
<$DirectoryTree Key="CL_DOCSDIR"    Dir="[CL_INSTALLDIR]\Docs\">
<$DirectoryTree Key="CL_INTERPDIR"  Dir="[CL_INSTALLDIR]\Interpreter\">
<$DirectoryTree Key="CL_LIBDIR"     Dir="[CL_INSTALLDIR]\Lib\">
<$DirectoryTree Key="CL_LIBCOMDIR"  Dir="[CL_INSTALLDIR]\Lib\Common\">
<$DirectoryTree Key="CL_LIBILDIR"   Dir="[CL_INSTALLDIR]\Lib\IL\">
<$DirectoryTree Key="CL_LIBMZSDIR"  Dir="[CL_INSTALLDIR]\Lib\MzScheme\">
<$DirectoryTree Key="CL_LIBMZSMDIR" Dir="[CL_INSTALLDIR]\Lib\MzScheme\simple-macros\">
<$DirectoryTree Key="CL_LIBSRFIDIR" Dir="[CL_INSTALLDIR]\Lib\SRFI\">
<$DirectoryTree Key="CL_REPLDIR"    Dir="[CL_INSTALLDIR]\Repl\">
<$DirectoryTree Key="CL_RTSDIR"     Dir="[CL_INSTALLDIR]\Rts\">
<$DirectoryTree Key="CL_RTSBLDDIR"  Dir="[CL_INSTALLDIR]\Rts\Build\">
<$DirectoryTree Key="CL_RTSDNETDIR" Dir="[CL_INSTALLDIR]\Rts\DotNet\">
<$DirectoryTree Key="CL_COMPDIR"    Dir="[CL_INSTALLDIR]\Compiler\">
<$DirectoryTree Key="CL_UTILDIR"    Dir="[CL_INSTALLDIR]\Util\">

<$File  Source="dotnet.heap.exe" Destination="CL_INSTALLDIR\CommonLarceny.exe">

<$Files "COPYRIGHT"				DestDir="CL_INSTALLDIR">
<$Files "README-DOTNET.txt"			DestDir="CL_INSTALLDIR">
<$Files "startup.sch"				DestDir="CL_INSTALLDIR">
<$Files "Scheme.dll"				DestDir="CL_INSTALLDIR">
<$Files "Twobit.fasl"				DestDir="CL_INSTALLDIR">
<$Files "Twobit.exe"				DestDir="CL_INSTALLDIR">
<$Files "Larceny.fasl"				DestDir="CL_INSTALLDIR">
<$Files "Larceny.exe"				DestDir="CL_INSTALLDIR">

<$Files "Asm\Common\asmutil.sch"		DestDir="CL_ASMCMDIR">
<$Files "Asm\Common\asmutil32-test.sch"		DestDir="CL_ASMCMDIR">
<$Files "Asm\Common\asmutil32.sch"		DestDir="CL_ASMCMDIR">
<$Files "Asm\Common\asmutil32be.sch"		DestDir="CL_ASMCMDIR">
<$Files "Asm\Common\asmutil32el.sch"		DestDir="CL_ASMCMDIR">
<$Files "Asm\Common\dumpheap.sch"		DestDir="CL_ASMCMDIR">
<$Files "Asm\Common\external-assembler.sch"	DestDir="CL_ASMCMDIR">
<$Files "Asm\Common\link-lop.sch"		DestDir="CL_ASMCMDIR">
<$Files "Asm\Common\logior-extra.sch"		DestDir="CL_ASMCMDIR">
<$Files "Asm\Common\makefasl.sch"		DestDir="CL_ASMCMDIR">
<$Files "Asm\Common\pass5p1.sch"		DestDir="CL_ASMCMDIR">

<$Files "Asm\IL\asm-switches.sch"		DestDir="CL_ASMILDIR">
<$Files "Asm\IL\config.sch"			DestDir="CL_ASMILDIR">
<$Files "Asm\IL\dumpheap-extra.sch"		DestDir="CL_ASMILDIR">
<$Files "Asm\IL\dumpheap-il.sch"		DestDir="CL_ASMILDIR">
<$Files "Asm\IL\il-gen.sch"			DestDir="CL_ASMILDIR">
<$Files "Asm\IL\il-rtif.sch"			DestDir="CL_ASMILDIR">
<$Files "Asm\IL\il-sourcefile.sch"		DestDir="CL_ASMILDIR">
<$Files "Asm\IL\il-src2string.sch"		DestDir="CL_ASMILDIR">
<$Files "Asm\IL\il.imp.sch"			DestDir="CL_ASMILDIR">
<$Files "Asm\IL\il.imp2.sch"			DestDir="CL_ASMILDIR">
<$Files "Asm\IL\pass5p2-instructions.sch"	DestDir="CL_ASMILDIR">
<$Files "Asm\IL\pass5p2-listify.sch"		DestDir="CL_ASMILDIR">
<$Files "Asm\IL\pass5p2-ops.sch"		DestDir="CL_ASMILDIR">
<$Files "Asm\IL\pass5p2.sch"			DestDir="CL_ASMILDIR">
<$Files "Asm\IL\peepopt.sch"			DestDir="CL_ASMILDIR">
<$Files "Asm\IL\util.sch"			DestDir="CL_ASMILDIR">
<$Files "Asm\IL\util-structs.sch"		DestDir="CL_ASMILDIR">

<$Files "Asm\IL\il-corememory.sch"		DestDir="CL_ASMILDIR">
<$Files "Asm\IL\il-jdot-aliases.sch"		DestDir="CL_ASMILDIR">
<$Files "Asm\IL\il-load-coremem.sch"		DestDir="CL_ASMILDIR">

<$Files "Auxlib\format.sch"			DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\io.sch"				DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\list.sch"			DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\load.sch"			DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\macros.sch"			DestDir="CL_AUXLIBDIR">
;; "Auxlib\Makefile"
<$Files "Auxlib\misc.sch"			DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\osdep-macos.sch"		DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\osdep-unix.sch"			DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\pp.sch"				DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\record.sch"			DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\std-ffi.sch"			DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\string.sch"			DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\unix-functions.sch"		DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\vector.sch"			DestDir="CL_AUXLIBDIR">

<$Files "Auxlib\comment.sch"			DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\common-syntax.sch"		DestDir="CL_AUXLIBDIR">
<$Files "Auxlib\shivers-syntax.sch"		DestDir="CL_AUXLIBDIR">

;;<$Files "bin\Debug\Scheme.dll"			DestDir="CL_BINDBGDIR">
;;<$Files "bin\Debug\Scheme.pdb"			DestDir="CL_BINDBGDIR">
;;<$Files "bin\Release\Scheme.dll"		DestDir="CL_BINRELDIR">

<$Files "CL\files.ss"			DestDir="CL_CLDIR">
<$Files "CL\mzscheme-runner.ss"			DestDir="CL_CLDIR">
<$Files "CL\README"			DestDir="CL_CLDIR">
<$Files "CL\Scripts\AssembleHeap2.bat"		DestDir="CL_CLSCRDIR">
<$Files "CL\Scripts\CompileHeap.bat"		DestDir="CL_CLSCRDIR">
<$Files "CL\Scripts\CompileRuntime2.bat"		DestDir="CL_CLSCRDIR">
<$Files "CL\Scripts\README"			DestDir="CL_CLSCRDIR">
<$Files "CL\Scripts\which.bat"			DestDir="CL_CLSCRDIR">
<$Files "CL\VS8\Larceny\Bundle\Bundleplt.ss"			DestDir="CL_CLVS8LBDIR">
<$Files "CL\VS8\Larceny\Bundle\Bundletar.ss"			DestDir="CL_CLVS8LBDIR">
<$Files "CL\VS8\Larceny\Bundle\Bundlezip.ss"			DestDir="CL_CLVS8LBDIR">
<$Files "CL\VS8\Larceny\Bundle\bundle-common.ss"			DestDir="CL_CLVS8LBDIR">
<$Files "CL\VS8\Larceny\Bundle\Bundle.vcproj"			DestDir="CL_CLVS8LBDIR">
<$Files "CL\VS8\Larceny\Configure\Configure.ss"			DestDir="CL_CLVS8LCDIR">
<$Files "CL\VS8\Larceny\Configure\Configure.vcproj"		DestDir="CL_CLVS8LCDIR">
<$Files "CL\VS8\Larceny\Heap\Rebuild.ss"			DestDir="CL_CLVS8LHDIR">
<$Files "CL\VS8\Larceny\Heap\Heap.vcproj"			DestDir="CL_CLVS8LHDIR">
<$Files "CL\VS8\Larceny\Larceny.sln"			DestDir="CL_CLVS8LDIR">
<$Files "CL\VS8\Larceny\Msi\WindowsInstall.vdproj"		DestDir="CL_CLVS8LMDIR">
<$Files "CL\VS8\Larceny\Preprocess\Preprocess.vcproj"		DestDir="CL_CLVS8LPDIR">
<$Files "CL\VSNET\Larceny\Bundle\Bundleplt.ss"			DestDir="CL_CLVSNLBDIR">
<$Files "CL\VSNET\Larceny\Bundle\Bundletar.ss"			DestDir="CL_CLVSNLBDIR">
<$Files "CL\VSNET\Larceny\Bundle\Bundlezip.ss"			DestDir="CL_CLVSNLBDIR">
<$Files "CL\VSNET\Larceny\Bundle\Bundle.vcproj"			DestDir="CL_CLVSNLBDIR">
<$Files "CL\VSNET\Larceny\Configure\Configure.ss"			DestDir="CL_CLVSNLCDIR">
<$Files "CL\VSNET\Larceny\Configure\Configure.vcproj"		DestDir="CL_CLVSNLCDIR">
<$Files "CL\VSNET\Larceny\Heap\Rebuild.ss"			DestDir="CL_CLVSNLHDIR">
<$Files "CL\VSNET\Larceny\Heap\Heap.vcproj"			DestDir="CL_CLVSNLHDIR">
<$Files "CL\VSNET\Larceny\Larceny.sln"			DestDir="CL_CLVSNLDIR">
<$Files "CL\VSNET\Larceny\Msi\WindowsInstall.vdproj"		DestDir="CL_CLVSNLMDIR">
<$Files "CL\VSNET\Larceny\Preprocess\Preprocess.vcproj"		DestDir="CL_CLVSNLPDIR">

<$Files "Compat\Larceny\compat.sch"			DestDir="CL_COMPATLDIR">
<$Files "Compat\Larceny\compat2.sch"			DestDir="CL_COMPATLDIR">
;; "Compat\Larceny\Makefile"
<$Files "Compat\Larceny\tobytevector-be.sch"		DestDir="CL_COMPATLDIR">
<$Files "Compat\Larceny\tobytevector-el.sch"		DestDir="CL_COMPATLDIR">

<$Files "Compat\MzScheme\bytevec-el.ss"			DestDir="CL_COMPATMDIR">
<$Files "Compat\MzScheme\bytevec.ss"			DestDir="CL_COMPATMDIR">
<$Files "Compat\MzScheme\compat.sch"			DestDir="CL_COMPATMDIR">
<$Files "Compat\MzScheme\compat2.sch"			DestDir="CL_COMPATMDIR">
<$Files "Compat\MzScheme\logops.ss"			DestDir="CL_COMPATMDIR">
<$Files "Compat\MzScheme\misc2bytevector-el.ss"		DestDir="CL_COMPATMDIR">
<$Files "Compat\MzScheme\misc2bytevector.ss"		DestDir="CL_COMPATMDIR">

<$Files "Docs\HOWTO-DOTNET"				DestDir="CL_DOCSDIR">
<$Files "Docs\KNOWN-BUGS"				DestDir="CL_DOCSDIR">

<$Files "Interpreter\interp-prim.sch"		DestDir="CL_INTERPDIR">
<$Files "Interpreter\interp.sch"			DestDir="CL_INTERPDIR">
<$Files "Interpreter\macro-expand.sch"		DestDir="CL_INTERPDIR">
<$Files "Interpreter\switches.sch"			DestDir="CL_INTERPDIR">

<$Files "Lib\makefile.sch"			DestDir="CL_LIBDIR">
<$Files "Lib\Common\arith.mal"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\belle.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\bignums-be.sch"		DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\bignums-el.sch"		DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\bignums.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\command-line.sch"		DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\conio.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\contag.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\control.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\dump.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\ecodes.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\ehandler.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\env.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\error.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\error0.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\eval.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\exit.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\fileio.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\flonums-be.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\flonums-el.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\flonums.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\format.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\gcctl.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\globals.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\go.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\hash.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\hashtable.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\ioboot.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\iosys.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\javadot-symbol.sch"		DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\javadot-syntax.sch"		DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\list.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\load.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\malcode.mal"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\mcode.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\memstats.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\num2str.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\number.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\oblist.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\preds.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\print.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\procinfo.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\profile.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\ratnums.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\reader.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\rectnums.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\secret.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\sort.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\stdio.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\str2num.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\string.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\stringio.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\struct.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\sys-macos.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\sys-unix.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\sys-win32.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\syscall-id.sch"		DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\syshooks.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\sysparam.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\system-interface.sch"		DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\timer.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\toplevel.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\transio.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\typetags.sch"			DestDir="CL_LIBCOMDIR">
<$Files "Lib\Common\vector.sch"			DestDir="CL_LIBCOMDIR">

<$Files "Lib\Common\require.sch"			DestDir="CL_LIBCOMDIR">

<$Files "Lib\IL\loadable.sch"			DestDir="CL_LIBILDIR">
<$Files "Lib\IL\primops.sch"			DestDir="CL_LIBILDIR">
<$Files "Lib\IL\toplevel-target.sch"		DestDir="CL_LIBILDIR">

<$Files "Lib\MzScheme\class.sch"			DestDir="CL_LIBMZSDIR">
;; "Lib\MzScheme\ClassInCore-demo.sch"
;; "Lib\MzScheme\CodeDOM-demo.sch"
<$Files "Lib\MzScheme\compress.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\cont.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\custodian.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\dotnet-ffi.sch"		DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\dotnet.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\envaux.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\Mzscheme\excel-demo.sch"		DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\exn.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\generic.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\gprint.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\hash-compat.sch"		DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\identifier.sch"		DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\init.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\inspector.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\instance.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\instance0.mal"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\macros.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\misc.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\namespace.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\parameters.sch"		DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\record.sch"			DestDir="CL_LIBMZSDIR">

<$Files "Lib\MzScheme\simple-macros\mzmacros.sch"			DestDir="CL_LIBMZSMDIR">
<$Files "Lib\MzScheme\simple-macros\r5rs.sch"			DestDir="CL_LIBMZSMDIR">
<$Files "Lib\MzScheme\simple-macros\simple-macros-tests.sch"	DestDir="CL_LIBMZSMDIR">
<$Files "Lib\MzScheme\simple-macros\simple-macros.sch"		DestDir="CL_LIBMZSMDIR">
<$Files "Lib\MzScheme\simple-macros\simple-module-examples.sch"	DestDir="CL_LIBMZSMDIR">
<$Files "Lib\MzScheme\simple-macros\simple-syntax-case.sch"	DestDir="CL_LIBMZSMDIR">

<$Files "Lib\MzScheme\struct-macros.sch"		DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\struct-proc.sch"		DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\struct-proc0.mal"		DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\struct.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\thread.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\wcm.sch"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\wcm0.mal"			DestDir="CL_LIBMZSDIR">
<$Files "Lib\MzScheme\windows.sch"			DestDir="CL_LIBMZSDIR">

<$Files "Lib\SRFI\scheme-r5rs.c"		DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-0.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-1.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-2.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-5.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-6.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-7.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-8.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-9.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-11.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-13.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-14.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-16.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-17.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-19.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-22.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-23.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-25.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-26.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-27.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-28.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-29.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-30.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-31.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-37.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-38.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-39.sch"			DestDir="CL_LIBSRFIDIR">
<$Files "Lib\SRFI\srfi-42.sch"			DestDir="CL_LIBSRFIDIR">

<$Files "Repl\main.sch"			DestDir="CL_REPLDIR">
;; "Repl\Makefile"
<$Files "Repl\reploop.sch"			DestDir="CL_REPLDIR">

<$Files "Rts\except.cfg"			DestDir="CL_RTSDIR">
<$Files "Rts\globals-nasm.cfg"		DestDir="CL_RTSDIR">
<$Files "Rts\globals.cfg"			DestDir="CL_RTSDIR">
<$Files "Rts\layouts.cfg"			DestDir="CL_RTSDIR">
;; "Rts\Makefile"
;; "Rts\makefile.sch"
<$Files "Rts\memstats.cfg"			DestDir="CL_RTSDIR">
<$Files "Rts\mprocs.cfg"			DestDir="CL_RTSDIR">
<$Files "Rts\regs.cfg"			DestDir="CL_RTSDIR">

<$Files "Rts\Build\c-table.c"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\cdefs.h"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\except.ah"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\except.cfg"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\except.ch"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\except.sh"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\globals.ah"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\globals.cfg"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\globals.ch"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\globals.sh"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\layouts.ah"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\layouts.cfg"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\layouts.ch"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\layouts.sh"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\mprocs.ah"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\mprocs.cfg"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\mprocs.ch"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\schdefs.h"			DestDir="CL_RTSBLDDIR">
<$Files "Rts\Build\sparc-table.s"			DestDir="CL_RTSBLDDIR">

<$Files "Rts\DotNet\AssemblyInfo.cs"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Call.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\ClassicOps.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\ClassicOpsSpecial.cs"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\CodeAddress.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Constants.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\ContinuationISH.cs"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\DynLoad.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Exn.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Factory.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\FFI.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Instructions.cs"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Load.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Macros.h"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\makefile"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\MapOps.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\MapOpsSpecial.cs"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Number.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops.h"				DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops_Procedure.inc"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops_SByteVL.inc"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops_SChar.inc"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops_SFixnum.inc"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops_SImmediate.inc"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops_SObject.inc"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops_SPair.inc"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops_STagged.inc"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Ops_SVL.inc"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\OpsSpecial.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Perf.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Reg.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Scheme.csproj"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Scheme.csproj8"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\SchemeObject.cs"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\SchemeObject.cs.cpp"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Syscall-enum.cs"		DestDir="CL_RTSDNETDIR">
<$Files "Rts\DotNet\Syscall.cs"			DestDir="CL_RTSDNETDIR">
<$Files "Rts\make-templates.sch"		DestDir="CL_RTSDIR">

<$Files "Compiler\common.imp.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\copy.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\driver-common.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\driver-larceny.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\driver-twobit.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\expand.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\hash.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\hashtable.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\hashtree.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\help-topics.txt"		DestDir="CL_COMPDIR">
<$Files "Compiler\help.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\lowlevel.sch"			DestDir="CL_COMPDIR">
;; "Compiler\Makefile"
<$Files "Compiler\pass1.aux.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass1.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass2.aux.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass2if.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass2p1.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass2p2.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass3.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass3anormal.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\pass3anormal2.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\pass3callgraph.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\pass3commoning.aux.sch"	DestDir="CL_COMPDIR">
<$Files "Compiler\pass3commoning.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\pass3folding.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\pass3inlining.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\pass3rep.aux.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\pass3rep.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass4.aux.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass4let.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass4p1.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass4p2.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass4p3.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\pass4special.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\prefs.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\printlap.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\sets.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\sparc.imp.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\sparc.imp2.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\standard-C.imp.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\standard-C.imp2.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\switches.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\syntaxenv.sch"			DestDir="CL_COMPDIR">
<$Files "Compiler\syntaxrules.sch"		DestDir="CL_COMPDIR">
<$Files "Compiler\usual.sch"			DestDir="CL_COMPDIR">

<$Files "Util\bundle.sch"			DestDir="CL_UTILDIR">
<$Files "Util\config.sch"			DestDir="CL_UTILDIR">
<$Files "Util\csharp-config.scm"		DestDir="CL_UTILDIR">
<$Files "Util\dotnet.sch"			DestDir="CL_UTILDIR">
<$Files "Util\expander.sch"			DestDir="CL_UTILDIR">
<$Files "Util\heap-shake.sch"		DestDir="CL_UTILDIR">
<$Files "Util\il.scm"			DestDir="CL_UTILDIR">
<$Files "Util\init-comp.sch"			DestDir="CL_UTILDIR">
<$Files "Util\macros.gdb"			DestDir="CL_UTILDIR">
<$Files "Util\make-support.sch"		DestDir="CL_UTILDIR">
<$Files "Util\make.sch"			DestDir="CL_UTILDIR">
<$Files "Util\memstats.sch"			DestDir="CL_UTILDIR">
<$Files "Util\nasm-unix.sch"			DestDir="CL_UTILDIR">
<$Files "Util\nbuild-files.sch"		DestDir="CL_UTILDIR">
<$Files "Util\nbuild.sch"			DestDir="CL_UTILDIR">
<$Files "Util\nbuild-param.sch"			DestDir="CL_UTILDIR">
<$Files "Util\nbuild-defns.sch"		DestDir="CL_UTILDIR">
;; "Util\petit-macosx-on-win32.sch"
<$Files "Util\petit-unix-be.sch"		DestDir="CL_UTILDIR">
<$Files "Util\petit-unix-el.sch"		DestDir="CL_UTILDIR">
<$Files "Util\petit-unix-defns-globals.sch"	DestDir="CL_UTILDIR">
<$Files "Util\petit-win32.sch"		DestDir="CL_UTILDIR">
<$Files "Util\process-stats.sch"		DestDir="CL_UTILDIR">
<$Files "Util\r5rs-heap.sch"			DestDir="CL_UTILDIR">
;; "Util\std-heap.sch"
<$Files "Util\sysdep-macos.sch"		DestDir="CL_UTILDIR">
<$Files "Util\sysdep-unix.sch"		DestDir="CL_UTILDIR">
<$Files "Util\sysdep-win32.sch"		DestDir="CL_UTILDIR">
<$Files "Util\twobit-heap.sch"		DestDir="CL_UTILDIR">
<$Files "Util\cleanup.sch"			DestDir="CL_UTILDIR">
<$Files "Util\dotnet-compile-file.sch"			DestDir="CL_UTILDIR">
