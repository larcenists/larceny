@REM  Rebuild the CommonLarceny runtime
@REM  Framework version 2.0 command line options
@SETLOCAL
@pushd %~d0%~p0

@REM find out where the framework directory is through a cheesy hack
@FOR /F "usebackq delims==" %%i IN (`which.bat csc.exe`) DO @set FRAMEWORKPATH=%%i
@cd ..\..\Rts\Dotnet

@REM  Build the debug version.
%FRAMEWORKPATH%Csc.exe /noconfig /nologo /unsafe- /checked+ /warn:4 /baseaddress:285212672 /define:DEBUG;HAS_PERFORMANCE_COUNTERS;HAS_OSVERSION;HAS_WINDOWS_FORMS /reference:%FRAMEWORKPATH%System.Data.dll /reference:%FRAMEWORKPATH%System.dll /reference:%FRAMEWORKPATH%System.Windows.Forms.dll /reference:%FRAMEWORKPATH%System.Xml.dll /debug+ /filealign:4096 /optimize- /out:..\..\bin\Debug\Scheme.dll /target:library /warnaserror- AssemblyInfo.cs Call.cs CodeAddress.cs Constants.cs ContinuationISH.cs Exn.cs Factory.cs FFI.cs Instructions.cs Load.cs Number.cs Ops.cs OpsSpecial.cs Reg.cs SchemeObject.cs Syscall.cs Syscall-enum.cs

@REM  Build the release version.
%FRAMEWORKPATH%Csc.exe /noconfig /nologo /unsafe- /checked+ /warn:4 /baseaddress:285212672 /define:HAS_OSVERSION;HAS_WINDOWS_FORMS /reference:%FRAMEWORKPATH%System.Data.dll /reference:%FRAMEWORKPATH%System.dll /reference:%FRAMEWORKPATH%System.Windows.Forms.dll /reference:%FRAMEWORKPATH%System.Xml.dll /debug- /filealign:4096 /optimize+ /out:..\..\bin\Release\Scheme.dll /target:library /warnaserror- AssemblyInfo.cs Call.cs CodeAddress.cs Constants.cs ContinuationISH.cs Exn.cs Factory.cs FFI.cs Instructions.cs Load.cs Number.cs Ops.cs OpsSpecial.cs Reg.cs SchemeObject.cs Syscall.cs Syscall-enum.cs

@popd
@ENDLOCAL
