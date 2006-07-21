@echo off
REM vim: fileformat=dos :

REM This script tries to find the Larceny binaries and to select the correct
REM pair.  If LARCENY_ROOT is set, it looks there; otherwise, if the directory
REM this script is in is named Scripts, it looks in the parent; otherwise, it
REM looks in the same directory as itself.

REM You can specify a particular LARCENY_ROOT here:
REM set LARCENY_ROOT=/usr/local/lib/larceny

setlocal

if not defined LARCENY_ROOT (
    set LARCENY_ROOT=%~dp0
)

if %~n0 == petit (
    call :check "%LARCENY_ROOT%\petit.bin.exe" -heap "%LARCENY_ROOT%\petit.heap" %*
) else if %~n0 == twobit (
    call :check "%LARCENY_ROOT%\twobit.bin.exe" -heap "%LARCENY_ROOT%\twobit.heap" %*
) else if %~n0 == larceny (
    call :check "%LARCENY_ROOT%\larceny.bin.exe" -heap "%LARCENY_ROOT%\larceny.heap" %*
) else if %~n0 == larceny-r5rs (
    call :check "%LARCENY_ROOT%\larceny.bin.exe" -heap "%LARCENY_ROOT%\r5rs.heap" %*
) else (
    echo Usage:
    echo     petit LARCENYOPTIONS
    echo     twobit LARCENYOPTIONS
    echo     larceny LARCENYOPTIONS
    goto :EOF
)

goto :EOF

:check

if not exist %1 (
    echo Not found: %1 >&2
    goto :EOF
) else if not exist %3 (
    echo Not found: %3 >&2
    goto :EOF
)

%*

