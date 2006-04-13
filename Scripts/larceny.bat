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
) else (
    echo Usage:
    echo     petit LARCENYOPTIONS
    echo     twobit LARCENYOPTIONS
    exit 1
)

REM Shouldn't get here:
exit 1

:check

if not exist %1 (
    echo Not found: %1 >&2
    exit 1
) else if not exist %3 (
    echo Not found: %3 >&2
    exit 1
)

%*
exit 0

