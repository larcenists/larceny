@REM    This bat file takes an executable name and returns the
@REM    directory in $PATH where it is found.
@SETLOCAL
@SET answer=%~dp$PATH:1
@IF "%answer%"=="" (
    GOTO :BAD
)
@echo %answer%
@ENDLOCAL
@EXIT /B 0
:bad
@echo %1 not found.
@ENDLOCAL
@EXIT /B 1

