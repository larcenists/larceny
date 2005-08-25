@REM  Rebuild the CommonLarceny runtime
@REM
@SETLOCAL
@pushd %~d0%~p0

@cd ..\VS8\Larceny\Heap
@MzScheme -fGmqv Rebuild.ss
@popd
@ENDLOCAL
