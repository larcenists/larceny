# Compilation commands for FFI test C code.
#
# $Id$

default:
	@echo "Choose sunos4, sunos5, linux, macosx, win32"

clean:
	rm -f ffi-test-ff.so std-ffi-test-ff.so abi-test-ff.so *.dylib

cleanwin:
	del *.dll
	del *.obj
	del *.pdb
	del *.ilk

sunos4: clean
	gcc -fPIC -shared ffi-test-ff.c -o ffi-test-ff.so
	gcc -fPIC -shared std-ffi-test-ff.c -o std-ffi-test-ff.so

sunos5: clean
	gcc -fPIC -shared ffi-test-ff.c -o ffi-test-ff.so
	gcc -fPIC -shared std-ffi-test-ff.c -o std-ffi-test-ff.so

linux: clean
	gcc -fPIC -shared ffi-test-ff.c -o ffi-test-ff.so
	gcc -fPIC -shared std-ffi-test-ff.c -o std-ffi-test-ff.so
	gcc -fPIC -shared abi-test-ff.c -o abi-test-ff.so

macosx: clean
	cc -shared ffi-test-ff.c -o ffi-test-ff.dylib
	cc -shared std-ffi-test-ff.c -o std-ffi-test-ff.dylib
	cc -shared abi-test-ff.c -o abi-test-ff.dylib

win32:
	cl /LD /Zi /Zp4 /DWIN32 std-ffi-test-ff.c /link /def:std-ffi-test.def
