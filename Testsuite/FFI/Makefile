# Compilation commands for FFI test C code.
#
# $Id$

default:
	@echo "Choose sunos4, sunos5"

clean:
	rm ffi-test-ff.so std-ffi-test-ff.so

sunos4: clean
	gcc -fPIC -shared ffi-test-ff.c -o ffi-test-ff.so
	gcc -fPIC -shared std-ffi-test-ff.c -o std-ffi-test-ff.so

sunos5: clean
	gcc -fPIC -shared ffi-test-ff.c -o ffi-test-ff.so
	gcc -fPIC -shared std-ffi-test-ff.c -o std-ffi-test-ff.so

