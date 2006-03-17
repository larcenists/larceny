; sassy.scm - load file for Sassy
; Copyright (C) 2005 Jonathan Kraut

; This library is free software; you can redistribute it and/or
; modify it under the terms of the GNU Lesser General Public
; License as published by the Free Software Foundation; either
; version 2.1 of the License, or (at your option) any later version.

; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Lesser General Public License for more details.

; You should have received a copy of the GNU Lesser General Public
; License along with this library; if not, write to the Free Software
; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

; Contact:
; Jonathan Kraut
; 4130 43 ST #C2
; Sunnyside, NY 11104
; jak76@columbia.edu

; see file COPYING in the top of Sassy's distribution directory


; *****************************************
; USER CONFIGURATION. UNCOMMENT ONE OF THE 
; FOLLOWING AND COMMENT OUT THE OTHERS
; *****************************************

; MZSCHEME 299.400
; (load "inits/mzscheme-299.400.scm")

; CHICKEN/CSI 2.2 with libffi, syntax-case and numbers eggs
; (load "inits/csi-2.2-libffi.scm")
; or go grap the EGG! and ignore this file, with:
; chicken-setup sassy

; GAUCHE-0.8.5
; (load "inits/gosh-0.8.5.scm")
; start gosh with "gosh -I."

; SCHEME48-1.3 - NO NEED TO MESS WITH THIS FILE!!
; start scheme48 with a nice big heap "scheme48 -h 6000000", then do
; ,config ,load sassy-48.scm
; ,open sassy

; SCM-5e1 with SLIB 3a2
; (load "inits/scm-5e1.scm")

; GUILE-1.7.91
; (load "inits/guile-1.7.91.scm")

; *****************************************
; END USER CONFIGURATION. YOU DON'T HAVE TO 
; CHANGE ANYTHING BELOW THIS POINT.
; *****************************************

;==============;
; 	       ;
; Sassy	       ;
; 	       ;
;==============;
(load "extras.scm")
(load "meta-lambda.scm")
(load "push-stacks.scm")
(load "api.scm")
(load "intern.scm")
(load "macros.scm")
(load "numbers.scm")
(load "other/srfi-56-pieces.scm")
(load "operands.scm")
(load "text-block.scm")
(load "opcodes.scm")
(load "text.scm")
(load "parse.scm")
(load "main.scm")
;=======================;
; 		        ;
; Output Modules        ;
; 		        ;
;=======================;
(load "flat-bin.scm")
(load "elf.scm")

;========================;
; 			 ;
; The test suite	 ;
; 			 ;
;========================;
; eval the following two expressions in order to run all the tests.

; (load "tests/run-tests.scm")
; or in scheme48
; ,open sassy-tests
; (sassy-run-tests 'all)

