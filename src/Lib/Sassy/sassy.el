; sassy.el - emacs minor mode for editing sassy files
; Copyright (C) 2006 Jonathan Kraut

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



; This is a minor mode extension to scheme-mode called sassy-mode It
; only provides some additional syntax highlighting and indentation
; for sassy keywords.  Files suffixed with a ".sassy" extension will
; autoload this mode
;
;
; INSTALLATION
; ============
;
; 1) Put this file in the appropriate place
;    (someplace in your emacs's load-path).
;    (For help with load-paths do:  M-x apropos load-path
;
; 2) Add the following two lines to your .emacs file, and uncomment them
;    there
;
;    (add-to-list 'auto-mode-alist '("\\.sassy$" . sassy-mode))
;    (autoload 'sassy-mode "sassy" "Minor mode for editing Sassy files" t)
;
; 3) Start up yo 'macs
;
; All files that end with the extension ".sassy" will activate this
; minor mode. Since it's built on top of scheme-mode any scheme code
; will still highlight as well.

(defvar sassy-extra-syntax
  '((seq 0)
    (begin 0)
    (inv 0)
    (if 0)
    (iter 0)
    (while 1)
    (esc 1)
    (mark 0)
    (leap 0)
    (with-win 1)
    (with-lose 1)
    (with-win-lose 2)
    (alt 0)
    (times 1)
    (until 1)
    (unless 1)
    (while 1)
    (case 1)
    (heap 0)
    (data 0)
    (text 0)
    (import 0)
    (export 0)
    (include 0)
    (direcs 0)
    (entry 0)
    (org 0)
    (! 0)
    (align 0)))

(put 'label 'scheme-indent-function 1)
(put 'macro 'scheme-indent-function 1)
(put 'lazy-macro 'scheme-indent-function 1)
(put 'let-macro 'scheme-indent-function 1)
(put 'let-lazy-macro 'scheme-indent-function 1)
(put 'locals 'scheme-indent-function 1)

(define-derived-mode sassy-mode scheme-mode "sassy"
  (dolist (syn sassy-extra-syntax)
	  (put (car syn) 'scheme-indent-function (cadr syn)))
  (font-lock-add-keywords
   'sassy-mode
   (eval-when-compile
     (list
      (cons (concat "(" (regexp-opt (mapcar (lambda (x)
					      (symbol-name (car x)))
					    sassy-extra-syntax) t)
		    "\\>")
	    1)
      (list (concat "("
		    "\\(label\\)"
		    " *?"
		    "\\(\\sw+\\)"
		    "\\>")
	    '(1 font-lock-constant-face)
	    '(2 font-lock-function-name-face))
      (list (concat "(" (regexp-opt '("locals") t)
		    "\\>")
	    '(1 font-lock-constant-face))
      (list (concat "("
		    "\\(macro\\)"
		    " *?"
		    "\\(\\sw+\\)"
		    "\\>")
	    '(1 font-lock-keyword-face)
	    '(2 font-lock-variable-name-face))))))
