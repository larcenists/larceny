(require 'gtk)

(define-foreign (gtk-check-menu-item-set-draw-as-radio gtkcheckmenuitem* bool) void)
(define-foreign (gtk-check-menu-item-get-draw-as-radio gtkcheckmenuitem*) bool)

(define-foreign (gtk-toolbar-insert gtktoolbar* gtktoolitem* int) void)
(define-foreign (gtk-toolbar-get-item-index gtktoolbar* gtktoolitem*) int)
(define-foreign (gtk-toolbar-get-n-items gtktoolbar*) int)
(define-foreign (gtk-toolbar-get-nth-item gtktoolbar* int) gtktoolitem*)
(define-foreign (gtk-toolbar-get-show-arrow gtktoolbar*) bool)
(define-foreign (gtk-toolbar-set-show-arrow gtktoolbar* bool) void)


(define-foreign (gtk-combo-box-new) gtkcombobox*)
(define-foreign (gtk-combo-box-new-text) gtkcombobox*)
(define-foreign (gtk-combo-box-append-text gtkcombobox* string) void)
(define-foreign (gtk-combo-box-insert-text gtkcombobox* int string) void)
(define-foreign (gtk-combo-box-prepend-text gtkcombobox* string) void)
(define-foreign (gtk-combo-box-remove-text gtkcombobox* int) void)
(define-foreign (gtk-combo-box-get-active gtkcombobox*) void)
(define-foreign (gtk-combo-box-set-active gtkcombobox* int) void)
