(require 'std-ffi)
(require 'foreign-sugar)

;; The redbook says:
;; Suffix	Data type   	Typical C type	OpenGL type
;; ======	============	==============	==========================
;;      b	8-bit int   	signed char   	GLbyte
;;      s	16-bit int  	short         	GLshort
;;      i	32-bit int  	int or long   	GLint, GLsizei
;;      f	32-bit float	float         	GLfloat, GLclampf
;;      d	64-bit float	double        	GLdouble, GLclampd
;;     ub	8-bit uint  	unsigned char 	GLubyte, GLboolean
;;     us	16-bit uint 	unsigned short	GLushort
;;     ul	32-bit uint 	unsigned int  	GLuint, GLenum, GLbitfield

(define-foreign (gl-viewport int int int int) void)
(define-foreign (gl-matrix-mode gl-matrix-mode-enum) void)
(define-foreign (gl-shade-model gl-shading-model-enum) void)
(define-foreign (gl-clear-color float float float float) void)
(define-foreign (gl-clear-depth double) void)

(define-foreign (gl-enable gl-enable-mode) void)
(define-foreign (gl-disable gl-enable-mode) void)
(define-foreign (gl-is-enabled gl-enable-mode) bool)

(define-foreign (gl-depth-func uint) void)
(define-foreign (gl-hint uint uint) void)
;(define-foreign (gl-clear unsigned) void)
(define-foreign (gl-clear gl-clear-buffer-bits) void)
(define-foreign (gl-load-identity) void)
(define-foreign (gl-translatef float float float) void)
(define-foreign (gl-begin gl-begin-mode) void)
;; valid commands in such a scope: 
;; glVertex*, glColor*, glIndex*, glNormal*,  glTexCoord*, 
;; glMultiTexCoord*ARB, glEdgeFlag*, glMaterial*, glArrayElement, 
;; glEvalCoord*, glEvalPoint*, glCallList, glCallLists
(define-foreign (gl-end) void)
(define-foreign (gl-flush) void)
(define-foreign (gl-color3f float float float) void)
(define-foreign (gl-rotatef float float float float) void)
(define-foreign (gl-rotated double double double double) void)
(define-foreign (gl-push-matrix) void)
(define-foreign (gl-pop-matrix) void)

(define-foreign (gl-ortho double double double double double double) void)

;; (define-gl-function-family (gl-foo (s d) type type) void)
;; => (begin (define-foreign (gl-foos short short) void)
;;           (define-foreign (gl-food double double) void))
;;
;; <exp>  ::= (define-gl-function-family (<id> (<t> ...) <type> ...) <type>)
;; <t>    ::= <ct>[<vsuf>]
;; <ct>   ::= b | s | i | f | d | ub | us | ul
;; <vsuf> ::= v
;; <type> ::= type | <ffi-attribute>

(define-syntax define-gl-function-family
  (transformer 
   (lambda (exp ren cmp)
     (define (suffix->ffi-type suffix)
       (case suffix
         ((b)  'char)  ((s)  'short)  ((i)   'int) 
         ((f) 'float) ((d) 'double)
         ((ub) 'uchar) ((us) 'ushort) ((ul) 'uint)
         ((bv) 'char*) ((sv) 'short*) ((iv) 'int*) 
         ((fv) 'float*) ((dv) 'double*)
         ;; TODO: ubv, usv, ulv...
         ))
     (define (subst old new t)
       (let rec ((t t))
         (cond ((cmp t old) new)
               ((pair? t) (cons (rec (car t)) (rec (cdr t))))
               ((vector? t) (list->vector (rec (vector->list t))))
               (else t))))
     (define (generate-define-foreign name suffix arg-types ret-type)
       (let ((new-name (string->symbol
                        (string-append (symbol->string name)
                                       (symbol->string suffix))))
             (new-arg-types (subst 'type (suffix->ffi-type suffix)
                                   arg-types))
             (new-ret-type (subst 'type (suffix->ffi-type suffix)
                                  ret-type)))
         `(,(ren 'define-foreign) (,new-name ,@new-arg-types) ,new-ret-type)))

     (let ((signature (cadr exp))
           (ret-type (caddr exp)))
       (let ((name-root-sym (car signature))
             (gl-suffixes (cadr signature))
             (arg-types (cddr signature)))
         `(,(ren 'begin)
           ,@(map (lambda (suffix)
                    (generate-define-foreign name-root-sym
                                             suffix
                                             arg-types
                                             ret-type))
                  gl-suffixes)))))))

(define-gl-function-family (gl-rect (s i f d) type type type type) void)
(define-gl-function-family (gl-rect (sv iv fv dv) type type) void)

(define-gl-function-family (gl-vertex2 (s i f d) type type) void)
(define-gl-function-family (gl-vertex3 (s i f d) type type type) void)
(define-gl-function-family (gl-vertex4 (s i f d) type type type type) void)
(define-gl-function-family (gl-vertex2 (sv iv fv dv) type) void)
(define-gl-function-family (gl-vertex3 (sv iv fv dv) type) void)
(define-gl-function-family (gl-vertex4 (sv iv fv dv) type) void)

;; XXX don't use uint; go to Appendix B to find list of queriable params
(define-foreign (gl-get-booleanv uint bool*) void) ; XXX
(define-foreign (gl-get-integerv uint int*) void)
(define-foreign (gl-get-floatv uint float*) void)
(define-foreign (gl-get-doublev uint double*) void)
(define-foreign (gl-get-pointerv uint void**) void)

(define-foreign (gl-finish) void)

(define-foreign (gl-point-size float) void)

(define-foreign (gl-line-width float) void)
(define-foreign (gl-line-stipple int ushort) void)

(define-foreign (gl-polygon-mode gl-material-face gl-polygon-mode-enum) void)
(define-foreign (gl-front-face gl-front-face-direction) void)
(define-foreign (gl-cull-face gl-cull-face-mode) void)
(define gl-polygon-stipple
  (let ((ubyte*-version
         (let ()
           (define-foreign (gl-polygon-stipple ubyte*) void)
           gl-polygon-stipple))
        (boxed-version ;; yeah, I'm evil (and lazy).
         (let ()
           (define-foreign (gl-polygon-stipple boxed) void)
           gl-polygon-stipple)))
    (lambda (arg)
      (if (bytevector? arg)
          (boxed-version arg)
          (ubyte*-version arg)))))

(define-foreign (gl-edge-flag bool) void)
(define-foreign (gl-edge-flagv bool*) void)

(define-gl-function-family (gl-normal3 (b s i d f) type type type) void)
(define-gl-function-family (gl-normal3 (bv sv iv dv fv) type) void)

(define-foreign (gl-enable-client-state gl-client-array-type) void)
(define-foreign (gl-disable-client-state gl-client-array-type) void)

;; (would be nice if below dynamically checked consistency of
;;  pointer-type and the particular void* subtype)
(define-foreign (gl-vertex-pointer int gl-vertex-pointer-type int void*) void)
(define-foreign (gl-color-pointer int gl-color-pointer-type int void*) void)
(define-foreign (gl-index-pointer gl-index-pointer-type int void*) void)
(define-foreign (gl-normal-pointer gl-normal-pointer-type int void*) void)
(define-foreign (gl-tex-coord-pointer int gl-tex-coord-pointer-type int void*) 
  void)
(define-foreign (gl-edge-flag-pointer int void*) void)

(define-foreign (gl-array-element int) void)
(define-foreign (gl-draw-elements gl-begin-mode int gl-draw-elements-type void*)
  void)
(define-foreign (gl-draw-range-elements gl-begin-mode uint uint int 
                                        gl-draw-elements-type void*) void)
(define-foreign (gl-draw-arrays gl-begin-mode int int) void)
(define-foreign (gl-interleaved-arrays gl-interleaved-arrays-enum int void*) 
  void)

(define-foreign (gl-push-attrib gl-attrib-bits) void)
(define-foreign (gl-pop-attrib) void)

(define-foreign (gl-push-client-attrib gl-client-attrib-bits) void)
(define-foreign (gl-pop-client-attrib) void)

(define-gl-function-family 
  (gl-light (i f) gl-light-name gl-light-parameter type) void)
(define-gl-function-family 
  (gl-light (iv fv) gl-light-name gl-light-parameter type) void)
(define-gl-function-family
  (gl-light-model (i f) gl-light-model-parameter type) void)
(define-gl-function-family
  (gl-light-model (iv fv) gl-light-model-parameter type) void)

;;; *** libGLU stuff ***
(define-foreign (glu-perspective double double double double) void)
(define-foreign (glu-ortho-2-d double double double double) void)
(define glu-ortho-2d glu-ortho-2-d) ; bleah
