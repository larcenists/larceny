; Lib/ioboot.sch
; Larceny -- i/o system boot code
;
; $Id: ioboot.sch,v 1.2 1997/03/05 19:28:51 lth Exp $
;
; This code is loaded in the bootstrap heap image immediately following
; the I/O system.  The call to 'initialize-io-system' opens the console
; input and output ports.  In a dumped heap image, the initialization call
; must be made explicitly by the initial thunk.

(initialize-io-system)

; eof
