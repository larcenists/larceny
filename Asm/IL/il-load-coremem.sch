(parameterize ((recognize-javadot-symbols? #f))
  (load "Asm/IL/il-jdot-aliases.sch"))
(parameterize ((recognize-javadot-symbols? #t))
  (load "Asm/IL/il-corememory.sch"))
