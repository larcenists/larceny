(data
  (label foo
    (dwords 100 200 300 (reloc abs $here 4))))

(text
  (label bar
    (esc ((push $win))
      (seq (nop)
	   (nop)))))
