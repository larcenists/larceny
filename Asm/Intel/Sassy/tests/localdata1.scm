(export foo)

(data
  (label foo
    (dwords "abcd" #\newline)
    (locals (foo)
      (label foo
	(dwords "defg" #\newline foo)))
    (dwords foo)))
