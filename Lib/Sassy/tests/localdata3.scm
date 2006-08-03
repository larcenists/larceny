(export _global_offset_table_ foo)

(data
  (label foo
    (locals (foo)
      (dwords "abcd" #\newline foo)
      (label foo
	(dwords "defg" #\newline)))))




