(export _global_offset_table_)

(import say-hello)

(entry _start)

(text (label _start (jmp (plt say-hello))))


