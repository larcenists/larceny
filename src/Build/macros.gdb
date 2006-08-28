set width 0

# argument: a dof_data_t pointer variable or pointer
define print_gens
  print *$arg0->gen[0]
  print *$arg0->gen[1]
  print *$arg0->gen[2]
  print *$arg0->gen[3]
  print *$arg0->gen[4]
  print *$arg0->gen[5]
end

# argument: a los_t pointer variable or pointer
define print_los
  set $i=0
  while $i < $arg0->generations
    print *($arg0->object_lists[$i])
    set $i=$i+1
  end
end

# argument: an address in the heap range
define gen_of 
  print gclib_desc_g[ ((unsigned)$arg0 - (unsigned)gclib_pagebase) >> 12 ]
end

# argument: an address in the heap range
define attrib_of
  print gclib_desc_b[ ((unsigned)$arg0 - (unsigned)gclib_pagebase) >> 12 ]
end
