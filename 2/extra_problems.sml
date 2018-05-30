fun alternate (num_list: int list) =
  let fun alternate_inner (nums, alt) =
    if null nums
    then 0
    else
      if alt
      then alternate_inner(tl nums, not alt) - hd nums 
      else alternate_inner(tl nums, not alt) + hd nums 
  in
    alternate_inner(num_list, false)
  end

fun min_max (num_list) =
  let
    fun min_max_int(nums, min, max) =
      if null nums
      then (min, max)
      else
        if hd nums > max
        then min_max_int(tl nums, min, hd nums)
        else if hd nums < min
        then min_max_int(tl nums, hd nums, max)
        else min_max_int(tl nums, min, max)
  in
    min_max_int(num_list, hd num_list, hd num_list)
  end

fun cumsum (num_list: int list) =
  let fun cumsum_int(nums, sumsofar) =
    if null nums
    then []
    else hd nums + sumsofar :: cumsum_int(tl nums, hd nums + sumsofar)
  in
    cumsum_int(num_list, 0)
  end

fun greeting (name: string option) =
  let fun get_name (name: string option) =
    if isSome name
    then valOf name
    else "you"
  in  
    "Hello there, " ^ get_name name ^ "!"
  end

fun repeat (list1, list2) =
  let
    fun append_x_times (number, x) =
      if x = 0
      then []
        else number :: append_x_times(number, x - 1)
    and repeat_internal (list1, list2) =
      if null list1
      then []
      else
        append_x_times (hd list1, hd list2) @ repeat_internal (tl list1, tl list2)
  in
    repeat_internal(list1, list2)
  end

fun addOpt (opt1, opt2) =
  if isSome opt1 andalso isSome opt2
  then SOME (valOf opt1 + valOf opt2)
  else NONE
