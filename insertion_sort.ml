(* insert value "value" in sorted list "list" returning a sorted list *)
let rec insert_value_in_sorted_list value list =
  if (list = [])
  then [ value ]
  else if (value < List.hd list)
  then value :: list
  else (List.hd list) :: (insert_value_in_sorted_list value (List.tl list))

(* sort a list *)
let rec sort_list list =
  if (list = [])
  then []
  else insert_value_in_sorted_list (List.hd list) (sort_list (List.tl list))
