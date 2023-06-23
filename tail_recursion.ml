let rec sum n =
  if n = 0 then 0 else n + sum (n - 1)

let sum_tr n =
  let rec sum_aux n acc = if n = 0 then acc else sum_aux (n - 1) (acc + n)
  in
  sum_aux n 0

let s = sum_tr 1000000

let _ = print_endline (string_of_int s)
