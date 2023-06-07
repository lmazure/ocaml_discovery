open OUnit2
open Ramsey

let c11 = Cell { color = Green; left = Nil; above = Nil }
let c12 = Cell { color = Green; left = c11; above = Nil }
let c13 = Cell { color = Blue;  left = c12; above = Nil }
let c14 = Cell { color = Green; left = c13; above = Nil }
let c15 = Cell { color = Blue;  left = c14; above = Nil }
let c21 = Cell { color = Blue; left = Nil; above = c11 }
let c22 = Cell { color = Green; left = c21; above = c12 }
let c23 = Cell { color = Blue;  left = c22; above = c13 }
let c24 = Cell { color = Green; left = c23; above = c14 }
let c25 = Cell { color = Green;  left = c24; above = c15 }
let c31 = Cell { color = Green; left = Nil; above = c21 }
let c41 = Cell { color = Green; left = Nil; above = c31 }

let rec row_to_string (c: cell) : string =
  match c with
  | Nil -> ""
  | Cell x -> (row_to_string x.left) ^ (color_to_string x.color)

let rec data_to_string (c: cell) : string =
  match c with
  | Nil -> ""
  | Cell x -> (data_to_string x.above) ^ "|" ^ (row_to_string c)

let test1 = "build from string" >::: [
  "G" >:: ( fun _ -> assert_equal c11 (build_from_string "G") ~printer:data_to_string);
  "GG" >:: ( fun _ -> assert_equal c12 (build_from_string "GG") ~printer:data_to_string);
  "GGB" >:: ( fun _ -> assert_equal c13 (build_from_string "GGB") ~printer:data_to_string);
  "GGBGB" >:: ( fun _ -> assert_equal c15 (build_from_string "GGBGB") ~printer:data_to_string);
  "GGBGB|BGBGG" >:: ( fun _ -> assert_equal c25 (build_from_string "GGBGB|BGBGG") ~printer:data_to_string);
  "G|B|G|G" >:: ( fun _ -> assert_equal c41 (build_from_string "G|B|G|G") ~printer:data_to_string)
]

let _ = run_test_tt_main test1

