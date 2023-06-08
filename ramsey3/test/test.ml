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
  | Cell x -> let p = (data_to_string x.above) in
                (if (p = "") then "" else (p ^ "|")) ^ (row_to_string c)

let rec string_of_string_list l =
    match l with
    | [] -> ""
    | h::t -> h ^ " " ^ (string_of_string_list t)

let test0 = "string from data" >::: [
  "G" >:: ( fun _ -> assert_equal "G" (data_to_string c11) ~printer:Fun.id );
  "GG" >:: ( fun _ -> assert_equal "GG" (data_to_string c12) ~printer:Fun.id );
  "GGB" >:: ( fun _ -> assert_equal "GGB" (data_to_string c13) ~printer:Fun.id );
  "GGBGB" >:: ( fun _ -> assert_equal "GGBGB" (data_to_string c15) ~printer:Fun.id );
  "GGBGB|BGBGG" >:: ( fun _ -> assert_equal "GGBGB|BGBGG" (data_to_string c25) ~printer:Fun.id );
  "G|B|G|G" >:: ( fun _ -> assert_equal "G|B|G|G" (data_to_string c41) ~printer:Fun.id )
]

let test1 = "build from string" >::: [
  "G" >:: ( fun _ -> assert_equal c11 (build_from_string "G") ~printer:data_to_string );
  "GG" >:: ( fun _ -> assert_equal c12 (build_from_string "GG") ~printer:data_to_string );
  "GGB" >:: ( fun _ -> assert_equal c13 (build_from_string "GGB") ~printer:data_to_string );
  "GGBGB" >:: ( fun _ -> assert_equal c15 (build_from_string "GGBGB") ~printer:data_to_string );
  "GGBGB|BGBGG" >:: ( fun _ -> assert_equal c25 (build_from_string "GGBGB|BGBGG") ~printer:data_to_string );
  "G|B|G|G" >:: ( fun _ -> assert_equal c41 (build_from_string "G|B|G|G") ~printer:data_to_string )
]

let test2 = "add column" >::: [
  "G" >:: ( fun _ -> assert_equal [ "GG"; "GB"] (List.map (fun x -> data_to_string x) (add_column (build_from_string "G"))) ~printer:string_of_string_list );
  "GB" >:: ( fun _ -> assert_equal [ "GBG"; "GBB"] (List.map (fun x -> data_to_string x) (add_column (build_from_string "GB"))) ~printer:string_of_string_list );
  "G|B|B" >:: ( fun _ -> assert_equal [ "GG|BG|BG"; "GB|BG|BG"; "GG|BB|BG"; "GB|BB|BG"; "GG|BG|BB"; "GB|BG|BB"; "GG|BB|BB"; "GB|BB|BB" ] (List.map (fun x -> data_to_string x) (add_column (build_from_string "G|B|B"))) ~printer:string_of_string_list )
]

let test3 = "add row" >::: [
  "G" >:: ( fun _ -> assert_equal [ "G|G"; "G|B"] (List.map (fun x -> data_to_string x) (add_row (build_from_string "G"))) ~printer:string_of_string_list );
  "GB" >:: ( fun _ -> assert_equal [ "GB|GG"; "GB|BG"; "GB|GB"; "GB|BB" ] (List.map (fun x -> data_to_string x) (add_row (build_from_string "GB"))) ~printer:string_of_string_list );
  "GBB" >:: ( fun _ -> assert_equal [  "GBB|GGG"; "GBB|BGG"; "GBB|GBG"; "GBB|BBG"; "GBB|GGB"; "GBB|BGB"; "GBB|GBB"; "GBB|BBB" ] (List.map (fun x -> data_to_string x) (add_row (build_from_string "GBB"))) ~printer:string_of_string_list )
]

let _ = run_test_tt_main test0
let _ = run_test_tt_main test1
let _ = run_test_tt_main test2
let _ = run_test_tt_main test3
