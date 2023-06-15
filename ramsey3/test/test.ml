open OUnit2
open Ramsey

let c11 = Cell { color = Green; left = Nil; above = Nil }
let c12 = Cell { color = Green; left = c11; above = Nil }
let c13 = Cell { color = Blue;  left = c12; above = Nil }
let c14 = Cell { color = Green; left = c13; above = Nil }
let c15 = Cell { color = Blue;  left = c14; above = Nil }
let c21 = Cell { color = Blue;  left = Nil; above = c11 }
let c22 = Cell { color = Green; left = c21; above = c12 }
let c23 = Cell { color = Blue;  left = c22; above = c13 }
let c24 = Cell { color = Green; left = c23; above = c14 }
let c25 = Cell { color = Green; left = c24; above = c15 }
let c31 = Cell { color = Green; left = Nil; above = c21 }
let c41 = Cell { color = Green; left = Nil; above = c31 }

let d11 = Cell { color = Green; left = Nil; above = Nil }
let d12 = Cell { color = Green; left = d11; above = Nil }
let d13 = Cell { color = Green; left = d12; above = Nil }
let d21 = Cell { color = Green; left = Nil; above = d11 }
let d22 = Cell { color = Blue;  left = d21; above = d12 }
let d23 = Cell { color = Blue;  left = d22; above = d13 }
let d31 = Cell { color = Green; left = Nil; above = d21 }
let d32 = Cell { color = Blue;  left = d31; above = d22 }


let rec string_of_string_list l =
    match l with
    | [] -> ""
    | h::t -> h ^ " " ^ (string_of_string_list t)

let test0 = "string from data" >::: [
  "G" >:: ( fun _ -> assert_equal "G" (string_of_cell_hierarchy c11) ~printer:Fun.id );
  "GG" >:: ( fun _ -> assert_equal "GG" (string_of_cell_hierarchy c12) ~printer:Fun.id );
  "GGB" >:: ( fun _ -> assert_equal "GGB" (string_of_cell_hierarchy c13) ~printer:Fun.id );
  "GGBGB" >:: ( fun _ -> assert_equal "GGBGB" (string_of_cell_hierarchy c15) ~printer:Fun.id );
  "GGBGB|BGBGG" >:: ( fun _ -> assert_equal "GGBGB|BGBGG" (string_of_cell_hierarchy c25) ~printer:Fun.id );
  "G|B|G|G" >:: ( fun _ -> assert_equal "G|B|G|G" (string_of_cell_hierarchy c41) ~printer:Fun.id );
  "G|G" >:: ( fun _ -> assert_equal "G|G" (string_of_cell_hierarchy d21) ~printer:Fun.id )
]

let test1 = "build from string" >::: [
  "G" >:: ( fun _ -> assert_equal c11 (build_from_string "G") ~printer:string_of_cell_hierarchy );
  "GG" >:: ( fun _ -> assert_equal c12 (build_from_string "GG") ~printer:string_of_cell_hierarchy );
  "GGB" >:: ( fun _ -> assert_equal c13 (build_from_string "GGB") ~printer:string_of_cell_hierarchy );
  "GGBGB" >:: ( fun _ -> assert_equal c15 (build_from_string "GGBGB") ~printer:string_of_cell_hierarchy );
  "GGBGB|BGBGG" >:: ( fun _ -> assert_equal c25 (build_from_string "GGBGB|BGBGG") ~printer:string_of_cell_hierarchy );
  "G|B|G|G" >:: ( fun _ -> assert_equal c41 (build_from_string "G|B|G|G") ~printer:string_of_cell_hierarchy )
]

let test2 = "add column" >::: [
  "G" >:: ( fun _ -> assert_equal [ "GG"; "GB"] (List.map (fun x -> string_of_cell_hierarchy x) (add_column (build_from_string "G"))) ~printer:string_of_string_list );
  "GB" >:: ( fun _ -> assert_equal [ "GBG"; "GBB"] (List.map (fun x -> string_of_cell_hierarchy x) (add_column (build_from_string "GB"))) ~printer:string_of_string_list );
  "G|B|B" >:: ( fun _ -> assert_equal [ "GG|BG|BG"; "GB|BG|BG"; "GG|BB|BG"; "GB|BB|BG"; "GG|BG|BB"; "GB|BG|BB"; "GG|BB|BB"; "GB|BB|BB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_column (build_from_string "G|B|B"))) ~printer:string_of_string_list )
]

let test3 = "add row" >::: [
  "G" >:: ( fun _ -> assert_equal [ "G|G"; "G|B"] (List.map (fun x -> string_of_cell_hierarchy x) (add_row (build_from_string "G"))) ~printer:string_of_string_list );
  "GB" >:: ( fun _ -> assert_equal [ "GB|GG"; "GB|BG"; "GB|GB"; "GB|BB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_row (build_from_string "GB"))) ~printer:string_of_string_list );
  "GBB" >:: ( fun _ -> assert_equal [  "GBB|GGG"; "GBB|BGG"; "GBB|GBG"; "GBB|BBG"; "GBB|GGB"; "GBB|BGB"; "GBB|GBB"; "GBB|BBB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_row (build_from_string "GBB"))) ~printer:string_of_string_list )
]

let test4 = "is color acceptable" >::: [
  "GG|Gg" >:: ( fun _ -> assert_equal false (is_color_acceptable Green d21 d11 d12) );
  "GG|Gb" >:: ( fun _ -> assert_equal true  (is_color_acceptable Blue  d21 d11 d12) );
  "GGG|GBB|GBb" >:: ( fun _ -> assert_equal false (is_color_acceptable Green d32 d22 d23) );
  "GGG|GBB|GBb" >:: ( fun _ -> assert_equal false (is_color_acceptable Blue  d32 d22 d23) )
]

let test5 = "add acceptable column" >::: [
  "G" >:: ( fun _ -> assert_equal [ "GG"; "GB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_acceptable_column (build_from_string "G"))) ~printer:string_of_string_list );
  "GB" >:: ( fun _ -> assert_equal [ "GBG"; "GBB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_acceptable_column (build_from_string "GB"))) ~printer:string_of_string_list );
  "G|B|B" >:: ( fun _ -> assert_equal [ "GG|BG|BG"; "GB|BG|BG"; "GG|BB|BG"; "GB|BB|BG"; "GG|BG|BB"; "GB|BG|BB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_acceptable_column (build_from_string "G|B|B"))) ~printer:string_of_string_list )
]  

let test6 = "add acceptable row" >::: [
  "G" >:: ( fun _ -> assert_equal [ "G|G"; "G|B" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_acceptable_row (build_from_string "G"))) ~printer:string_of_string_list );
  "GB" >:: ( fun _ -> assert_equal [ "GB|GG"; "GB|BG"; "GB|GB"; "GB|BB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_acceptable_row (build_from_string "GB"))) ~printer:string_of_string_list );
  "GGB" >:: ( fun _ -> assert_equal [ "GGB|BGG"; "GGB|GBG"; "GGB|BBG"; "GGB|BGB"; "GGB|GBB"; "GGB|BBB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_acceptable_row (build_from_string "GGB"))) ~printer:string_of_string_list );
  "GBB" >:: ( fun _ -> assert_equal [ "GBB|GGG"; "GBB|BGG"; "GBB|GBG"; "GBB|BBG"; "GBB|GGB"; "GBB|BGB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_acceptable_row (build_from_string "GBB"))) ~printer:string_of_string_list )
]  

let test7 = "add acceptable colun and row" >::: [
  "G" >:: ( fun _ -> assert_equal [ "GG|BG"; "GG|GB"; "GG|BB"; "GB|GG"; "GB|BG"; "GB|GB"; "GB|BB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_acceptable_column_and_row (build_from_string "G"))) ~printer:string_of_string_list );
  "GG|GB" >:: ( fun _ -> assert_equal [ "GGG|GBG|BGG"; "GGG|GBG|BBG"; "GGG|GBG|GGB"; "GGG|GBG|BGB"; "GGG|GBG|GBB"; "GGG|GBG|BBB"; "GGB|GBG|GGG"; "GGB|GBG|BGG"; "GGB|GBG|GBG"; "GGB|GBG|BBG"; "GGB|GBG|GGB"; "GGB|GBG|BGB"; "GGB|GBG|GBB"; "GGB|GBG|BBB"; "GGG|GBB|BGG"; "GGG|GBB|BBG"; "GGG|GBB|GGB"; "GGG|GBB|BGB"; "GGB|GBB|GGG"; "GGB|GBB|BGG"; "GGB|GBB|GBG"; "GGB|GBB|BBG"; "GGB|GBB|GGB"; "GGB|GBB|BGB" ] (List.map (fun x -> string_of_cell_hierarchy x) (add_acceptable_column_and_row (build_from_string "GG|GB"))) ~printer:string_of_string_list )
]  

let _ = run_test_tt_main test0
let _ = run_test_tt_main test1
let _ = run_test_tt_main test2
let _ = run_test_tt_main test3
let _ = run_test_tt_main test4
let _ = run_test_tt_main test5
let _ = run_test_tt_main test6
let _ = run_test_tt_main test7
