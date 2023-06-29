open OUnit2
open Square_of_squares

let test0 = "string from data" >::: [
  "dummy" >:: ( fun _ -> assert_equal ( -2, 6) (min_max_list [ 1; 2; 4; -1; -2; 5; 6; -1;0]))
]

let _ = run_test_tt_main test0
