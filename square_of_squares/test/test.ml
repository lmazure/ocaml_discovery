open OUnit2
open Square_of_squares

let test0 = "string from data" >::: [
  "dummy" >:: ( fun _ -> assert_equal ( -2, 6) (min_max_list [ 1; 2; 4; -1; -2; 5; 6; -1;0]))
]

let _ = run_test_tt_main test0


let _ = Random.init (int_of_string Sys.argv.(1))

let a = ref (
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999
)


let _ = print_endline "--- optimize ---"
let previous_score = ref Int.max_int
let current_score = ref (score !a)
let _ =
  while (current_score < previous_score) do
    a := improve3 !a;
    previous_score := !current_score;
    current_score := score !a;
  done 
let _ = print_description !a
let _ = print_endline (string_of_int !current_score)
let _ = print_newline ()


let _ = print_endline "--- scale ---"
let _ = a := scale_square !a 1.1
(* let _ = print_description !a *)

let _ = print_endline "--- optimize ---"
let previous_score = ref Int.max_int
let current_score = ref (score !a)
let _ =
  while (current_score < previous_score) do
    a := improve3 !a;
    previous_score := !current_score;
    current_score := score !a;
  done
let _ = print_description !a
let _ = print_endline (string_of_int !current_score)
let _ = print_newline ()
