open OUnit2
open Square_of_squares

let test0 = "string from data" >::: [
  "dummy" >:: ( fun _ -> assert_equal ( -2, 6) (min_max_list [ 1; 2; 4; -1; -2; 5; 6; -1;0]))
]

let _ = run_test_tt_main test0


let _ = Random.self_init ()

let a = ref (
  Random.int 999,
  Random.int 999,
  Random.int 999,
  Random.int 999,
  Random.int 999,
  Random.int 999,
  Random.int 999,
  Random.int 999,
  Random.int 999
)

let previous_score = ref Int.max_int
let current_score = ref (score !a)

let _ =
  while (current_score < previous_score) do
    a := improve2 !a;
    print_description !a;
    previous_score := !current_score;
    current_score := score !a;
    print_endline (string_of_int !current_score);
    print_newline ();
  done

