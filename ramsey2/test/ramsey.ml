open OUnit2
open Ramsey

let test1 = "color possibility" >::: [
  "single cell" >:: (fun _ -> assert_equal true (can_be_of_color Green
                                                                 []
                                                                 []
                                                                 []));
  "possible" >:: (fun _ -> assert_equal true (can_be_of_color Green
                                                               [ {color = Green; left = []; diag = []; above = []}; { color = Blue; left = []; diag = []; above = []}; {color = Blue;  left = []; diag = []; above = []} ]
                                                               [ {color = Green; left = []; diag = []; above = []}; { color = Blue; left = []; diag = []; above = []}; {color = Green; left = []; diag = []; above = []} ]
                                                               [ {color = Blue;  left = []; diag = []; above = []}; { color = Blue; left = []; diag = []; above = []}; {color = Blue;  left = []; diag = []; above = []}] ));
  "impossible" >:: (fun _ -> assert_equal false (can_be_of_color Blue
                                                                 [ {color = Green; left = []; diag = []; above = []}; { color = Blue; left = []; diag = []; above = []}; {color = Blue;  left = []; diag = []; above = []} ]
                                                                 [ {color = Green; left = []; diag = []; above = []}; { color = Blue; left = []; diag = []; above = []}; {color = Green; left = []; diag = []; above = []} ]
                                                                 [ {color = Blue;  left = []; diag = []; above = []}; { color = Blue; left = []; diag = []; above = []}; {color = Blue;  left = []; diag = []; above = []}] ));
  
]

let consumer_accumulation: (cell list * cell list) list ref = ref ([]: (cell list * cell list) list)
let consumer column row = (consumer_accumulation:= (!consumer_accumulation @ [(column, row)]))

let cell_list_to_string (list: cell list) = String.concat "" (List.map (fun x -> cell_to_debug_string x) list)

let column_row_pair_to_string ((col: cell list), (row: cell list)) = String.concat ""
                                                                                 [ "column: ";
                                                                                    (cell_list_to_string col);
                                                                                    " row: ";
                                                                                    (cell_list_to_string row)]
                                        
let column_row_pair_list_to_string (list: (cell list * cell list) list) = "\n" ^ (String.concat "\n" (List.map (fun x -> (column_row_pair_to_string x)) list))

let test2 = "column addition" >::: [
  "single green cell" >:: (fun _ -> begin
                                      consumer_accumulation := ([]: (cell list * cell list) list);
                                      add_column consumer [] [ { color = Green; left = []; diag =  []; above = [] } ];
                                      assert_equal [ ([{ color = Green; left = [ { color = Green; left = []; diag = []; above = [] } ]; diag = []; above = [] }], [{ color = Green; left = []; diag =  []; above = [] }]);
                                                     ([{ color = Blue;  left = [ { color = Green; left = []; diag = []; above = [] } ]; diag = []; above = [] }], [{ color = Green; left = []; diag =  []; above = [] }])]
                                                   !consumer_accumulation
                                                   ~printer:column_row_pair_list_to_string
                                    end);
  "single blue cell" >:: (fun _ -> begin
                                      consumer_accumulation := ([]: (cell list * cell list) list);
                                      add_column consumer [] [ { color = Blue; left = []; diag =  []; above = [] } ];
                                      assert_equal [ ([{ color = Green; left = [ { color = Blue; left = []; diag = []; above = [] } ]; diag = []; above = [] }], [{ color = Blue; left = []; diag =  []; above = [] }]);
                                                     ([{ color = Blue;  left = [ { color = Blue; left = []; diag = []; above = [] } ]; diag = []; above = [] }], [{ color = Blue; left = []; diag =  []; above = [] }])]
                                                   !consumer_accumulation
                                                   ~printer:column_row_pair_list_to_string
                                   end);
]

let test3 = "blabla" >::: [
  "blabla" >:: (fun _ -> begin
                                      assert_equal ([], []) ( generate_data_from_string "B" )
                                                   ~printer:column_row_pair_to_string
                                    end);
]


let _ = run_test_tt_main test1
let _ = run_test_tt_main test2
let _ = run_test_tt_main test3

(* -------------------------------------------------------- *)

