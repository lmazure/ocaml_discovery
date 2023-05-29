open OUnit2
open Ramsey

let test1 = "cell accessors" >::: [
  "get color of cell" >:: (fun _ -> assert_equal Green
                                                 (get_color_of_cell (Green,
                                                                     [ (Green, [], [], []); (Blue, [], [], []); (Blue, [], [], []) ],
                                                                     [ (Green, [], [], []); (Blue, [], [], []); (Green, [], [], []) ],
                                                                     [ (Blue, [], [], []); (Blue, [], [], []) ])));
  "get left cells of cell" >:: (fun _ -> assert_equal [ (Green, [], [], []); (Blue, [], [], []); (Blue, [], [], []) ]
                                                      (get_left_of_cell (Green,
                                                                         [ (Green, [], [], []); (Blue, [], [], []); (Blue, [], [], []) ],
                                                                         [ (Green, [], [], []); (Blue, [], [], []); (Green, [], [], []) ],
                                                                         [ (Blue, [], [], []); (Blue, [], [], []) ])));
  "get diagonal cells of cell" >:: (fun _ -> assert_equal [ (Green, [], [], []); (Blue, [], [], []); (Green, [], [], []) ]
                                                          (get_diag_of_cell (Green,
                                                                             [ (Green, [], [], []); (Blue, [], [], []); (Blue, [], [], []) ],
                                                                             [ (Green, [], [], []); (Blue, [], [], []); (Green, [], [], []) ],
                                                                             [ (Blue, [], [], []); (Blue, [], [], []) ])));
  "get above cells of cell" >:: (fun _ -> assert_equal [ (Blue, [], [], []); (Blue, [], [], []) ]
                                                       (get_above_of_cell (Green,
                                                                           [ (Green, [], [], []); (Blue, [], [], []); (Blue, [], [], []) ],
                                                                           [ (Green, [], [], []); (Blue, [], [], []); (Green, [], [], []) ],
                                                                           [ (Blue, [], [], []); (Blue, [], [], []) ])));
]

let test2 = "color possibility" >::: [
  "single cell" >:: (fun _ -> assert_equal true (can_be_of_color Green
                                                                 []
                                                                 []
                                                                 []));
  "possible" >:: (fun _ -> assert_equal true (can_be_of_color Green
                                                               [ (Green, [], [], []); (Blue, [], [], []); (Blue, [], [], []) ]
                                                               [ (Green, [], [], []); (Blue, [], [], []); (Green, [], [], []) ]
                                                               [ (Blue, [], [], []); (Blue, [], [], []); (Blue, [], [], [])] ));
  "impossible" >:: (fun _ -> assert_equal false (can_be_of_color Blue
                                                                 [ (Green, [], [], []); (Blue, [], [], []); (Blue, [], [], []) ]
                                                                 [ (Green, [], [], []); (Blue, [], [], []); (Green, [], [], []) ]
                                                                 [ (Blue, [], [], []); (Blue, [], [], []); (Blue, [], [], [])] ));
  
]


let _ = run_test_tt_main test1
let _ = run_test_tt_main test2
