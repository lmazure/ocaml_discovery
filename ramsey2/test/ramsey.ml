open OUnit2
open Ramsey

let tests = "cell accessors" >::: [
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

let _ = run_test_tt_main tests