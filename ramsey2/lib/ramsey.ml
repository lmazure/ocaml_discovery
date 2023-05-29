type color = Green | Blue

let get_color_of_cell cell  = match cell with
  | ( color, _, _, _) -> color

let get_left_of_cell cell  = match cell with
  | ( _, left, _, _) -> left

let get_diag_of_cell cell  = match cell with
  | ( _, _, diag, _) -> diag

let get_above_of_cell cell  = match cell with
  | ( _, _, _, above) -> above

  