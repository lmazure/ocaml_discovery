type color = Green | Blue

(* accessors *)
let get_color_of_cell cell  = match cell with
  | ( color, _, _, _) -> color

let get_left_of_cell cell  = match cell with
  | ( _, left, _, _) -> left

let get_diag_of_cell cell  = match cell with
  | ( _, _, diag, _) -> diag

let get_above_of_cell cell  = match cell with
  | ( _, _, _, above) -> above


(* test is current cell can be of color color *)
let rec can_be_of_color color left_cells diag_cells above_cells =
match left_cells with
| [] -> true
| hl::tl -> begin
  match diag_cells with
  | [] -> true
  | hd::td-> begin
    match above_cells with
    | [] -> true
    | ha::ta -> (((get_color_of_cell hl) != color || (get_color_of_cell hd) != color || (get_color_of_cell ha) != color)  && ( can_be_of_color color tl td ta))
    end
  end
