type color = Green | Blue;;

(* a cell is defined by
  - its colors  
  - the list of colors at its left
  - the list of colors on the left-above diagonal
  - the list of colors above 
*)
type cell = color * color list * color list * color list;;

let rec can_be_of_color color l1 l2 l3 =
match l1 with
| [] -> true
| h1::t1 -> begin
  match l2 with
  | [] -> true
  | h2::t2 -> begin
    match l3 with
    | [] -> true
    | h3::t3 -> ((h1 != color || h2 != color || h3 != color)  && ( can_be_of_color color t1 t2 t3))
    end
  end;;

let color_to_string color =
  match color with
  | Green -> "G"
  | Blue -> "B";;

let display_line line = 
  List.iter ( fun x -> x |> color_to_string |> print_string ) line;;

let display column row = 
  display_line column;
  print_string "|";
  display_line row;
  print_newline ();;

let rec add_column last_column last_row =
  add_column_cell last_column last_row [ Green ] ; add_column_cell last_column last_row [ Blue ]
and
add_column_cell last_column_remainder last_row new_column =
  match last_column_remainder with
  | [] -> ( add_row [ Green ] last_row ; add_row [ Blue ] last_row )
  | [last] -> ( add_row ( Green :: new_column ) last_row ; add_row ( Blue :: new_column ) last_row )
  | h::t -> ( add_column_cell t last_row ( Green :: new_column ) ; add_column_cell t last_row ( Blue :: new_column ) )
and
add_row last_column last_row =
  add_row_cell last_column last_row [ Green ] ; add_row_cell last_column last_row [ Blue ]
and
add_row_cell last_column last_row_remainder new_row =
  match last_row_remainder with
  | [] -> ( display last_column new_row )
  | h::t -> ( add_row_cell last_column t ( Green :: new_row ) ; add_row_cell last_column t ( Blue :: new_row ) )
;;

add_column [] [Green];;
