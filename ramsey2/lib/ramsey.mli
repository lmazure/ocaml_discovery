type color = Green | Blue
val color_to_string : color -> string
val char_to_color : char -> color
type cell = {
  color : color;
  left : cell list;
  diag : cell list;
  above : cell list;
}
val cell_to_debug_string : cell -> string
val last_list_element : 'a list -> 'a
val build_new_cell : color -> cell -> cell -> cell -> cell
val can_be_of_color : color -> cell list -> cell list -> cell list -> bool
val explode_string : string -> char list
val build_last_row : char list -> cell list
val generate_data_from_strings : string list -> cell list * cell list
val generate_data_from_string : string -> cell list * cell list
val add_column_cell :
  (cell list -> cell list -> unit) ->
  cell list -> cell list -> cell list -> unit
val add_column :
  (cell list -> cell list -> unit) -> cell list -> cell list -> unit
type cell2 = { color : color; left : cell2; diag : cell2; above : cell2; }
val cell2_to_debug_string : cell2 -> string
val generate_data_from_strings : string list -> cell list * cell list
val generate2_data_from_string : string -> cell list * cell list
