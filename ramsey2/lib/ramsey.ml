type color = Green | Blue

let color_to_string color =
  match color with
  | Green -> "G"
  | Blue -> "B"

let char_to_color str =
  match str with
  | 'G' -> Green
  | 'B' -> Blue
  | _ -> raise (Invalid_argument "Unknown color")

(* a cell is defined by
  - its colors  
  - the list of cells at its left (starting from the nearest one)
  - the list of cells on the left-above diagonal (starting from the nearest one)
  - the list of cells above (starting from the nearest one)
*)
type cell = { color: color; left: cell list; diag: cell list; above: cell list }

let rec cell_to_debug_string cell =
  "[color=" 
  ^ (color_to_string cell.color )
  ^ ",left="
  ^ (String.concat "" (List.map (fun x -> cell_to_debug_string x) cell.left))
  ^ ",diag="
  ^ (String.concat "" (List.map (fun x -> cell_to_debug_string x) cell.diag))
  ^ ",above="
  ^ (String.concat "" (List.map (fun x -> cell_to_debug_string x) cell.above))
  ^ "]"

let rec last_list_element list =
  match list with
  | [] -> raise (Invalid_argument "Empty list")
  | [last] -> last
  | _::t -> last_list_element t

(* build a cell of the indicated color, left cells, diagonal cells, and above cells *)
let build_new_cell (color: color) (left_cell: cell) (diag_cell: cell) (above_cell: cell) = 
  { color = color;
    left = left_cell::left_cell.left;
    diag = diag_cell::diag_cell.diag;
    above = above_cell::above_cell.above }

(* test is current cell can be of color color *)
let rec can_be_of_color (color: color) (left_cells: cell list) (diag_cells: cell list) (above_cells: cell list) =
match left_cells with
| [] -> true
| hl::tl -> begin
  match diag_cells with
  | [] -> true
  | hd::td-> begin
    match above_cells with
    | [] -> true
    | ha::ta -> ((hl.color != color || hd.color != color || ha.color != color)  && ( can_be_of_color color tl td ta))
    end
  end

let explode_string (str: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (str.[i] :: l) in
  exp (String.length str - 1) []

let rec build_last_row (last_row_characters: char list): cell list =
  match last_row_characters with
  | [] -> []
  | h::t -> (build_last_row t) @  [ { color = (char_to_color h); left = []; diag = []; above = [] }  ]

let rec generate_data_from_strings (strings: string list) : ( cell list * cell list ) =
  match strings with
  | [] -> ([], [])
  | [last] ->  ([], build_last_row (explode_string last))
  | h::t -> begin
              let (before_last_column, _) = (generate_data_from_strings t) in
              let new_last_column = (before_last_column @  [ { color = (t |> last_list_element |> explode_string |> last_list_element |> char_to_color); left = []; diag = []; above = [] }  ]) in
              let new_last_row = build_last_row (explode_string h) in
              (new_last_column, new_last_row)
            end

(* the data is defined by an array of array of data_from_strings
   the result is a pair (last_column, last_row) *)
let generate_data_from_string (str: string) : ( cell list * cell list) =
  generate_data_from_strings (String.split_on_char '|' str)

let rec add_column_cell (consumer: cell list -> cell list -> unit) (last_column_remainder: cell list) (last_row: cell list) (new_column: cell list) =
  (* print_newline();
     print_endline "add_column_cell";
     print_string  "last_column_remainder "; display_line last_column_remainder; print_newline() ;
     print_string  "last_row ";              display_line last_row;              print_newline() ;
     print_string  "new_column ";            display_line new_column;            print_newline() ; *)
  match last_column_remainder with
  | [] -> consumer new_column last_row 
  | [last] -> ( consumer ( (build_new_cell Green (List.hd last_row) last (List.hd new_column)) :: new_column ) last_row; 
                consumer ( (build_new_cell Blue  (List.hd last_row) last (List.hd new_column)) :: new_column ) last_row  )
  | h::((i::_) as rest) -> ( add_column_cell consumer rest last_row ( (build_new_cell Green i h (List.hd new_column)) :: new_column ) ;
                             add_column_cell consumer rest last_row ( (build_new_cell Blue  i h (List.hd new_column)) :: new_column ) )

let add_column (consumer: cell list -> cell list -> unit) (last_column: cell list) (last_row: cell list)  =
  (* print_newline();
     print_endline "add_column";
     print_string  "last_column ";        display_line last_column;         print_newline() ;
     print_string  "last_row ";           display_line last_row;            print_newline() ; *)
  match last_column with
  | [] -> begin
            let last_row_element = last_list_element last_row in
              begin
                add_column_cell consumer last_column last_row [ { color = Green; left = last_row_element::last_row_element.left; diag = []; above = [] } ] ;
                add_column_cell consumer last_column last_row [ { color = Blue;  left = last_row_element::last_row_element.left; diag = []; above = [] } ]
              end
          end
  | _ -> ()
