type color = Green | Blue

let color_to_string color =
  match color with
  | Green -> "G"
  | Blue -> "B";;

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
