type color = Green | Blue;;

(* a cell is defined by
  - its colors  
  - the list of colors at its left
  - the list of colors on the left-above diagonal
  - the list of colors above 
*)
type cell = color * color list * color list * color list;;

let get_color_of_cell cell  = match cell with
  | ( color, _, _, _) -> color;;

let get_left_of_cell cell  = match cell with
  | ( _, left, _, _) -> left;;

let get_diag_of_cell cell  = match cell with
  | ( _, _, diag, _) -> diag;;

let get_above_of_cell cell  = match cell with
  | ( _, _, _, above) -> above;;

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

let cell_to_string cell = match cell with
  | ( color, _, _, _) -> color_to_string color;;

let display_line line = 
  List.iter ( fun x -> x |> cell_to_string |> print_string ) line;;

let display column row = 
  display_line column;
  print_string "|";
  display_line row;
  print_newline ();;

let rec display_whole_color_row row =
  match row with
  | [] -> print_newline ()
  | h::t -> begin
              display_whole_color_row t;
              h |> color_to_string |> print_string
            end;;

let rec display_whole_row row =
  match row with
  | [] -> print_newline ()
  | h::t -> begin
              display_whole_row t;
              h |> cell_to_string |> print_string
            end;;

let rec display_whole last_column last_row =
  match last_column with
  | [] -> display_whole_row last_row
  | h::t -> begin
              display_whole t;
              h |> get_left_of_cell |> display_whole_color_row
            end;;
  
let build_new_cell color left_cell diag_cell above_cell = 
  (color,
   ((get_color_of_cell left_cell)::(get_left_of_cell left_cell)),
   ((get_color_of_cell diag_cell)::(get_diag_of_cell diag_cell)),
   ((get_color_of_cell above_cell)::(get_above_of_cell above_cell)));;
  
let rec add_column last_column last_row =
  (* print_newline();
     print_endline "add_column";
     print_string  "last_column ";        display_line last_column;         print_newline() ;
     print_string  "last_row ";           display_line last_row;            print_newline() ; *)
  add_column_cell last_column last_row [ (Green, [], [], []) ] ;
  add_column_cell last_column last_row [ (Blue, [], [], []) ]
and
add_column_cell last_column_remainder last_row new_column =
  (* print_newline();
     print_endline "add_column_cell";
     print_string  "last_column_remainder "; display_line last_column_remainder; print_newline() ;
     print_string  "last_row ";              display_line last_row;              print_newline() ;
     print_string  "new_column ";            display_line new_column;            print_newline() ; *)
  match last_column_remainder with
  | [] -> ( add_row [ (Green, [], [], []) ] last_row ;
            add_row [ (Blue,  [], [], []) ] last_row )
  | [last] -> ( add_row ( (build_new_cell Green (List.hd last_row) last (List.hd new_column)) :: new_column ) last_row; 
                add_row ( (build_new_cell Blue  (List.hd last_row) last (List.hd new_column)) :: new_column ) last_row  )
  | h::((i::t) as rest) -> ( add_column_cell rest last_row ( (build_new_cell Green i h (List.hd new_column)) :: new_column ) ;
                             add_column_cell rest last_row ( (build_new_cell Blue  i h (List.hd new_column)) :: new_column ) )
and
add_row last_column last_row =
  (* print_newline();
     print_endline "add_row";
     print_string  "last_column ";        display_line last_column;         print_newline() ;
     print_string  "last_row ";           display_line last_row;            print_newline() ; *)
  add_row_cell last_column last_row [ (Green, [], [], ((last_row |> List.hd |> get_color_of_cell)::(last_row |> List.hd |> get_above_of_cell))) ] ;
  add_row_cell last_column last_row [ (Blue,  [], [], ((last_row |> List.hd |> get_color_of_cell)::(last_row |> List.hd |> get_above_of_cell))) ]
and
add_row_cell last_column last_row_remainder new_row =
  (* print_newline();
     print_endline "add_row_cell";
     print_string  "last_column ";        display_line last_column;         print_newline() ;
     print_string  "last_row_remainder "; display_line last_row_remainder;  print_newline() ;
     print_string  "new_row ";            display_line new_row ;            print_newline() ; *)
  match last_row_remainder with
  | [] -> if ((List.length last_column) > 2) then
              begin
                print_endline ">>>";
                display_whole last_column new_row;
                print_endline "<<<"
              end
            else
              add_column last_column new_row
  | [last] -> ( add_row_cell last_column [] ( (build_new_cell Green (new_row |> List.hd) last (last_column |> List.hd)) :: new_row ) ;
                add_row_cell last_column [] ( (build_new_cell Blue  (new_row |> List.hd) last (last_column |> List.hd)) :: new_row ) )
  | h::((i::t) as rest) -> ( add_row_cell last_column rest ( (build_new_cell Green (new_row |> List.hd) h i) :: new_row ) ;
                             add_row_cell last_column rest ( (build_new_cell Blue  (new_row |> List.hd) h i) :: new_row ) )
;;

add_column [] [ (Green, [], [], []) ];;

(* add_column [ (Green, [], [], []) ] [ (Green, [], [], []); (Green, [], [], []) ];; *)
