let explode_string (str: string) : char list =
  let rec exp i l = 
    if (i < 0) then (l) else (exp (i - 1) ( l @ [str.[i]]))
  in
    exp (String.length str - 1) []

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
  - the left cell
  - the cell above
*)
type cell = Cell of { color: color; left: cell; above: cell } | Nil

let rec row_to_string (c: cell) : string =
  match c with
  | Nil -> ""
  | Cell x -> (row_to_string x.left) ^ (color_to_string x.color)

let rec data_to_string (c: cell) : string =
  match c with
  | Nil -> ""
  | Cell x -> let p = (data_to_string x.above) in
                (if (p = "") then "" else (p ^ "|")) ^ (row_to_string c)

let rec is_color_acceptable (color: color) (left_cell: cell) (diag_cell: cell) (above_cell: cell) : bool =
  match left_cell with
  | Nil -> true
  | Cell l -> begin
                match diag_cell with
                | Nil -> true
                | Cell d -> begin
                              match above_cell with
                              | Nil -> true
                              | Cell a -> begin
                                            if (l.color = color) && (d.color = color) && (a.color = color)
                                              then false
                                              else match d.left with
                                                   | Nil -> true
                                                   | Cell ll -> (is_color_acceptable color l.left ll.above a.above)
                                          end
                            end
              end

let add_one_row_from_string (str: string) (previous_row: cell): cell =
  let l1 = explode_string str
  in
    let rec accumulate (l2: char list) (r: cell) =
      match l2 with
      | [] -> Nil
      | h::t -> begin
                  match r with
                  | Nil -> Cell { color = (char_to_color h); left = (accumulate t Nil); above = Nil }
                  | Cell c -> Cell { color = (char_to_color h); left = (accumulate t (c.left)); above = Cell c }
                end      
    in accumulate l1 previous_row

let build_from_string (s: string) : cell =
  let rec loop (l: string list) (c: cell) : cell =
    match l with
    | [] -> c
    | h::t -> loop t (add_one_row_from_string h c)
  in loop (String.split_on_char '|' s) Nil

let rec filter_map_list (m: 'a -> 'b) (f: 'a -> bool) (l: 'a list): 'b list =
  match l with
  | [] -> []
  | h::t -> if (f h) then ((m h)::(filter_map_list m f t)) else (filter_map_list m f t)

let add_column (c: cell) : cell list =
  let rec add (cell_prev_col: cell) : cell list = 
    match cell_prev_col with
    | Nil -> []
    | Cell pc -> match pc.above with
                  | Nil ->  [ Cell { color = Green; left = cell_prev_col; above = Nil };
                              Cell { color = Blue;  left = cell_prev_col; above = Nil }]
                  | Cell pca -> filter_map_list (fun x -> Cell { color = Green; left = cell_prev_col; above = x }) (fun _ -> true) (add (Cell pca)) @
                                filter_map_list (fun x -> Cell { color = Blue;  left = cell_prev_col; above = x }) (fun _ -> true) (add (Cell pca))
  in add c

let add_row (c: cell) : cell list =
  let rec add (cell_prev_row: cell) : cell list = 
    match cell_prev_row with
    | Nil -> []
    | Cell pc -> match pc.left with
                  | Nil ->  [ Cell { color = Green; left = Nil ; above = cell_prev_row };
                              Cell { color = Blue;  left = Nil ; above = cell_prev_row }]
                  | Cell pcb -> filter_map_list (fun x -> Cell { color = Green; left = x ; above = cell_prev_row }) (fun _ -> true) (add (Cell pcb)) @
                                filter_map_list (fun x -> Cell { color = Blue;  left = x ; above = cell_prev_row }) (fun _ -> true) (add (Cell pcb))
  in add c

let add_acceptable_column (c: cell) : cell list =
  let rec add (cell_prev_col: cell) : cell list = 
    match cell_prev_col with
    | Nil -> []
    | Cell pc -> match pc.above with
                  | Nil ->  [ Cell { color = Green; left = cell_prev_col; above = Nil };
                              Cell { color = Blue;  left = cell_prev_col; above = Nil }]
                  | Cell pca -> filter_map_list (fun x -> Cell { color = Green; left = cell_prev_col; above = x }) (fun x -> (is_color_acceptable Green cell_prev_col pc.above x)) (add (Cell pca)) @
                                filter_map_list (fun x -> Cell { color = Blue;  left = cell_prev_col; above = x }) (fun x -> (is_color_acceptable Blue  cell_prev_col pc.above x)) (add (Cell pca))
  in add c
  