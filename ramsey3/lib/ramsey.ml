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

let generate_row_from_string (str: string) (previous_row: cell): cell =
  let l1 = explode_string str
  in
    let rec accumulate (l2: char list) (r: cell) =
      match l2 with
      | [] -> Nil
      | h::t -> begin
                  match r with
                  | Nil -> Cell { color = (char_to_color h); left = (accumulate t Nil); above = Nil }
                  | Cell c -> Cell { color = (char_to_color h); left = (accumulate t (c.left)); above = c.above }
                end      
    in
      accumulate l1 previous_row

let rec generate2_data_from_strings (strs: string list) : cell =
  match strs with
  | [] -> Nil
  | [last] -> generate_row_from_string last Nil
  | h::t -> generate_row_from_string h (generate2_data_from_strings t)

let build_from_string str: cell = generate2_data_from_strings (String.split_on_char '|' str)
