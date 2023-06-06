type node = Node of { left : node; value : int; right : node } | Nil

let rec list_of_tree (n: node): int list =
  match n with
  | Nil -> []
  | Node { left; value; right } -> (list_of_tree left) @ [ value ] @ (list_of_tree right)

let rec string_of_tree (n: node): string =
  match n with
  | Nil -> ""
  | Node { left; value; right } -> ("{" ^ (string_of_tree left) ^ "," ^ (string_of_int value) ^ "," ^  (string_of_tree right) ^ "}")

let rec string_of_list (l: int list): string =
  List.fold_right (fun v s -> (string_of_int v) ^ "," ^ s) l ""

let print_tree (n: node): unit =
  print_endline (string_of_tree n)

let print_flat_tree (n: node): unit =
  print_endline (string_of_list (list_of_tree n))

let rec insert_value_in_tree (v: int) (tree: node): node =
  match tree with
  | Nil -> Node { left = Nil; value = v; right = Nil }
  | Node n -> if (v < n.value) then Node { left = (insert_value_in_tree v n.left); value = n.value; right = n.right }
                               else Node { left = n.left; value = n.value; right = (insert_value_in_tree v n.right) }
  
let rec insert_values_in_tree (l: int list) (tree: node): node =
  match l with
  | [] -> tree
  | h::t -> insert_value_in_tree h (insert_values_in_tree t tree)

let build_tree (l: int list): node = insert_values_in_tree l Nil


let _ = print_tree (build_tree [])
let _ = print_tree (build_tree [1])
let _ = print_tree (build_tree [1;2])
let _ = print_tree (build_tree [2;1])
let _ = print_tree (build_tree [3;1;2])
let _ = print_tree (build_tree [3;1;2;7;9;4;3;5;8])
