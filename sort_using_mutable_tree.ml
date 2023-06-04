type node = Node of { mutable left : node; value : int; mutable right : node } | Nil

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
  
let rec insert_value_in_tree (v: int) (n: node): node =
  print_endline ("insert " ^ (string_of_int v) ^ " in tree " ^ (string_of_tree n));
  match n with
  | Nil -> Node { left = Nil; value = v; right = Nil }
  | Node nn -> begin
                if (nn.value > v) then
                  begin
                    match nn.left with
                   | Nil -> nn.left <- Node { left = Nil; value = v; right = Nil}
                   | Node _ -> ignore (insert_value_in_tree v nn.left)
                  end
                else
                  begin
                   match nn.right with
                   | Nil -> nn.right <- Node { left = Nil; value = v; right = Nil}
                   | Node _ -> ignore (insert_value_in_tree v nn.right)
                  end;
                print_endline ("returned tree is " ^ (string_of_tree n));
                n
              end

let rec build_tree_rec (l: int list) (n:node): node =
  match l with
  | [] -> n
  | h::t -> begin
              build_tree_rec t (insert_value_in_tree h n)
            end

let build_tree (l: int list): node = build_tree_rec l Nil





let _ = print_flat_tree (build_tree [])
let _ = print_flat_tree (build_tree [1])
let _ = print_flat_tree (build_tree [1;2])
let _ = print_flat_tree (build_tree [2;1])
let _ = print_flat_tree (build_tree [3;1;2])
let _ = print_flat_tree (build_tree [3;1;2;7;9;4;3;5;8])
