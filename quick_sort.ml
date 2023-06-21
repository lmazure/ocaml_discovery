type current_division = { pivot: int; smaller: int list; larger: int list; remainder: int list }

(* recursively split a list into two sub-lists according to a pivot
the first sublist contains the number smaller than the pivot
the second sublist contain the number larger than the pivot *)
let rec divide current =
  if (current.remainder = [])
  then { pivot = current.pivot;
         smaller = current.smaller;
         larger = current.larger ;
         remainder = []}
  else if (( List.hd current.remainder ) > current.pivot)
  then divide { pivot = current.pivot;
                smaller = current.smaller;
                larger = List.hd current.remainder :: current.larger ;
                remainder = List.tl current.remainder}
  else divide { pivot = current.pivot;
                smaller = List.hd current.remainder :: current.smaller;
                larger = current.larger;
                remainder = List.tl current.remainder }

(* quick sort *)
let rec quick_sort l =
  if (l = [])
  then []
  else let current = divide {pivot = (List.hd l); smaller =  []; larger = []; remainder = (List.tl l)} in
    (quick_sort current.smaller) @ [ List.hd l] @ (quick_sort current.larger)

(* quick sort -- shorter version *)

let rec  quick_sort_2 list =
  match list with
  | [] -> []
  | h::t -> let (a,b) = List.partition (fun x -> x < h) t in
      (quick_sort a) @ ( h :: quick_sort b)
