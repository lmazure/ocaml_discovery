(* extract the mon and the max values from avlist ot int *)
let rec min_max_list (list: int list) : int * int =
  match list with
  | [] -> invalid_arg "empty list"
  | [s] -> (s, s)
  | h::t -> let (cmin, cmax) = min_max_list t
            in if (h < cmin)
               then (h, cmax)
               else if (h > cmax)
               then (cmin, h)
               else (cmin, cmax)

type square = int * int * int * int * int * int * int * int * int 

(* convert a list into a square *)
let square_of_list (list: int list) : square =
  match list with
  | [ a; b; c; d; e; f; g; h; i ] -> (a, b, c, d, e, f, g, h, i)
  | _ -> invalid_arg "invalist list (should contain 9 int's)"


(* convert a square into a list *)
let list_of_square (sq: square) : int list =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in  [ a; b; c; d; e; f; g; h; i ]

(* print a square *)
let print_square (sq: square) : unit =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in Printf.printf "%d %d %d\n%d %d %d\n%d %d %d\n" a b c d e f g h i

(* print the full description of a square *)
let print_description (sq: square) : unit =
  print_square sq;
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (l1, l2, l3, c1, c2, c3, d1, d2) = (a*a + b*b + c*c, d*d + e*e + f*f, g*g + h*h + i*i, a*a + d*d + g*g, b*b + e*e + h*h, c*c + f*f + i*i, a*a + e*e + i*i, c*c + e*e + g*g)
  in begin
    print_endline ("line 1:   " ^ (string_of_int l1));
    print_endline ("line 2:   " ^ (string_of_int l2));
    print_endline ("line 3:   " ^ (string_of_int l3));
    print_endline ("column 1: " ^ (string_of_int c1));
    print_endline ("column 2: " ^ (string_of_int c2));
    print_endline ("column 3: " ^ (string_of_int c3));
    print_endline ("diag 1:   " ^ (string_of_int d1));
    print_endline ("diag 2:   " ^ (string_of_int d2));
    print_endline ("error: " ^ (let (min, max) = min_max_list [l1; l2; l3; c1; c2; c3; d1; d2]
                                in ((string_of_int max) ^ " - " ^ (string_of_int min) ^ " = " ^ (string_of_int (max -min)))))
  end

let score (sq: square) : int =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let a2, b2, c2, d2, e2, f2, g2, h2, i2 = a * a, b * b, c * c, d * d, e * e, f * f, g * g, h * h, i * i
  in let total = a2 + b2 + c2 + d2 + e2 + f2 + g2 + h2 + i2
  in let square x = x * x
  in let dist x y z = square (3 * (x + y + z) - total)
  in (dist a2 b2 c2) + (dist d2 e2 f2) + (dist g2 h2 i2) + (dist a2 d2 g2) + (dist b2 e2 h2) + (dist c2 f2 i2) + (dist a2 e2 i2) + (dist c2 e2 g2)

let determinate_best (sql: square list): square =
  let rec determinate_best_aux (sqll: square list) : square * int =
    if (sqll= [])
    then ((0,0,0,0,0,0,0,0,0), Int.max_int)
    else let current = score (List.hd sqll) in
      let best, next = determinate_best_aux (List.tl sqll)
      in
      if (current < next)
      then (List.hd sqll, current)
      else (best, next)
  in let b, _ = determinate_best_aux sql
  in b
      
let improve (sq: square): square =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in determinate_best [
    (a + 1, b, c, d, e, f, g, h, i);
    (a - 1, b, c, d, e, f, g, h, i);
    (a, b + 1, c, d, e, f, g, h, i);
    (a, b - 1, c, d, e, f, g, h, i);
    (a, b, c + 1, d, e, f, g, h, i);
    (a, b, c - 1, d, e, f, g, h, i);
    (a, b, c, d + 1, e, f, g, h, i);
    (a, b, c, d - 1, e, f, g, h, i);
    (a, b, c, d, e + 1, f, g, h, i);
    (a, b, c, d, e - 1, f, g, h, i);
    (a, b, c, d, e, f + 1, g, h, i);
    (a, b, c, d, e, f - 1, g, h, i);
    (a, b, c, d, e, f, g + 1, h, i);
    (a, b, c, d, e, f, g - 1, h, i);
    (a, b, c, d, e, f, g, h + 1, i);
    (a, b, c, d, e, f, g, h - 1, i);
    (a, b, c, d, e, f, g, h, i + 1);
    (a, b, c, d, e, f, g, h, i - 1)
  ]

(* ---------------------------------------------------------------------------------------------------------- *)

  let rec improve2_aux (treated: int list) (to_be_treated: int list) (funct: square -> 'a) (best: square) (nest_score: int): square * int =
    match to_be_treated with
    | [] -> let sq = square_of_list treated in (sq, funct sq)
    | h::t -> let (b1, s1) = improve2_aux (( h - 1 ) :: treated) t funct best nest_score in
              let (b2, s2) = improve2_aux (h :: treated) t funct best nest_score in
              let (b3, s3) = improve2_aux (( h + 1 ) :: treated) t funct best nest_score in 
              if ((s1 < s2) && (s1< s3))
              then (b1, s1)
              else if (s2 < s3)
              then (b2, s2)
              else (b3, s3)

let improve2 (sq: square): square =
  let (b, _) = improve2_aux [] (list_of_square sq) score (0, 0, 0, 0, 0, 0, 0, 0, 0) Int.min_int
in b
