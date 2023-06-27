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

(* scale a square *)
let scale_square (sq: square ) (factor: float) : square =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in (
    a |> Int.to_float|> (Float.mul factor) |> (Float.add 0.5) |> Int.of_float,
    b|> Int.to_float|> (Float.mul factor) |> (Float.add 0.5) |> Int.of_float,
    c |> Int.to_float|> (Float.mul factor) |> (Float.add 0.5) |> Int.of_float,
    d |> Int.to_float|> (Float.mul factor) |> (Float.add 0.5) |> Int.of_float,
    e |> Int.to_float|> (Float.mul factor) |> (Float.add 0.5) |> Int.of_float,
    f |> Int.to_float|> (Float.mul factor) |> (Float.add 0.5) |> Int.of_float,
    g |> Int.to_float|> (Float.mul factor) |> (Float.add 0.5) |> Int.of_float,
    h |> Int.to_float|> (Float.mul factor) |> (Float.add 0.5) |> Int.of_float,
    i |> Int.to_float|> (Float.mul factor) |> (Float.add 0.5) |> Int.of_float
  )

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

let improve3_select_best_of_3_squares (sq1: square) (s1: int) (sq2: square) (s2: int) (sq3: square) (s3: int) : square * int =
  if ((s1 < s2) && (s1 < s3))
  then (sq1, s1)
  else if (s2 < s3)
  then (sq2, s2)
  else (sq3, s3)

let improve3_level_9 (sq: square) (funct: square -> int) : square * int =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let sq1, sq2, sq3 = (a,b, c, d, e, f, g, h, i-1), (a,b, c, d, e, f, g, h, i), (a,b, c, d, e, f, g, h, i+1)
  in let s1, s2, s3 = funct sq1, funct sq2, funct sq3
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_8 (sq: square) (funct: square -> int) : square * int =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_9 (a, b, c, d, e, f, g, h-1, i) funct, improve3_level_9 (a, b, c, d, e, f, g, h, i) funct, improve3_level_9 (a, b, c, d, e, f, g, h+1, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_7 (sq: square) (funct: square -> int) : square * int =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_8 (a, b, c, d, e, f, g-1, h, i) funct, improve3_level_8 (a, b, c, d, e, f, g, h, i) funct, improve3_level_8 (a, b, c, d, e, f, g+1, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_6 (sq: square) (funct: square -> int) : square * int =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_7 (a, b, c, d, e, f-1, g, h, i) funct, improve3_level_7 (a, b, c, d, e, f, g, h, i) funct, improve3_level_7 (a, b, c, d, e, f+1, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_5 (sq: square) (funct: square -> int) : square * int =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_6 (a, b, c, d, e-1, f, g, h, i) funct, improve3_level_6 (a, b, c, d, e, f, g, h, i) funct, improve3_level_6 (a, b, c, d, e+1, f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_4 (sq: square) (funct: square -> int) : square * int =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_5 (a, b, c, d-1, e, f, g, h, i) funct, improve3_level_5 (a, b, c, d, e, f, g, h, i) funct, improve3_level_5 (a, b, c, d+1, e, f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_3 (sq: square) (funct: square -> int) : square * int =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_4 (a, b, c-1, d, e, f, g, h, i) funct, improve3_level_4 (a, b, c, d, e, f, g, h, i) funct, improve3_level_4 (a, b, c+1, d, e, f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_2 (sq: square) (funct: square -> int) : square * int =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_3 (a, b-1, c, d, e, f, g, h, i) funct, improve3_level_3 (a, b, c, d, e, f, g, h, i) funct, improve3_level_3 (a, b+1, c, d, e, f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_1 (sq: square) (funct: square -> int) : square * int =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_2 (a-1, b, c, d, e, f, g, h, i) funct, improve3_level_2 (a, b, c, d, e, f, g, h, i) funct, improve3_level_2 (a+1, b, c, d, e, f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

  let improve3 (sq: square) : square  =
    let (b, _) = improve3_level_1 sq score
    in b
  
let _ = Random.init (int_of_string Sys.argv.(1))

let a = ref (
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999,
  Random.int 9999
)

let _ = print_endline "--- optimize ---"
let previous_score = ref Int.max_int
let current_score = ref (score !a)
let _ =
  while (current_score < previous_score) do
    a := improve3 !a;
    previous_score := !current_score;
    current_score := score !a;
  done 
let _ = print_description !a
let _ = print_endline (string_of_int !current_score)
let _ = print_newline ()


let _ = print_endline "--- scale ---"
let _ = a := scale_square !a 1.1
(* let _ = print_description !a *)

let _ = print_endline "--- optimize ---"
let previous_score = ref Int.max_int
let current_score = ref (score !a)
let _ =
  while (current_score < previous_score) do
    a := improve3 !a;
    previous_score := !current_score;
    current_score := score !a;
  done
let _ = print_description !a
let _ = print_endline (string_of_int !current_score)
let _ = print_newline ()
