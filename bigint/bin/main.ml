open Z

let rec min_max_list (list: Z.t list) : Z.t * Z.t =
  match list with
  | [] -> invalid_arg "empty list"
  | [s] -> (s, s)
  | h::t -> let (cmin, cmax) = min_max_list t
            in if (h < cmin)
               then (h, cmax)
               else if (h > cmax)
               then (cmin, h)
               else (cmin, cmax)

type square = Z.t * Z.t * Z.t * Z.t * Z.t * Z.t * Z.t * Z.t * Z.t 

let is_valid_square (sq: square) : bool =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in
  a <> b && a <> c && a <> d && a <> e && a <> f && a <> g && a <> h && a <> i &&
  b <> c && b <> d && b <> e && b <> f && b <> g && b <> h && b <> i &&
  c <> d && c <> e && c <> f && c <> g && c <> h && c <> i &&
  d <> e && d <> f && d <> g && d <> h && d <> i &&
  e <> f && e <> g && e <> h && e <> i &&
  f <> g && f <> h && f <> i &&
  g <> h && g <> i &&
  h <> i

(* print a square *)
let print_square (sq: square) : unit =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in Printf.printf "%s %s %s\n%s %s %s\n%s %s %s\n" (Z.to_string a) (Z.to_string b) (Z.to_string c) (Z.to_string d) (Z.to_string e) (Z.to_string f) (Z.to_string g) (Z.to_string h) (Z.to_string i)

(* print the full description of a square *)
let print_description (sq: square) : unit =
  print_square sq;
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (l1, l2, l3, c1, c2, c3, d1, d2) = (a*a + b*b + c*c, d*d + e*e + f*f, g*g + h*h + i*i, a*a + d*d + g*g, b*b + e*e + h*h, c*c + f*f + i*i, a*a + e*e + i*i, c*c + e*e + g*g)
  in begin
    print_endline ("line 1:   " ^ (Z.to_string l1));
    print_endline ("line 2:   " ^ (Z.to_string l2));
    print_endline ("line 3:   " ^ (Z.to_string l3));
    print_endline ("column 1: " ^ (Z.to_string c1));
    print_endline ("column 2: " ^ (Z.to_string c2));
    print_endline ("column 3: " ^ (Z.to_string c3));
    print_endline ("diag 1:   " ^ (Z.to_string d1));
    print_endline ("diag 2:   " ^ (Z.to_string d2));
    print_endline ("error: " ^ (let (min, max) = min_max_list [l1; l2; l3; c1; c2; c3; d1; d2]
                                in ((Z.to_string max) ^ " - " ^ (Z.to_string min) ^ " = " ^ (Z.to_string (max -min)))))
  end

let score (sq: square) : Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let a2, b2, c2, d2, e2, f2, g2, h2, i2 = a * a, b * b, c * c, d * d, e * e, f * f, g * g, h * h, i * i
  in let total = a2 + b2 + c2 + d2 + e2 + f2 + g2 + h2 + i2
  in let square x = x * x
  in let dist x y z = square ((Z.of_int 3) * (x + y + z) - total)
  in (dist a2 b2 c2) + (dist d2 e2 f2) + (dist g2 h2 i2) + (dist a2 d2 g2) + (dist b2 e2 h2) + (dist c2 f2 i2) + (dist a2 e2 i2) + (dist c2 e2 g2)

let score2 (sq: square) : Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (l1, l2, l3, c1, c2, c3, d1, d2) = (a*a + b*b + c*c, d*d + e*e + f*f, g*g + h*h + i*i, a*a + d*d + g*g, b*b + e*e + h*h, c*c + f*f + i*i, a*a + e*e + i*i, c*c + e*e + g*g)
  in let (min, max) = min_max_list [l1; l2; l3; c1; c2; c3; d1; d2]
  in max - min

let improve3_select_best_of_3_squares (sq1: square) (s1: Z.t) (sq2: square) (s2: Z.t) (sq3: square) (s3: Z.t) : square * Z.t =
  if ((s1 < s2) && (s1 < s3))
  then (sq1, s1)
  else if (s2 < s3)
  then (sq2, s2)
  else (sq3, s3)

let improve3_level_9 (sq: square) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let sq1, sq2, sq3 = (a,b, c, d, e, f, g, h, Z.pred i), (a,b, c, d, e, f, g, h, i), (a,b, c, d, e, f, g, h, Z.succ i)
  in let s1, s2, s3 = funct sq1, funct sq2, funct sq3
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_8 (sq: square) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_9 (a, b, c, d, e, f, g, Z.pred h, i) funct, improve3_level_9 (a, b, c, d, e, f, g, h, i) funct, improve3_level_9 (a, b, c, d, e, f, g, Z.succ h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_7 (sq: square) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_8 (a, b, c, d, e, f, Z.pred g, h, i) funct, improve3_level_8 (a, b, c, d, e, f, g, h, i) funct, improve3_level_8 (a, b, c, d, e, f, Z.succ g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_6 (sq: square) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_7 (a, b, c, d, e, Z.pred f, g, h, i) funct, improve3_level_7 (a, b, c, d, e, f, g, h, i) funct, improve3_level_7 (a, b, c, d, e, Z.succ f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_5 (sq: square) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_6 (a, b, c, d, Z.pred e, f, g, h, i) funct, improve3_level_6 (a, b, c, d, e, f, g, h, i) funct, improve3_level_6 (a, b, c, d, Z.succ e, f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_4 (sq: square) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_5 (a, b, c, Z.pred d, e, f, g, h, i) funct, improve3_level_5 (a, b, c, d, e, f, g, h, i) funct, improve3_level_5 (a, b, c, Z.succ d, e, f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_3 (sq: square) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_4 (a, b, Z.pred c, d, e, f, g, h, i) funct, improve3_level_4 (a, b, c, d, e, f, g, h, i) funct, improve3_level_4 (a, b, Z.succ c, d, e, f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_2 (sq: square) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_3 (a, Z.pred b, c, d, e, f, g, h, i) funct, improve3_level_3 (a, b, c, d, e, f, g, h, i) funct, improve3_level_3 (a, Z.succ b, c, d, e, f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_1 (sq: square) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_2 (Z.pred a, b, c, d, e, f, g, h, i) funct, improve3_level_2 (a, b, c, d, e, f, g, h, i) funct, improve3_level_2 (Z.succ a, b, c, d, e, f, g, h, i) funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3 (sq: square) : square * Z.t =
    improve3_level_1 sq score2

let max_score : Z.t = Z.of_string "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"

let optimize (sq: square) : square * Z.t =
  let a = ref sq
  in let previous_score : Z.t ref = ref max_score
  in let current_score : Z.t ref  = ref (score sq)
  in while (current_score < previous_score) do
    previous_score := !current_score;
    let (b, s) = improve3 !a in
    begin
      a := b;
      current_score := s;
    end
  done;
  ( !a, !current_score)

let launch (seed: int) : square*Z.t =
  let _ = Random.init (seed)
  in let a = ( 9999 |> Random.int |> Z.of_int,
               9999 |> Random.int |> Z.of_int,
               9999 |> Random.int |> Z.of_int,
               9999 |> Random.int |> Z.of_int,
               9999 |> Random.int |> Z.of_int,
               9999 |> Random.int |> Z.of_int,
               9999 |> Random.int |> Z.of_int,
               9999 |> Random.int |> Z.of_int,
               9999 |> Random.int |> Z.of_int 
            )
  in optimize a

let start : int = if (Array.length Sys.argv) != 2 then invalid_arg ("Syntax: " ^ Sys.argv.(0) ^ " <seed>"); int_of_string Sys.argv.(1)

let previous_score : Z.t ref = ref max_score

let _ = for seed = start to (Int.add start 1000000) do
  let (square, score) = launch seed in
  begin
    if (is_valid_square square) && (score < !previous_score) then
      begin
        print_endline (string_of_int seed);
        print_description square;
        print_newline ();
        previous_score := score
      end
  end
done
