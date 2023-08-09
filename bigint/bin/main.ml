open Z

(* return the minimum and maximum values of a list of values *)
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

(* the type square*)
type square = Z.t * Z.t * Z.t * Z.t * Z.t * Z.t * Z.t * Z.t * Z.t 

(* test if a square is valid, i.e. all numbers are different *)
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

(* generate a random square *)
let random_square (max_value: int) (seed: int) : square =
  let  _ = Random.init (seed)
  in
  ( max_value |> Random.int |> Z.of_int,
    max_value |> Random.int |> Z.of_int,
    max_value |> Random.int |> Z.of_int,
    max_value |> Random.int |> Z.of_int,
    max_value |> Random.int |> Z.of_int,
    max_value |> Random.int |> Z.of_int,
    max_value |> Random.int |> Z.of_int,
    max_value |> Random.int |> Z.of_int,
    max_value |> Random.int |> Z.of_int 
  )
(*
let score (sq: square) : Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let a2, b2, c2, d2, e2, f2, g2, h2, i2 = a * a, b * b, c * c, d * d, e * e, f * f, g * g, h * h, i * i
  in let total = a2 + b2 + c2 + d2 + e2 + f2 + g2 + h2 + i2
  in let square x = x * x
  in let dist x y z = square ((Z.of_int 3) * (x + y + z) - total)
  in (dist a2 b2 c2) + (dist d2 e2 f2) + (dist g2 h2 i2) + (dist a2 d2 g2) + (dist b2 e2 h2) + (dist c2 f2 i2) + (dist a2 e2 i2) + (dist c2 e2 g2)
*)

let score (sq: square) : Z.t =
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

let improve3_level_9 (sq: square) (incr: Z.t) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let sq1, sq2, sq3 = (a,b, c, d, e, f, g, h, Z.add i incr), (a,b, c, d, e, f, g, h, i), (a,b, c, d, e, f, g, h, Z.sub i incr)
  in let s1, s2, s3 = funct sq1, funct sq2, funct sq3
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_8 (sq: square) (incr: Z.t) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_9 (a, b, c, d, e, f, g, Z.add h incr, i) incr funct, improve3_level_9 (a, b, c, d, e, f, g, h, i) incr funct, improve3_level_9 (a, b, c, d, e, f, g, Z.sub h incr, i) incr funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_7 (sq: square) (incr: Z.t) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_8 (a, b, c, d, e, f, Z.add g incr, h, i) incr funct, improve3_level_8 (a, b, c, d, e, f, g, h, i) incr funct, improve3_level_8 (a, b, c, d, e, f, Z.sub g incr, h, i) incr funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_6 (sq: square) (incr: Z.t) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_7 (a, b, c, d, e, Z.add f incr, g, h, i) incr funct, improve3_level_7 (a, b, c, d, e, f, g, h, i) incr funct, improve3_level_7 (a, b, c, d, e, Z.sub f incr, g, h, i) incr funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_5 (sq: square) (incr: Z.t) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_6 (a, b, c, d, Z.add e incr, f, g, h, i) incr funct, improve3_level_6 (a, b, c, d, e, f, g, h, i) incr funct, improve3_level_6 (a, b, c, d, Z.sub e incr, f, g, h, i) incr funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_4 (sq: square) (incr: Z.t) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_5 (a, b, c, Z.add d incr, e, f, g, h, i) incr funct, improve3_level_5 (a, b, c, d, e, f, g, h, i) incr funct, improve3_level_5 (a, b, c, Z.sub d incr, e, f, g, h, i) incr funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_3 (sq: square) (incr: Z.t) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_4 (a, b, Z.add c incr, d, e, f, g, h, i) incr funct, improve3_level_4 (a, b, c, d, e, f, g, h, i) incr funct, improve3_level_4 (a, b, Z.sub c incr, d, e, f, g, h, i) incr funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_2 (sq: square) (incr: Z.t) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_3 (a, Z.add b incr, c, d, e, f, g, h, i) incr funct, improve3_level_3 (a, b, c, d, e, f, g, h, i) incr funct, improve3_level_3 (a, Z.sub b incr, c, d, e, f, g, h, i) incr funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3_level_1 (sq: square) (incr: Z.t) (funct: square -> Z.t) : square * Z.t =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in let (sq1, s1), (sq2, s2), (sq3, s3) = improve3_level_2 (Z.add a incr, b, c, d, e, f, g, h, i) incr funct, improve3_level_2 (a, b, c, d, e, f, g, h, i) incr funct, improve3_level_2 (Z.sub a incr, b, c, d, e, f, g, h, i) incr funct
  in improve3_select_best_of_3_squares sq1 s1 sq2 s2 sq3 s3

let improve3 (sq: square) (incr: Z.t): square * Z.t =
    improve3_level_1 sq incr score

(* maximum value of the random numbers *)
let max_range: int = 999999
let max_score : Z.t = (Z.of_int max_range)  * (Z.of_int max_range)  * (Z.of_int 9)

let optimize (sq: square) : square * Z.t =
  let a = ref sq
  in let incr : Z.t ref = ref (shift_right (Z.of_int max_range) 10)
  in let previous_score : Z.t ref = ref max_score
  in let current_score : Z.t ref  = ref (score sq)
  in while (!incr != Z.zero) do
    while (current_score < previous_score) do
      previous_score := !current_score;
      let (b, s) = improve3 !a !incr in
      begin
        a := b;
        current_score := s;
      end
    done;
    incr := Z.shift_right !incr 1
  done;
  ( !a, !current_score)


let launch (seed: int) : square*Z.t =
  let a = random_square max_range seed
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
