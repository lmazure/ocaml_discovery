type square = int * int * int * int * int * int * int * int * int 

let print_square (sq: square) : unit =
  let (a ,b, c, d, e, f, g, h, i) = sq
  in Printf.printf "%d %d %d\n%d %d %d\n%d %d %d\n" a b c d e f g h i

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

