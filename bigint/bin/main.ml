open Z

let c =
  let a = Z.of_string "12345678901234567890" in
  let b = Z.of_string "98765432109876543210" in
  a + b

let _ = Printf.printf "Sum: %s\n" (Z.to_string c);
