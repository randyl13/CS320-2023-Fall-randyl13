

(* ****** Given int n, returns a reversed int representation
using tail recursion****** *)

let rec intrev n acc: int =
  if n = 0 then acc
  else
  intrev (n / 10) (acc * 10 + (n mod 10))

let intrev10(n: int): int = 
  intrev n 0




