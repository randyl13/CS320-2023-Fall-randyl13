

(* ****** Given int n, returns a reversed int representation
using tail recursion****** *)

let rec intrev n acc: int =
  if n = 0 then acc
  else
  intrev (n / 10) (acc * 10 + (n mod 10))

let intrev10(n: int): int = 
  intrev n 0

let () =
assert(intrev10(1234) = 4321)
let () =
assert(intrev10(1020304) = 4030201)
let () =
assert(intrev10(intrev10(13579)) = 13579)
(* ****** ****** *)
let () =
print_string("Assign1-1-test passed!\n")



