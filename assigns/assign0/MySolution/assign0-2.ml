(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

(* ******  This function uses a loop to find if there are any factors
  of n0 other than itself and 1 ****** *)

let isPrime(n0: int): bool =
if n0 < 2 then
  false
else if n0 < 4 then 
  true
else 
  let rec myLoop(x: int): bool =
  if x * x > n0 then
    true
  else if n0 mod x = 0 then
    false
  else 
    myLoop(x+1)
  in myLoop 2
;;


