(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(* ****** ****** *)

let rec
list_map
(fopr: 'a -> 'b)(xs: 'a list): 'b list =
match xs with
| [] -> []
| x1 :: xs -> fopr(x1) :: list_map(fopr)(xs)
;;

let head = function
  | [] -> []
  | x :: _ -> x

let tail = function
  | [] -> []
  | _ :: x -> x

let rec
matrix_transpose(xss: 'a list list): 'a list list= function
  | [] -> []
  | [] :: _ -> []
  | x -> 
  list_map head x :: matrix_transpose (list_map tail x)

