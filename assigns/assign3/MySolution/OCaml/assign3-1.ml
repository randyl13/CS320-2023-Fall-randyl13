(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(* ****** ****** *)

let rec
list_map
(fopr)(xs) =
match xs with
| [] -> []
| x1 :: xs -> fopr(x1) :: list_map(fopr)(xs)


let head = function
  | [] -> []
  | x :: _ -> x

let tail = function
  | [] -> []
  | _ :: x -> x

let rec matrix_transpose = function
| [] -> []
| [] :: _ -> []
| rows    -> 
  list_map head rows :: matrix_transpose (list_map tail rows)

  let m1 =
    [[1;2;3];[4;5;6];[7;8;9]]
    ;;
    let n1 = matrix_transpose(m1)
    ;;
    (* ****** ****** *)
    let () = assert(m1 = matrix_transpose(n1))
    (* ****** ****** *)
    let m2 =
    [[1;2;3;10];[4;5;6;11];[7;8;9;12]]
    ;;
    let n2 = matrix_transpose(m2)
    ;;
    (* ****** ****** *)
    let () = assert(m2 = matrix_transpose(n2))
    (* ****** ****** *)
    let () = print_string("Assign3-1-test passed!\n")