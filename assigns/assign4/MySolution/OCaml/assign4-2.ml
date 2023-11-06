#use "./../../assign4.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let theNatPairs: (int * int) stream =
  let rec generate_pairs i j x = fun () -> 
    if i = 0 && j = 0 then
      StrCons((i, j),  generate_pairs i (j + 1) (x + 1))
    else if j = 0 then 
      StrCons((i, j), generate_pairs 0 (x + 1) (x + 1))
    else 
      StrCons((i, j), generate_pairs (i + 1) (j - 1) x)

    in generate_pairs 0 0 0
;;