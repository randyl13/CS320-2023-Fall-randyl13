(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(* ****** ****** *)

let
list_map
(xs: 'a list)(fopr: 'a -> 'b): 'b list =
list_foldright(xs)([])(fun x0 r0 -> fopr(x0) :: r0)
;;

let build_subsets (xs: 'a) (subsets: 'a list list): 'a list list =
  list_foldright subsets [] (fun subset acc ->
    (xs :: subset) :: subset :: acc
  )

let list_subsets (xs: 'a list): 'a list list = 
list_foldright xs [[]] build_subsets 

