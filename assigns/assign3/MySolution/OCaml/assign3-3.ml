(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(* ****** ****** *)

let rec length xs =
  match xs with
  | [] -> 0
  | _ :: x -> 1 + length x

let
list_map
(xs: 'a list)(fopr: 'a -> 'b): 'b list =
list_foldright(xs)([])(fun x0 r0 -> fopr(x0) :: r0)

let build_subsets (xs: 'a) (subsets: 'a list list): 'a list list =
  list_foldright subsets [] (fun subset acc ->
    (xs :: subset) :: subset :: acc
  )

let list_subsets (xs: 'a list): 'a list list = 
list_foldright xs [[]] build_subsets 

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let list_nchoose 
(xs: 'a list) (n0: int): 'a list list =
  filter (fun x -> (length x) = n0) (list_subsets xs)
  