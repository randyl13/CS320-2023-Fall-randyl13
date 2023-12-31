(* ************************************************ *)

(*
Q2-5: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldright. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)
let head = function
  | [] -> []
  | x :: _ -> x

exception Empty
let list_last(xs: 'a list): 'a = 
  match xs with
    | [] -> raise Empty
    | _ -> list_foldright xs (head xs) (fun x acc -> x) 
