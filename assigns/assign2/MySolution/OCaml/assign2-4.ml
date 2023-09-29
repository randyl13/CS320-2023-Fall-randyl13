#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let rec length lst =
  match lst with
  | [] -> 0
  | _ :: t -> 1 + length t

let rec combine sep = function
  | [] -> ""
  | h :: t -> if length t = 0 then string_append h (combine sep t) else
    string_append (string_append h sep) (combine sep t)

let
string_sepjoin_list
(sep: string)(xs: string list): string =
combine sep xs
