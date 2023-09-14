(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

(* ****** This function returns a reversed string using the given functions ****** *)

let stringrev(cs: string): string =
  if cs = "" then ""
  else
    string_init (string_length cs) (fun i -> string_get(cs, (string_length cs - i -1)))