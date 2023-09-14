(* ****** ****** *)

#use "./../assign0.ml";;

(* ****** ****** *)

(* ****** Converts a string to int by parsing through the string using
the index and given functions. ****** *)

let str2int(cs: string): int = 
  let rec myloop index acc: int =
    if index = (string_length cs) -1 then acc * 10 + (ord(string_get(cs, index)) - 48)
    else 
      myloop (index+1) ((acc * 10) + (ord(string_get(cs, index)) - 48))
  in
  if cs = "0" then 0
  else
    myloop 0 0
;;
